{-# LANGUAGE RecordWildCards, CPP #-}
{-# LANGUAGE RecursiveDo #-}
module Foreign.JavaScript.EventLoop (
    eventLoop,
    runEval, callEval, debug, onDisconnect,
    newHandler, fromJSStablePtr,
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM   as STM
import           Control.DeepSeq                  (deepseq)
import           Control.Exception        as E
import           Control.Monad
import qualified Data.Aeson               as JSON
import qualified Data.ByteString.Char8    as BS
import           Data.IORef
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified System.Mem

import Foreign.RemotePtr             as Foreign
import Foreign.JavaScript.CallBuffer
import Foreign.JavaScript.Types

rebug :: IO ()
#ifdef REBUG
rebug = System.Mem.performGC
#else
rebug = return ()
#endif

{-----------------------------------------------------------------------------
    Event Loop
------------------------------------------------------------------------------}
-- | Handle a single event
handleEvent w@(Window{..}) (name, args) = do
    mhandler <- Foreign.lookup name wEventHandlers
    case mhandler of
        Nothing -> return ()
        Just f  -> withRemotePtr f (\_ f -> f args)


type Result = Either String JSON.Value

-- | Event loop for a browser window.
-- Supports concurrent invocations of `runEval` and `callEval`.
eventLoop :: (Window -> IO void) -> EventLoop
eventLoop init server info comm = void $ do
    -- To support concurrent FFI calls, we need three threads.
    -- A fourth thread supports 
    --
    -- The thread `multiplexer` reads from the client and
    --   sorts the messages into the appropriate queue.
    events      <- newTQueueIO
    results     <- newTQueueIO :: IO (TQueue Result)
    -- The thread `handleCalls` executes FFI calls
    --    from the Haskell side in order.
    -- The corresponding queue records `TMVar`s in which to put the results.
    calls       <- newTQueueIO :: IO (TQueue (Maybe (TMVar Result), ServerMsg))
    -- The thread `handleEvents` handles client Events in order.

    -- We only want to make an FFI call when the connection browser<->server is open
    -- Otherwise, throw an exception.
    let atomicallyIfOpen stm = do
            r <- atomically $ do
                b <- readTVar $ commOpen comm
                if b then fmap Right stm else return (Left ())
            case r of
                Right a -> return a
                Left  _ -> throwIO $ ErrorCall "Foreign.JavaScript: Browser <-> Server communication broken."

    -- FFI calls are made by writing to the `calls` queue.
    let run  msg = msg `deepseq` do     -- see [ServerMsg strictness]
            atomicallyIfOpen $ writeTQueue calls (Nothing , msg)
        call msg = msg `deepseq` do     -- see [ServerMsg strictness]
            ref <- newEmptyTMVarIO
            atomicallyIfOpen $ writeTQueue calls (Just ref, msg)
            er  <- atomicallyIfOpen $ takeTMVar ref
            case er of
                Left  e -> E.throwIO $ JavaScriptException e
                Right x -> return x
        debug s  = s `deepseq` do       -- see [ServerMsg strictness]
            atomicallyIfOpen $ writeServer comm $ Debug s

    -- We also send a separate event when the client disconnects.
    disconnect <- newTVarIO $ return ()
    -- FIXME: Make it possible to store *multiple* event handlers
    let onDisconnect m = atomically $ writeTVar disconnect m

    w0 <- newPartialWindow
    let w = w0 { getServer    = server
               , getCookies   = info
               , runEval      = run  . RunEval
               , callEval     = call . CallEval
               , debug        = debug
               , timestamp    = run Timestamp
               , onDisconnect = onDisconnect
               }

    -- The individual threads are as follows:
    --
    -- Read client messages and send them to the
    -- thread that handles events or the thread that handles FFI calls.
    let multiplexer = forever $ atomically $ do
            msg <- readClient comm
            case msg of
                Event x y   -> writeTQueue events (x,y)
                Result x    -> writeTQueue results (Right x)
                Exception e -> writeTQueue results (Left  e)

    -- Send FFI calls to client and collect results
    let handleCalls = forever $ do
            ref <- atomically $ do
                (ref, msg) <- readTQueue calls
                writeServer comm msg
                return ref
            atomically $
                case ref of
                    Just ref -> do
                        result <- readTQueue results
                        putTMVar ref result
                    Nothing  -> return ()

    -- Receive events from client and handle them in order.
    let handleEvents = do
            me <- atomically $ do
                open <- readTVar $ commOpen comm
                if open
                    then Just <$> readTQueue events
                    else return Nothing -- channel is closed
            case me of
                Nothing -> return ()    -- channel is closed, we're done
                Just e  -> do
                    handleEvent w e
                        `E.onException` commClose comm -- close channel in case of exception
                    rebug
                    handleEvents

    -- Execute an IO action, but also print any exceptions that it may throw.
    -- (The exception is rethrown.)
    let
        printException :: IO a -> IO a
        printException = E.handle $ \e -> do
            sLog server . BS.pack $ show (e :: E.SomeException)
            E.throwIO e

    -- NOTE: Due to an issue with `snap-server` library,
    -- we print the exception ourselves.
    printException $
        -- Wrap the main loop into `withRemotePtr` in order to keep the root alive.
        Foreign.withRemotePtr (wRoot w) $ \_ _ ->
        -- run `multiplexer` and `handleCalls` concurrently
        withAsync multiplexer $ \_ ->
        withAsync handleCalls $ \_ ->
        withAsync (flushCallBufferPeriodically w) $ \_ ->
        E.finally (init w >> handleEvents) $ do
            putStrLn "Foreign.JavaScript: Browser window disconnected."
            -- close communication channel if still necessary
            commClose comm
            -- trigger the `disconnect` event
            -- FIXME: Asynchronous exceptions should not be masked during the disconnect handler
            m <- atomically $ readTVar disconnect
            m

-- | Thread that periodically flushes the call buffer
flushCallBufferPeriodically :: Window -> IO ()
flushCallBufferPeriodically w =
    forever $ threadDelay (flushPeriod*1000) >> flushCallBuffer w


{-----------------------------------------------------------------------------
    Exports, Imports and garbage collection
------------------------------------------------------------------------------}
-- | Turn a Haskell function into an event handler.
newHandler :: Window -> ([JSON.Value] -> IO ()) -> IO HsEvent
newHandler w@(Window{..}) handler = do
    coupon <- newCoupon wEventHandlers
    newRemotePtr coupon (handler . parseArgs) wEventHandlers
    where
    fromSuccess (JSON.Success x) = x
    -- parse a genuine JavaScript array
    parseArgs x = fromSuccess (JSON.fromJSON x) :: [JSON.Value]
    -- parse a JavaScript arguments object
    -- parseArgs x = Map.elems (fromSuccess (JSON.fromJSON x) :: Map.Map String JSON.Value)


-- | Convert a stable pointer from JavaScript into a 'JSObject'.
fromJSStablePtr :: JSON.Value -> Window -> IO JSObject
fromJSStablePtr js w@(Window{..}) = do
    let JSON.Success coupon = JSON.fromJSON js
    mhs <- Foreign.lookup coupon wJSObjects
    case mhs of
        Just hs -> return hs
        Nothing -> do
            ptr <- newRemotePtr coupon (JSPtr coupon) wJSObjects
            addFinalizer ptr $
                runEval ("Haskell.freeStablePtr('" ++ T.unpack coupon ++ "')")
            return ptr

