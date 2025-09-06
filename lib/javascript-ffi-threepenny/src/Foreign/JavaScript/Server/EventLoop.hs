{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
module Foreign.JavaScript.Server.EventLoop
    ( eventLoop
    , runEval, callEval, debug, onDisconnect
    ) where

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM   as STM
import           Control.DeepSeq                  (deepseq)
import           Control.Exception        as E
import           Control.Monad
import qualified Data.Aeson               as JSON
import qualified Data.ByteString.Char8    as BS
import qualified Data.Text                as T

import Foreign.RemotePtr             as Foreign
import Foreign.JavaScript.CallBuffer
import Foreign.JavaScript.Types

rebug :: IO ()
#ifdef REBUG
rebug = System.Mem.performGC
#else
rebug = pure ()
#endif

{-----------------------------------------------------------------------------
    Event Loop
------------------------------------------------------------------------------}
-- | Handle a single event
handleEvent :: Window -> (Coupon, JSON.Value) -> IO ()
handleEvent Window{wEventHandlers} (name, args) = do
    mhandler <- Foreign.lookup name wEventHandlers
    case mhandler of
        Nothing -> pure ()
        Just f  -> withRemotePtr f (\_ g -> g args)


type Result = Either String JSON.Value

-- | Event loop for a browser window.
-- Supports concurrent invocations of `runEval` and `callEval`.
eventLoop :: (Window -> IO void) -> EventLoop
eventLoop initialize server info comm = void $ do
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
                if b then fmap Right stm else pure (Left ())
            case r of
                Right a -> pure a
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
                Right x -> pure x
        debug s  = s `deepseq` do       -- see [ServerMsg strictness]
            atomicallyIfOpen $ writeServer comm $ Debug s

    -- We also send a separate event when the client disconnects.
    disconnect <- newTVarIO $ pure ()
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
            mref <- atomically $ do
                (mref, msg) <- readTQueue calls
                writeServer comm msg
                pure mref
            atomically $
                case mref of
                    Just ref -> do
                        result <- readTQueue results
                        putTMVar ref result
                    Nothing  -> pure ()

    -- Receive events from client and handle them in order.
    let handleEvents = do
            me <- atomically $ do
                open <- readTVar $ commOpen comm
                if open
                    then Just <$> readTQueue events
                    else pure Nothing -- channel is closed
            case me of
                Nothing -> pure ()    -- channel is closed, we're done
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
        E.finally (initialize w >> handleEvents) $ do
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
