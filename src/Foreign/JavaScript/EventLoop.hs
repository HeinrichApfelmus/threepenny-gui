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
import           Control.Exception        as E    (finally)
import           Control.Monad
import qualified Data.Aeson               as JSON
import           Data.IORef
import qualified Data.Map                 as Map
import qualified Data.Text                as T
import qualified System.Mem

import Foreign.RemotePtr        as Foreign
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
handleEvent w@(Window{..}) (name, args, consistency) = do
    mhandler <- Foreign.lookup name wEventHandlers
    case mhandler of
        Nothing -> return ()
        Just f  -> withRemotePtr f (\_ f -> f args)


-- | Event loop for a browser window.
-- Supports concurrent invocations of `runEval` and `callEval`.
eventLoop :: (Window -> IO void) -> (Comm -> IO ())
eventLoop init comm = do
    -- To support concurrent FFI calls, we make three threads.
    -- The thread `multiplexer` reads from the client and 
    --   sorts the messages into the appropriate queue.
    events      <- newTQueueIO
    results     <- newTQueueIO :: IO (TQueue JSON.Value)
    -- The thread `handleCalls` executes FFI calls
    --    from the Haskell side in order.
    -- The corresponding queue records `TMVar`s in which to put the results.
    calls       <- newTQueueIO :: IO (TQueue (Maybe (TMVar JSON.Value), ServerMsg))
    -- The thread `handleEvents` handles client Events in order.

    -- Events will be queued (and labelled `Inconsistent`) whenever
    -- the server is
    --    * busy handling an event
    --    * or waiting for the result of a function call.
    handling    <- newTVarIO False
    calling     <- newTVarIO False

    -- FFI calls are made by writing to the `calls` queue.
    w0 <- newPartialWindow
    let run msg = do
            atomically $ writeTQueue calls (Nothing , msg)
        call msg = do
            ref <- newEmptyTMVarIO
            atomically $ writeTQueue calls (Just ref, msg)
            atomically $ takeTMVar ref
        debug    s = do
            atomically $ writeServer comm $ Debug s

    -- We also send a separate event when the client disconnects.
    disconnect <- newTVarIO $ return ()
    let onDisconnect m = atomically $ writeTVar disconnect m
    
    let w = w0 { runEval        = run  . RunEval
               , callEval       = call . CallEval
               , debug        = debug
               , timestamp    = run Timestamp
               , onDisconnect = onDisconnect
               }

    -- The individual threads are as follows:
    --
    -- Read client messages and send them to the
    -- thread that handles events or the thread that handles FFI calls.
    let multiplexer = do
            m <- untilJustM $ atomically $ do
                msg <- readClient comm
                case msg of
                    Event x y -> do
                        b <- (||) <$> readTVar handling <*> readTVar calling
                        let c = if b then Inconsistent else Consistent
                        writeTQueue events (x,y,c)
                        return Nothing
                    Result x  -> do
                        writeTQueue results x
                        return Nothing
                    Quit      -> Just <$> readTVar disconnect
            m
        
    -- Send FFI calls to client and collect results
    let handleCalls = forever $ do
            ref <- atomically $ do
                (ref, msg) <- readTQueue calls
                writeTVar calling True
                writeServer comm msg
                return ref
            atomically $ do
                writeTVar calling False
                case ref of
                    Just ref -> do
                        result <- readTQueue results
                        putTMVar ref result
                    Nothing  -> return ()
    
    -- Receive events from client and handle them in order.
    let handleEvents = do
            init w
            forever $ do
                e <- atomically $ do
                    writeTVar handling True
                    readTQueue events
                handleEvent w e
                rebug
                atomically $ writeTVar handling False

    -- Foreign.addFinalizer (wRoot w) $ putStrLn "wRoot garbage collected."
    Foreign.withRemotePtr (wRoot w) $ \_ _ -> do    -- keep root alive
        E.finally
            (foldr1 race_ [multiplexer, handleEvents, handleCalls])
            (commClose comm)

    return ()

-- | Repeat an action until it returns 'Just'. Similar to 'forever'.
untilJustM :: Monad m => m (Maybe a) -> m a
untilJustM m = m >>= \x -> case x of
    Nothing -> untilJustM m
    Just a  -> return a

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

