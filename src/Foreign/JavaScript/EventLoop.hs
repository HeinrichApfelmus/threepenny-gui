{-# LANGUAGE RecordWildCards, CPP #-}
{-# LANGUAGE RecursiveDo #-}
module Foreign.JavaScript.EventLoop (
    eventLoop,
    runEval, callEval, debug,
    exportHandler, fromJSStablePtr,
    ) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM  as STM
import           Control.Exception               (finally)
import           Control.Monad
import qualified Data.Aeson              as JSON
import           Data.IORef
import qualified Data.Map                as Map
import qualified Data.Text               as T
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
eventLoop init comm = mdo
    -- To support concurrency, we make three threads.
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

    
    w0 <- newPartialWindow
    let runEval s = do
            atomically $ writeTQueue calls (Nothing , RunEval s)
        callEval s = do
            ref <- newEmptyTMVarIO
            atomically $ writeTQueue calls (Just ref, CallEval s)
            atomically $ takeTMVar ref
        debug    s = do
            atomically $ writeServer comm $ Debug s
    let w = w0 { runEval = runEval, callEval = callEval, debug = debug }


    let fork m  = forkFinally m (const killall)

    -- Read client messages and send them to the
    -- thread that handles events or the thread that handles FFI calls.
    multiplexer <- fork $ forever $ do
        atomically $ do
            msg <- readClient comm
            case msg of
                Event x y -> do
                    b <- (||) <$> readTVar handling <*> readTVar calling
                    let c = if b then Inconsistent else Consistent
                    writeTQueue events (x,y,c)
                Quit      -> writeTQueue events quit
                Result x  -> writeTQueue results x

    -- Send FFI calls to client and collect results
    handleCalls <- fork $ forever $ do
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
    -- Also ensure that root is alive
    handleEvents <- myThreadId
    flip finally killall $
        Foreign.withRemotePtr (wRoot w) $ \_ _ -> do
            init w
            forever $ do
                e <- atomically $ do
                    writeTVar handling True
                    readTQueue events
                handleEvent w e
                rebug
                atomically $ writeTVar handling False

    let killall = mapM_ killThread [multiplexer, handleCalls, handleEvents]
    return ()

{-----------------------------------------------------------------------------
    Exports, Imports and garbage collection
------------------------------------------------------------------------------}
-- | Export a Haskell function as an event handler.
--
-- WARNING: The event handler will be garbage collected unless you
-- keep a reference to it /on the Haskell side/!
-- Registering it with a JavaScript function will generally /not/
-- keep it alive.
--
-- Note: The input to the handler is a list of JavaScript arguments.
--
-- FIXME: Support marshalled arguments.
exportHandler :: ([JSON.Value] -> IO ()) -> Window -> IO HsEvent
exportHandler handler w@(Window{..}) = do
    coupon <- newCoupon wEventHandlers
    newRemotePtr coupon (handler . parseArgs) wEventHandlers
    where
    fromSuccess (JSON.Success x) = x
    parseArgs x = Map.elems (fromSuccess (JSON.fromJSON x) :: Map.Map String JSON.Value)


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

