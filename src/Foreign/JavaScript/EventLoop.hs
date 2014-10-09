{-# LANGUAGE RecordWildCards, CPP #-}
module Foreign.JavaScript.EventLoop (
    eventLoop,
    runEval, callEval, debug,
    exportHandler, fromJSStablePtr,
    ) where

import qualified Control.Concurrent.Chan as Chan
import           Control.Monad
import qualified Data.Aeson              as JSON
import           Data.IORef
import qualified Data.Map                as Map
import qualified Data.Text               as T
import qualified System.Mem

import Foreign.RemotePtr        as Foreign
import Foreign.JavaScript.Types

{-----------------------------------------------------------------------------
    Event Loop
------------------------------------------------------------------------------}
-- | Event loop.
-- 
-- TODO: For the moment, we assume that the event loop is *single-threaded*.
eventLoop :: (Window -> IO void) -> (Comm -> IO ())
eventLoop init c = do
    w <- newWindow c
    Foreign.withRemotePtr (wRoot w) $ \_ _ -> do -- ensure that root is alive
        init w
        forever $ do
            e <- readEvent w
            handleEvent w e
#ifdef REBUG
            -- debug garbage collection of elements:
            System.Mem.performGC
#endif

handleEvent w@(Window{..}) (name, args, consistency) = do
    mhandler <- Foreign.lookup name wEventHandlers
    case mhandler of
        Nothing -> return ()
        Just f  -> withRemotePtr f (\_ f -> f args)

-- | Read an expected @Result@ from the client.
-- If we get an event instead, queue it and label it "potentially inconsistent".
readResult :: Window -> IO JSON.Value
readResult w@(Window{..}) = do
    c <- readClient wComm
    case c of
        Result x  -> return x
        Event x y -> do
            modifyIORef wEventQueue (++ [(x, y, Inconsistent)])
            readResult w
        Quit      -> do
            writeIORef wEventQueue [quit]
            return $ error $ "Foreign.JavaScript: Client has Quit!"

-- | Read an event from the queue or the client.
readEvent :: Window -> IO Event
readEvent w@(Window{..}) = do
    es <- readIORef wEventQueue
    case es of
        []   -> do
            msg <- readClient wComm
            case msg of
                Event x y -> return (x,y,Consistent)
                Quit      -> return quit
        e:_  -> do
            modifyIORef wEventQueue tail
            return e

{-----------------------------------------------------------------------------
    Calling JavaScript functions
------------------------------------------------------------------------------}
write :: Window -> ServerMsg -> IO ()
write Window{..} = writeServer wComm

-- | Run a JavaScript expression.
runEval :: String -> Window -> IO ()
runEval s w = write w $ RunEval s

-- | Run a JavaScript expression and wait for result.
callEval :: String -> Window -> IO JSON.Value
callEval s w = do
    write w $ CallEval s
    x <- readResult w
    return x

-- | Send a debug message to the client.
debug :: String -> Window -> IO ()
debug s w = write w $ Debug s

{-----------------------------------------------------------------------------
    Exports, Imports and garbage collection
------------------------------------------------------------------------------}
-- | Export a Haskell function as an event handler.
--
-- WARNING: The event handler will be garbage collected unless you
-- keep a reference to it /on the Haskell side/!
-- Registering it with a JavaScript function will generally /not/
-- keep it alive.
exportHandler :: (JSON.Value -> IO ()) -> Window -> IO HsEvent
exportHandler handler w@(Window{..}) = do
    coupon <- newCoupon wEventHandlers
    newRemotePtr coupon handler wEventHandlers

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
                runEval ("Haskell.freeStablePtr('" ++ T.unpack coupon ++ "')") w
            return ptr

