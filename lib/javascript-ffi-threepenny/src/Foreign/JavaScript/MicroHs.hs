{-# LANGUAGE RecordWildCards #-}
module Foreign.JavaScript.MicroHs
    ( withBrowserWindow
    ) where

import           Foreign.JavaScript.Types
import qualified Foreign.JavaScript.JSON        as JSON
import qualified Foreign.RemotePtr              as RemotePtr

import Data.IORef
    ( IORef, newIORef, readIORef, writeIORef )
import Data.Text
    ( grabCString, useAsCString, pack, unpack )
import Foreign.C.String
    ( CString )
import Foreign.JavaScript.JSON.Parser
    ( decodeJSON )
import Foreign.StablePtr
    ( StablePtr, newStablePtr )
import System.IO.Unsafe
    ( unsafePerformIO )

foreign import ccall "js_debug"     jsDebug    :: CString -> IO ()
foreign import ccall "js_eval_run"  jsEvalRun  :: CString -> IO ()
foreign import ccall "js_eval_call" jsEvalCall :: CString -> IO CString
foreign import ccall "js_set_haskellCallback"
    jsSetHaskellCallback :: StablePtr (CString -> IO CString) -> IO ()

{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
refWindow :: IORef Window
refWindow = unsafePerformIO (newPartialWindow >>= newIORef)

-- | Run when the browser 'Window' is initialized.
withBrowserWindow :: (Window -> IO ()) -> IO ()
withBrowserWindow action = do
    w0 <- readIORef refWindow
    let wdebug = \s -> useAsCString (pack s) jsDebug
    let w1 = w0
            { debug    = \s -> useAsCString (pack s) jsDebug
            , runEval  = \s -> useAsCString (pack s) jsEvalRun
            , callEval = \s -> do
                -- wdebug s
                t <- useAsCString (pack s) $
                    \p -> jsEvalCall p >>= grabCString
                -- wdebug (show $ decodeJSON t)
                pure $ case decodeJSON t of JSON.Success x -> x
            }
    callback <- newStablePtr haskellCallback
    jsSetHaskellCallback callback
    writeIORef refWindow w1
    action w1

haskellCallback :: CString -> IO CString
haskellCallback cs = grabCString cs >>= \s -> do
    w <- readIORef refWindow
    let JSON.Success x = decodeJSON s
    let (name, args) = case x of
            JSON.Object [("name", JSON.String n), ("args", a)] -> (n, a)
            JSON.Object [("args", a), ("name", JSON.String n)] -> (n, a)
            _ -> error "haskellCallback: cannot parse arguments"
    handleEvent w (name, args)
    useAsCString (pack "") pure

-- | Handle a single event
handleEvent :: Window -> (RemotePtr.Coupon, JSON.Value) -> IO ()
handleEvent Window{debug,wEventHandlers} (name, args) = do
    mhandler <- RemotePtr.lookup name wEventHandlers
    case mhandler of
        Nothing -> pure ()
        Just f  -> do
            RemotePtr.withRemotePtr f (\_ g -> g args)
