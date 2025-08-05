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
import System.IO.Unsafe
    ( unsafePerformIO )

foreign import javascript "console.log(UTF8ToString($0))"
    ffiDebug :: CString -> IO ()
foreign import javascript "eval(UTF8ToString($0))"
    ffiRunEval :: CString -> IO ()
foreign import javascript "return stringToNewUTF8(JSON.stringify(eval(UTF8ToString($0))))"
    ffiCallEval :: CString -> IO CString

foreign export javascript callbackHs
    :: CString -> CString -> IO ()

{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
refWindow :: IORef Window
refWindow = unsafePerformIO (newPartialWindow >>= newIORef)

-- | Run when the browser 'Window' is initialized.
withBrowserWindow :: (Window -> IO ()) -> IO ()
withBrowserWindow action = do
    w0 <- readIORef refWindow
    let wdebug = \s -> useAsCString (pack s) ffiDebug
    let w1 = w0
            { debug    = \s -> useAsCString (pack s) ffiDebug
            , runEval  = \s -> useAsCString (pack s) ffiRunEval
            , callEval = \s -> do
                t <- useAsCString (pack s) $
                    \p -> ffiCallEval p >>= grabCString
                pure $ JSON.Raw t
            }
    writeIORef refWindow w1
    action w1

-- Module._callbackHs(stringToNewUTF8('hello'))
callbackHs :: CString -> CString -> IO ()
callbackHs cname cargs = do
    name <- grabCString cname
    args <- grabCString cargs
    w <- readIORef refWindow
    handleEvent w (name, JSON.Raw args)

-- | Handle a single event
handleEvent :: Window -> (RemotePtr.Coupon, JSON.Value) -> IO ()
handleEvent Window{debug,wEventHandlers} (name, args) = do
    mhandler <- RemotePtr.lookup name wEventHandlers
    case mhandler of
        Nothing -> pure ()
        Just f  -> do
            RemotePtr.withRemotePtr f (\_ g -> g args)
