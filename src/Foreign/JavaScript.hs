{-# LANGUAGE RecordWildCards #-}
module Foreign.JavaScript (
    -- * Synopsis
    -- | A JavaScript foreign function interface (FFI).
    --
    -- This module implements a web server that communicates with
    -- a web browser and allows you to execute arbitrary JavaScript code on it.
    --
    -- NOTE: This module is used internally by the "Graphics.UI.Threepenny"
    -- library, but the types are /not/ compatible directly
    -- (although some escape hatches are provided).
    -- Use "Foreign.JavaScript" only if you want to roll your own
    -- interface to the web browser.

    -- * Server
    serve, defaultConfig, Config(
          jsPort, jsAddr
        , jsCustomHTML, jsStatic, jsLog
        , jsWindowReloadOnDisconnect, jsCallBufferMode),
    Server, MimeType, URI, loadFile, loadDirectory,
    Window, getServer, getCookies, root,

    -- * JavaScript FFI
    ToJS(..), FromJS, JSFunction, JSObject, JavaScriptException,
    FFI, ffi, runFunction, callFunction,
    NewJSObject, unsafeCreateJSObject,
    CallBufferMode(..), setCallBufferMode, getCallBufferMode, flushCallBuffer,
    IsHandler, exportHandler, onDisconnect,
    debug, timestamp,
    ) where

import           Control.Concurrent.STM       as STM
import           Control.Monad                           (unless)
import qualified Data.Aeson                   as JSON
import           Foreign.JavaScript.CallBuffer
import           Foreign.JavaScript.EventLoop
import           Foreign.JavaScript.Marshal
import           Foreign.JavaScript.Server
import           Foreign.JavaScript.Types
import           Foreign.RemotePtr            as Foreign

{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
-- | Run a "Foreign.JavaScript" server.
serve
    :: Config               -- ^ Configuration options.
    -> (Window -> IO ())    -- ^ Initialization whenever a client connects.
    -> IO ()
serve config init = httpComm config $ eventLoop $ \w -> do
    setCallBufferMode w (jsCallBufferMode config)
    runFunction w $
        ffi "connection.setReloadOnDisconnect(%1)" $ jsWindowReloadOnDisconnect config
    flushCallBuffer w   -- make sure that all `runEval` commands are executed
    init w
    flushCallBuffer w   -- make sure that all `runEval` commands are executed

{-----------------------------------------------------------------------------
    JavaScript
------------------------------------------------------------------------------}
-- | Run a JavaScript function, but do not wait for a result.
--
-- NOTE: The JavaScript function need not be executed immediately,
-- it can be buffered and sent to the browser window at a later time.
-- See 'setCallBufferMode' and 'flushCallBuffer' for more.
runFunction :: Window -> JSFunction () -> IO ()
runFunction w f = bufferRunEval w =<< toCode f

-- | Run a JavaScript function that creates a new object.
-- Return a corresponding 'JSObject' without waiting for the browser
-- to send a result.
--
-- WARNING: This function assumes that the supplied JavaScript code does,
-- in fact, create an object that is new.
unsafeCreateJSObject :: Window -> JSFunction NewJSObject -> IO JSObject
unsafeCreateJSObject w f = do
    g <- wrapImposeStablePtr w f
    bufferRunEval w =<< toCode g
    marshalResult g w JSON.Null

-- | Call a JavaScript function and wait for the result.
callFunction :: Window -> JSFunction a -> IO a
callFunction w f = do
    -- FIXME: Add the code of f to the buffer as well!
    -- However, we have to pay attention to the semantics of exceptions in this case.
    flushCallBuffer w

    resultJS <- callEval w =<< toCode f
    marshalResult f w resultJS

-- | Export a Haskell function as an event handler.
--
-- The result is a JavaScript @Function@ object that can be called
-- from JavaScript like a regular function. However,
-- the corresponding Haskell function will /not/ be run immediately,
-- rather it will be added to the event queue and processed
-- like an event. In other words, this the Haskell code is only called
-- asynchronously.
--
-- WARNING: The event handler will be garbage collected unless you
-- keep a reference to it /on the Haskell side/!
-- Registering it with a JavaScript function will generally /not/
-- keep it alive.
exportHandler :: IsHandler a => Window -> a -> IO JSObject
exportHandler w f = do
    g <- newHandler w (\args -> handle f w args >> flushCallBuffer w)
    h <- unsafeCreateJSObject w $
        ffi "Haskell.newEvent(%1,%2)" g (convertArguments f)
    Foreign.addReachable h g
    return h
