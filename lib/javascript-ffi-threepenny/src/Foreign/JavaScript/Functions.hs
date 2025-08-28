{-# LANGUAGE CPP #-}

-- | Given a 'Window', call JavaScript functions from Haskell and vice-versa.
--
-- The main functions are 'ffi', 'runFunction', and 'callFunction'.
module Foreign.JavaScript.Functions
    ( -- * Types
      ToJS (..), FromJS, JSFunction, JSObject
#if !defined(__MHS__)
    , JavaScriptException
#endif
    -- * Calling JavaScript from Haskell
    , FFI, ffi, runFunction, callFunction
    , NewJSObject, unsafeCreateJSObject
    , CallBufferMode(..), setCallBufferMode, getCallBufferMode, flushCallBuffer
    -- * Calling Haskell from JavaScript
    , IsHandler, exportHandler, onDisconnect
    -- * Debugging
    , debug, timestamp
    ) where

import Foreign.JavaScript.CallHaskell
import Foreign.JavaScript.CallBuffer
import Foreign.JavaScript.Marshal
import Foreign.JavaScript.Types
import Foreign.RemotePtr as RemotePtr

{-----------------------------------------------------------------------------
    JavaScript Functions
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
    marshalResult g w err
  where
    err = error "unsafeCreateJSObject: marshal does not take arguments"

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
    RemotePtr.addReachable h g
    pure h
