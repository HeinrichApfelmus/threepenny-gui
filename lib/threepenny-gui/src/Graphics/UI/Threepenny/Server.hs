{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server-related functionality.
module Graphics.UI.Threepenny.Server (
    -- $server
    Config(..), ConfigSSL (..), defaultConfig, startGUI,
    loadFile, loadDirectory,
    ) where

import qualified Foreign.JavaScript.Server as JS

import Foreign.JavaScript.Server
    ( Config(..), ConfigSSL (..), defaultConfig )
import Graphics.UI.Threepenny.Internal
    ( Window, UI, liftJSWindow, setupWindow )

{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
{- $server

To display the user interface, you have to start a server using 'startGUI'.
Then, visit the URL <http://localhost:8023/> in your browser
(assuming that you use the default server configuration 'defaultConfig',
or have set the port number to @jsPort=Just 8023@.)

The server is multithreaded.
FFI calls can be made concurrently, but events are handled sequentially.

FFI calls can be __buffered__,
so in some circumstances, it may happen that you manipulate the browser window,
but the effect is not immediately visible.
See 'CallBufferMode' for more information.

-}

-- | Start server for GUI sessions.
startGUI
    :: Config               -- ^ Server configuration.
    -> (Window -> UI ())    -- ^ Action to run whenever a client browser connects.
    -> IO ()
startGUI config = JS.serve config . setupWindow

-- | Begin to serve a local file with a given 'MimeType' under a relative URI.
loadFile
    :: String    -- ^ MIME type
    -> FilePath  -- ^ Local path to the file
    -> UI String -- ^ Relative URI under which this file is now accessible
loadFile x y = liftJSWindow $ \w -> JS.loadFile (JS.getServer w) x y

-- | Make a local directory available under a relative URI.
loadDirectory :: FilePath -> UI String
loadDirectory x = liftJSWindow $ \w -> JS.loadDirectory (JS.getServer w) x
