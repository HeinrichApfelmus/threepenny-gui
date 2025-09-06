{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server-related functionality.
module Graphics.UI.Threepenny.Server (
    -- $server
    Config(..), ConfigSSL (..), defaultConfig, startGUI,
    loadFile, loadDirectory,
    ) where

import Foreign.JavaScript
    ( Config(..), ConfigSSL (..), defaultConfig )
import Graphics.UI.Threepenny.Internal
    ( loadDirectory, loadFile, startGUI )

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

