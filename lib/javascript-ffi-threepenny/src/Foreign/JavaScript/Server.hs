-- | Start a web server that provides a JavaScript 'Window'.
--
-- Whenever a web browser loads the starting page of the server,
-- the initialization argument of 'serve' is called.
module Foreign.JavaScript.Server
    ( serve, defaultConfig, Config(
          jsPort, jsAddr
        , jsCustomHTML, jsStatic, jsLog
        , jsWindowReloadOnDisconnect, jsCallBufferMode
        , jsUseSSL)
    , ConfigSSL (..)
    , Server, MimeType, URI, loadFile, loadDirectory
    , getServer, getCookies, root
    ) where

import Foreign.JavaScript.CallHaskell
import Foreign.JavaScript.CallBuffer
import Foreign.JavaScript.Functions
import Foreign.JavaScript.Marshal
import Foreign.JavaScript.Server.EventLoop
import Foreign.JavaScript.Server.HTTP
import Foreign.JavaScript.Types

-- | Run a "Foreign.JavaScript" server.
serve
    :: Config               -- ^ Configuration options.
    -> (Window -> IO ())    -- ^ Initialization whenever a client connects.
    -> IO ()
serve config initialize = httpComm config $ eventLoop $ \w -> do
    setCallBufferMode w (jsCallBufferMode config)
    runFunction w $
        ffi "connection.setReloadOnDisconnect(%1)" $ jsWindowReloadOnDisconnect config
    flushCallBuffer w   -- make sure that all `runEval` commands are executed
    initialize w
    flushCallBuffer w   -- make sure that all `runEval` commands are executed
