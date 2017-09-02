{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Foreign.JavaScript.Server (
    httpComm, loadFile, loadDirectory,
    ) where

-- import general libraries
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM     as STM
import qualified Control.Exception          as E
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Map                   as M
import           Data.Text
import qualified Safe                       as Safe
import           System.Environment
import           System.FilePath

-- import web libraries
import           Data.Aeson                             ((.=))
import qualified Data.Aeson                    as JSON
import qualified Network.WebSockets            as WS
import qualified Network.WebSockets.Snap       as WS
import           Snap.Core                     as Snap
import qualified Snap.Http.Server              as Snap
import           Snap.Util.FileServe

-- import internal modules
import Foreign.JavaScript.Resources
import Foreign.JavaScript.Types

{-----------------------------------------------------------------------------
    HTTP Server using WebSockets
------------------------------------------------------------------------------}
-- | Run a HTTP server that creates a 'Comm' channel.
httpComm :: Config -> EventLoop -> IO ()
httpComm Config{..} worker = do
    env <- getEnvironment
    let portEnv = Safe.readMay =<< Prelude.lookup "PORT" env
    let addrEnv = fmap BS.pack $ Prelude.lookup "ADDR" env
    
    let config = Snap.setPort      (maybe defaultPort id (jsPort `mplus` portEnv))
               $ Snap.setBind      (maybe defaultAddr id (jsAddr `mplus` addrEnv))
               $ Snap.setErrorLog  (Snap.ConfigIoLog jsLog)
               $ Snap.setAccessLog (Snap.ConfigIoLog jsLog)
               $ Snap.defaultConfig

    server <- Server <$> newMVar newFilepaths <*> newMVar newFilepaths <*> return jsLog

    Snap.httpServe config . route $
        routeResources server jsCustomHTML jsStatic
        ++ routeWebsockets (worker server)

-- | Route the communication between JavaScript and the server
routeWebsockets :: (RequestInfo -> Comm -> IO void) -> Routes
routeWebsockets worker = [("websocket", response)]
    where
    response = do
        requestInfo <- Snap.getRequest
        WS.runWebSocketsSnap $ \ws -> void $ do
            comm <- communicationFromWebSocket ws
            worker (rqCookies requestInfo) comm
            -- error "Foreign.JavaScript: unreachable code path."

-- | Create 'Comm' channel from WebSocket request.
communicationFromWebSocket :: WS.PendingConnection -> IO Comm
communicationFromWebSocket request = do
    connection <- WS.acceptRequest request
    commIn     <- STM.newTQueueIO   -- outgoing communication
    commOut    <- STM.newTQueueIO   -- incoming communication
    commOpen   <- STM.newTVarIO True

    -- write data to browser
    let sendData = forever $ do
            x <- atomically $ STM.readTQueue commOut
            -- see note [ServerMsg strictness]
            WS.sendTextData connection . JSON.encode $ x

    -- read data from browser
    let readData = forever $ do
            input <- WS.receiveData connection
            case input of
                "ping" -> WS.sendTextData connection . LBS.pack $ "pong"
                "quit" -> E.throwIO WS.ConnectionClosed
                input  -> case JSON.decode input of
                    Just x   -> atomically $ STM.writeTQueue commIn x
                    Nothing  -> error $
                        "Foreign.JavaScript: Couldn't parse JSON input"
                        ++ show input

    -- block until the channel is closed
    let sentry = atomically $ do
            open <- STM.readTVar commOpen
            when open retry

    -- explicitly close the Comm chanenl
    let commClose = atomically $ STM.writeTVar commOpen False

    -- read/write data until an exception occurs or the channel is no longer open
    forkFinally (sendData `race_` readData `race_` sentry) $ \_ -> void $ do
        -- close the communication channel explicitly if that didn't happen yet
        commClose

        -- attempt to close websocket if still necessary/possible
        -- ignore any exceptions that may happen if it's already closed
        let all :: E.SomeException -> Maybe ()
            all _ = Just ()
        E.tryJust all $ WS.sendClose connection $ LBS.pack "close"

    return $ Comm {..}

{-----------------------------------------------------------------------------
    Resources
------------------------------------------------------------------------------}
type Routes = [(ByteString, Snap ())]

routeResources :: Server -> Maybe FilePath -> Maybe FilePath -> Routes
routeResources server customHTML staticDir =
    fixHandlers noCache $
        static ++
        [("/"            , root)
        ,("/haskell.js"  , writeTextMime jsDriverCode  "application/javascript")
        ,("/haskell.css" , writeTextMime cssDriverCode "text/css")
        ,("/file/:name"                ,
            withFilepath (sFiles server) (flip serveFileAs))
        ,("/dir/:name"                 ,
            withFilepath (sDirs  server) (\path _ -> serveDirectory path))
        ]
    where
    fixHandlers f routes = [(a,f b) | (a,b) <- routes]
    noCache h = modifyResponse (setHeader "Cache-Control" "no-cache") >> h

    static = maybe [] (\dir -> [("/static", serveDirectory dir)]) staticDir

    root = case customHTML of
        Just file -> case staticDir of
            Just dir -> serveFile (dir </> file)
            Nothing  -> logError "Foreign.JavaScript: Cannot use jsCustomHTML file without jsStatic"
        Nothing   -> writeTextMime defaultHtmlFile "text/html"

writeTextMime text mime = do
    modifyResponse (setHeader "Content-type" mime)
    writeText text

-- | Extract  from a URI
withFilepath :: MVar Filepaths -> (FilePath -> ByteString -> Snap a) -> Snap a
withFilepath rDict cont = do
    mName    <- getParam "name"
    (_,dict) <- liftIO $ withMVar rDict return
    case (\key -> M.lookup key dict) =<< mName of
        Just (path,mimetype) -> cont path (BS.pack mimetype)
        Nothing              -> error $ "File not loaded: " ++ show mName

-- FIXME: Serving large files fails with the exception
-- System.SendFile.Darwin: invalid argument (Socket is not connected)

-- | Associate an URL to a FilePath
newAssociation :: MVar Filepaths -> (FilePath, MimeType) -> IO String
newAssociation rDict (path,mimetype) = do
    (old, dict) <- takeMVar rDict
    let new = old + 1; key = show new ++ takeFileName path
    putMVar rDict $ (new, M.insert (BS.pack key) (path,mimetype) dict)
    return key

-- | Begin to serve a local file with a given 'MimeType' under a URI.
loadFile :: Server -> MimeType -> FilePath -> IO String
loadFile server mimetype path = do
    key <- newAssociation (sFiles server) (path, mimetype)
    return $ "/file/" ++ key

-- | Begin to serve a local directory under a URI.
loadDirectory :: Server -> FilePath -> IO String
loadDirectory server path = do
    key <- newAssociation (sDirs server) (path,"")
    return $ "/dir/" ++ key

