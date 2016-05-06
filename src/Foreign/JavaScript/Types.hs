{-# LANGUAGE OverloadedStrings #-}
module Foreign.JavaScript.Types where

import           Control.Applicative
import           Control.Concurrent.STM  as STM
import           Control.Concurrent.Chan as Chan
import           Control.Concurrent.MVar
import           Control.DeepSeq
import           Data.Aeson              as JSON
import           Data.ByteString.Char8           (ByteString)
import qualified Data.ByteString.Char8   as BS   (hPutStrLn)
import           Data.IORef
import           Data.Map                as Map
import           Data.String
import           Data.Text
import           System.IO                       (stderr)

import Foreign.RemotePtr

{-----------------------------------------------------------------------------
    Server Configuration
------------------------------------------------------------------------------}
-- | Configuration of a "Foreign.JavaScript" server.
data Config = Config
    { jsPort       :: Maybe Int           
        -- ^ Port number.
        -- @Nothing@ means that the port number is
        -- read from the environment variable @PORT@.
        -- Alternatively, port @8023@ is used if this variable is not set.
    , jsAddr       :: Maybe ByteString
        -- ^ Bind address.
        -- @Nothing@ means that the bind address is
        -- read from the environment variable @ADDR@.
        -- Alternatively, address @127.0.0.1@ is
        -- used if this variable is not set.
    , jsCustomHTML :: Maybe FilePath
        -- ^ Custom HTML file to replace the default one.
    , jsStatic     :: Maybe FilePath
        -- ^ Directory that is served under @/static@.
    , jsLog        :: ByteString -> IO ()
        -- ^ Print a single log message.
    }

defaultPort :: Int
defaultPort = 8023

defaultAddr :: ByteString
defaultAddr = "127.0.0.1"

-- | Default configuration.
--
-- Port from environment variable or @8023@,
-- listening on @localhost@, no custom HTML, no static directory,
-- logging to stderr.
defaultConfig :: Config
defaultConfig = Config
    { jsPort       = Nothing
    , jsAddr       = Nothing
    , jsCustomHTML = Nothing
    , jsStatic     = Nothing
    , jsLog        = BS.hPutStrLn stderr
    }

{-----------------------------------------------------------------------------
    Communication channel
------------------------------------------------------------------------------}
-- | Bidirectional communication channel.
data Comm = Comm
    { commIn    :: TQueue JSON.Value
    , commOut   :: TQueue JSON.Value
    , commClose :: IO ()
    }

writeComm :: Comm -> JSON.Value -> STM ()
writeComm c = STM.writeTQueue (commOut c)

readComm :: Comm -> STM JSON.Value
readComm c = STM.readTQueue (commIn c)

{-----------------------------------------------------------------------------
    Communication protocol
------------------------------------------------------------------------------}
-- | Messages received from the JavaScript client.
data ClientMsg
    = Event Coupon JSON.Value
    | Result JSON.Value
    | Quit
    deriving (Eq, Show)

instance FromJSON ClientMsg where
    parseJSON (Object msg) = do
        tag <- msg .: "tag"
        case (tag :: Text) of
            "Event" ->
                Event  <$> (msg .: "name") <*> (msg .: "arguments")
            "Result" ->
                Result <$> (msg .: "contents")
            "Quit"   ->
                return Quit

readClient :: Comm -> STM ClientMsg
readClient c = do
    msg <- readComm c
    case JSON.fromJSON msg of
        Error   s -> error $ "Foreign.JavaScript: Error parsing client message " ++ show s
        Success x -> return x

-- | Messages sent by the Haskell server.
data ServerMsg
    = RunEval  String
    | CallEval String
    | Debug    String
    | Timestamp
    deriving (Eq,Show)

instance NFData ServerMsg where
    rnf (RunEval   x) = rnf x
    rnf (CallEval  x) = rnf x
    rnf (Debug     x) = rnf x
    rnf (Timestamp  ) = ()

instance ToJSON ServerMsg where
    toJSON (Debug    x) = object [ "tag" .= t "Debug"   , "contents" .= toJSON x]
    toJSON (Timestamp ) = object [ "tag" .= t "Timestamp" ]
    toJSON (RunEval  x) = object [ "tag" .= t "RunEval" , "contents" .= toJSON x]
    toJSON (CallEval x) = object [ "tag" .= t "CallEval", "contents" .= toJSON x]

t s = fromString s :: Text

writeServer :: Comm -> ServerMsg -> STM ()
writeServer c = writeComm c . toJSON . force

{- Note [ServerMsg strictness]

The type `ServerMsg` may contain components that evalute to _|_, and
an exception will be thrown when we try to send one of those to the browser.

However, we have to make sure that the exception is thrown
in the thread that constructed the message, not in the thread that
handles the actual communication with the client. That's why we use
the function `Control.DeepSeq.force` to make sure that any exception
is thrown before handing the message over to another thread.

-}

{-----------------------------------------------------------------------------
    Window & Event Loop
------------------------------------------------------------------------------}
data Consistency  = Consistent | Inconsistent
type Event        = (Coupon, JSON.Value, Consistency)

-- | An event handler that can be passed to the JavaScript client.
type HsEvent      = RemotePtr (JSON.Value -> IO ())

quit :: Event
quit = ("quit", JSON.Null, Consistent)

-- | Representation of a browser window.
data Window = Window
    { runEval        :: String -> IO ()
    , callEval       :: String -> IO JSON.Value
    , timestamp      :: IO ()
    -- ^ Print a timestamp and the time difference to the previous one
    -- in the JavaScript console.
    , debug          :: String -> IO ()
    -- ^ Send a debug message to the JavaScript console.
    , onDisconnect   :: IO () -> IO ()
    -- ^ Register an action to be performed when the client disconnects.
    , wRoot          :: RemotePtr ()
    , wEventHandlers :: Vendor (JSON.Value -> IO ())
    , wJSObjects     :: Vendor JSPtr
    }

newPartialWindow :: IO Window
newPartialWindow = do
    ptr <- newRemotePtr "" () =<< newVendor
    let nop = const $ return ()
    Window nop undefined (return ()) nop nop ptr <$> newVendor <*> newVendor

-- | For the purpose of controlling garbage collection,
-- every 'Window' as an associated 'RemotePtr' that is alive
-- as long as the external JavaScript connection is alive.
root :: Window -> RemotePtr ()
root = wRoot

{-----------------------------------------------------------------------------
    Marshalling
------------------------------------------------------------------------------}
newtype JSPtr = JSPtr { unsJSPtr :: Coupon }

-- | A mutable JavaScript object.
type JSObject = RemotePtr JSPtr

-- | A mutable JavaScript object that has just been created.
-- This a dummy type used for additional type safety.
data NewJSObject = NewJSObject
