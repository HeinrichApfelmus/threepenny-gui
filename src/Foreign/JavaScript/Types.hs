{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Foreign.JavaScript.Types where

import           Control.Applicative
import qualified Control.Exception       as E
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
import           Data.Typeable
import           Snap.Core                       (Cookie(..))
import           System.IO                       (stderr)

import Foreign.RemotePtr

{-----------------------------------------------------------------------------
    Server Configuration -- Static
------------------------------------------------------------------------------}
-- NOTE: Unfortunately, Haddock currently does not create documentation for
-- record fields when the constructor is not exported.
-- That's why we copy & paste it in the documentation for the data type.
{- | Static configuration for a "Foreign.JavaScript" server.

This is a record type which has the following fields:

* @jsPort :: Maybe Int@          

    Port number.
    @Nothing@ means that the port number is read from the environment variable @PORT@.
    Alternatively, port @8023@ is used if this variable is not set.

* @jsAddr :: Maybe ByteString@

    Bind address.
    @Nothing@ means that the bind address is read from the environment variable @ADDR@.
    Alternatively, address @127.0.0.1@ is used if this variable is not set.

* @jsCustomHTML :: Maybe FilePath@

    Custom HTML file to replace the default one.

* @jsStatic :: Maybe FilePath@

    Directory that is served under @/static@.

* @jsLog :: ByteString -> IO ()@

    Function to print a single log message.

* @jsWindowReloadOnDisconnect :: Bool@

    Reload the browser window if the connection to the server was dropped accidentally,
    for instance because the computer was put to sleep and awoken again.

* @jsCallBufferMode :: CallBufferMode@

    The initial 'CallBufferMode' to use for 'runFunction'.
    It can be changed at any time with 'setCallBufferMode'.

(For reasons of forward compatibility, the constructor is not exported.)

-}
data Config = Config
    { jsPort       :: Maybe Int           
    , jsAddr       :: Maybe ByteString
    , jsCustomHTML :: Maybe FilePath
    , jsStatic     :: Maybe FilePath
    , jsLog        :: ByteString -> IO ()
    , jsWindowReloadOnDisconnect :: Bool
    , jsCallBufferMode :: CallBufferMode
    }

defaultPort :: Int
defaultPort = 8023

defaultAddr :: ByteString
defaultAddr = "127.0.0.1"

-- | Default configuration.
--
-- Port from environment variable or @8023@, listening on @localhost@,
-- no custom HTML, no static directory,
-- logging to stderr,
-- do reload on disconnect,
-- __buffer FFI calls__.
defaultConfig :: Config
defaultConfig = Config
    { jsPort       = Nothing
    , jsAddr       = Nothing
    , jsWindowReloadOnDisconnect = True
    , jsCustomHTML = Nothing
    , jsStatic     = Nothing
    , jsLog        = BS.hPutStrLn stderr
    , jsCallBufferMode = FlushOften
    }

{-----------------------------------------------------------------------------
    Server Configuration -- Dynamic
------------------------------------------------------------------------------}
-- | URI type.
--
-- FIXME: Use the correct type from "Network.URI"
type URI = String

-- | MIME type.
type MimeType = String

-- | Representation of a "Foreign.JavaScript" server.
--
-- Can be used for dynamic configuration, e.g. serving additional files.
data Server = Server
    { sFiles :: MVar Filepaths
    , sDirs  :: MVar Filepaths
    , sLog   :: ByteString -> IO () -- function for logging
    }
type Filepaths = (Integer, Map ByteString (FilePath, MimeType))
newFilepaths = (0, Map.empty)

{-----------------------------------------------------------------------------
    Communication channel
------------------------------------------------------------------------------}
-- | Bidirectional communication channel.
data Comm = Comm
    { commIn    :: TQueue JSON.Value    -- ^ Read from channel.
    , commOut   :: TQueue JSON.Value    -- ^ Write into channel.
    , commOpen  :: TVar   Bool          -- ^ Indicate whether the channel is still open.
    , commClose :: IO ()                -- ^ Close the channel.
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
    | Exception String
    | Quit
    deriving (Eq, Show)

instance FromJSON ClientMsg where
    parseJSON (Object msg) = do
        tag <- msg .: "tag"
        case (tag :: Text) of
            "Event"     -> Event     <$> (msg .: "name") <*> (msg .: "arguments")
            "Result"    -> Result    <$> (msg .: "contents")
            "Exception" -> Exception <$> (msg .: "contents")
            "Quit"      -> return Quit

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
handles the actual communication with the client.

That's why we have to use the function
`Control.DeepSeq.deepseq` to make sure that any exception
is thrown before handing the message over to another thread.

Since exceptions in pure code do not have a precise ordering relative
to exceptions in IO code, evaluating the pure value
also helps with ensuring that the exception is raised before
any subsequent IO exception; this makes it easier to pinpoint
the root cause for library users.

-}


data JavaScriptException = JavaScriptException String deriving Typeable

instance E.Exception JavaScriptException

instance Show JavaScriptException where
    showsPrec _ (JavaScriptException err) = showString $ "JavaScript error: " ++ err

{-----------------------------------------------------------------------------
    Window & Event Loop
------------------------------------------------------------------------------}
-- | An event sent from the browser window to the server.
type Event        = (Coupon, JSON.Value)

-- | An event handler that can be passed to the JavaScript client.
type HsEvent      = RemotePtr (JSON.Value -> IO ())

quit :: Event
quit = ("quit", JSON.Null)

-- | Specification of how JavaScript functions should be called.
data CallBufferMode
    = NoBuffering
    -- ^ When 'runFunction' is used to call a JavaScript function,
    -- immediately send a message to the browser window to execute
    -- said function.
    | BufferRun
    -- ^ When 'runFunction' is used to call a JavaScript function,
    -- hold back any message to the server.
    -- All JavaScript functions that are held back in this way
    -- are combined into a single message,
    -- which is finally sent whenever 'callFunction' or
    -- 'flushCallBuffer' are used, or an exported Haskell function is called.
    | FlushOften
    -- ^ The same as 'BufferRun', but this mode indicates
    -- client libraries and programs are encouraged to flush the buffer more often
    -- to simplify usage. Users may choose 'BufferRun' instead if they want more control
    -- over flushing the buffer.
    | FlushPeriodically
    -- ^ The same as 'BufferRun', except that the buffer will also be flushed
    -- every 300ms.

flushPeriod = 300 :: Int

-- | Action that the server will run when a browser window connects.
type EventLoop   = Server -> RequestInfo -> Comm -> IO ()
type RequestInfo = [Cookie]

-- | Representation of a browser window.
data Window = Window
    { getServer      :: Server
    -- ^ Server that the browser window communicates with.
    , getCookies     :: [Cookie]
    -- ^ Cookies that the browser window has sent to the server when connecting.

    , runEval        :: String -> IO ()
    , callEval       :: String -> IO JSON.Value

    , wCallBuffer     :: TVar (String -> String)
    , wCallBufferMode :: TVar CallBufferMode

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
    b1  <- newTVarIO id
    b2  <- newTVarIO NoBuffering
    let nop = const $ return ()
    Window undefined [] nop undefined b1 b2 (return ()) nop nop ptr <$> newVendor <*> newVendor

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
