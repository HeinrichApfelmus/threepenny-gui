{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.UI.Threepenny.Internal.Types where

import Prelude              hiding (init)

import Control.Applicative
import Control.Concurrent
import Control.DeepSeq
import Control.Monad
import qualified Reactive.Threepenny    as E
import           Data.ByteString.Char8  (ByteString)
import qualified Data.ByteString.Char8  as BS
import Data.Map                         (Map)
import Data.String                      (fromString)
import Data.Time
import Data.Text.Encoding               (encodeUtf8, decodeUtf8)

import Network.URI
import Data.Data
import           Data.Aeson             as JSON
import qualified Data.Aeson.Types       as JSON
import Data.Text

import System.IO (stderr)
import System.IO.Unsafe

import qualified Foreign.Coupon as Foreign

{-----------------------------------------------------------------------------
    Elements and ElementIds
------------------------------------------------------------------------------}
-- | Reference to an element in the DOM of the client window.
type Element     = Foreign.Item ElementData
data ElementData = ElementData
    { elTagName  :: String          -- element is a <tag>..</tag> element
    , elSession  :: Session         -- associated browser window
    , elHandlers :: MVar Handlers   -- event handlers associated with that element
    , elEvents   :: Events          -- events         associated with that element
    }
newtype ElementId = ElementId BS.ByteString
    deriving (Data,Typeable,Show,Eq,Ord)

instance NFData ElementId where
    rnf (ElementId x) =
#if defined(CABAL) || defined(FPCOMPLETE)
#if MIN_VERSION_bytestring(0, 10, 0)
        rnf x
#else
        BS.length x `seq` ()
#endif
#else
        rnf x
#endif

type EventId  = String
type Handlers = Map EventId (E.Handler EventData)
type Events   = EventId -> E.Event EventData


-- Marshalling ElementId
instance ToJSON ElementId where
    toJSON (ElementId o)  = toJSON $ decodeUtf8 o
instance FromJSON ElementId where
    parseJSON (Object v)  = (ElementId . encodeUtf8) <$> v .: "Element"
    parseJSON _           = mzero


-- | Perform an action on the element.
-- The element is not garbage collected while the action is run.
withElementData :: Element -> (ElementId -> ElementData -> IO a) -> IO a
withElementData e f = Foreign.withItem e $ \coupon el ->
    let elid = ElementId $ case fromString (elTagName el) of
            ""     -> coupon
            "head" -> "head"
            "body" -> "body" 
            tag    -> BS.concat ["*",coupon,":",tag]
    in f elid el

-- | Special case of 'withElementData'.
withElement :: Element -> (ElementId -> Session -> IO b) -> IO b
withElement e f = withElementData e $ \elid el -> f elid (elSession el)

-- | Get 'ElementId' without any guarantee that the element is still alive.
unprotectedGetElementId :: Element -> ElementId
unprotectedGetElementId e = unsafePerformIO . withElement e $ \elid _ -> return elid


-- | Look up an element in the browser window.
lookupElement :: ElementId -> Session -> IO Element
lookupElement (ElementId xs) Session{..} = case xs of
        "head"      -> return sHeadElement
        "body"      -> return sBodyElement
        xs          -> maybe (error msg) id <$> Foreign.lookup (coupon xs) sPrizeBooth
    where
    coupon xs = if BS.head xs == '*'
        then BS.takeWhile (/= ':') . BS.tail $ xs
        else xs

    msg = "Graphics.UI.Threepenny: Fatal error: ElementId " ++ show xs
        ++ "was garbage collected on the server, but is still present in the browser."


{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
-- | A client session.
data Session = Session
  { sSignals        :: Chan Signal
  , sInstructions   :: Chan Instruction
  , sMutex          :: MVar ()
  , sEventQuit      :: (E.Event (), E.Handler ())
  , sClosures       :: MVar [Integer]
  , sPrizeBooth     :: Foreign.PrizeBooth ElementData
  , sHeadElement    :: Element
  , sBodyElement    :: Element
  , sToken          :: Integer
  , sConnectedState :: MVar ConnectedState
  , sThreadId       :: ThreadId
  , sStartInfo      :: (URI,[(String,String)])
  , sServerState    :: ServerState
  }

type Sessions      = Map Integer Session
type MimeType      = ByteString
type Filepaths     = (Integer, Map ByteString (FilePath, MimeType))

data ServerState = ServerState
    { sSessions :: MVar Sessions
    , sFiles    :: MVar Filepaths
    , sDirs     :: MVar Filepaths
    }

data ConnectedState
  = Disconnected UTCTime -- ^ The time that the poll disconnected, or
                         -- the first initial connection time.
  | Connected            -- ^ The client is connected, we don't care
                         -- since when.
  deriving (Show)

-- | A Haskell value/function of type 'a',
-- presented in a form that can be called from JavaScript.
data HsFunction a = HsFunction ElementId EventId
    deriving (Typeable, Data, Show)


{-----------------------------------------------------------------------------
    Public types
------------------------------------------------------------------------------}
-- | The client browser window.
type Window = Session

-- | Data from an event. At the moment it is empty.
data EventData = EventData [Maybe String]

-- | Record for configuring the Threepenny GUI server.
data Config = Config
    { tpPort       :: Maybe Int           
        -- ^ Port number.
        -- @Nothing@ means that the port number is
        -- read from the environment variable @PORT@.
        -- Alternatively, port @8023@ is used if this variable is not set.
    , tpCustomHTML :: Maybe FilePath
        -- ^ Custom HTML file to replace the default one.
    , tpStatic     :: Maybe FilePath
        -- ^ Directory that is served under @/static@.
    , tpLog        :: ByteString -> IO ()
        -- ^ Print a single log message.
    }

defaultPort :: Int
defaultPort = 8023

-- | Default configuration.
--
-- Port from environment variable or @8023@,
-- no custom HTML, no static directory, logging to stderr.
defaultConfig :: Config
defaultConfig = Config
    { tpPort       = Nothing
    , tpCustomHTML = Nothing
    , tpStatic     = Nothing
    , tpLog        = \s -> BS.hPut stderr s >> BS.hPut stderr "\n"
    }


{-----------------------------------------------------------------------------
    Communication between client and server
------------------------------------------------------------------------------}
-- | An instruction that is sent to the client as JSON.
data Instruction
  = Debug String
  | SetToken Integer
  | Bind EventId ElementId
  | GetValues [ElementId]
  | RunJSFunction String
  | CallJSFunction String
  | Delete ElementId
  deriving (Typeable,Data,Show)

instance ToJSON Instruction where
    toJSON (Debug x)          = object [ "tag" .= ("Debug" :: Text)
                                       , "contents" .= x]
    toJSON (SetToken x)       = object [ "tag" .= ("SetToken" :: Text)
                                       , "contents" .= x]
    toJSON (Bind x y)         = object [ "tag" .= ("Bind" :: Text)
                                       , "contents" .= [toJSON x, toJSON y]]
    toJSON (GetValues xs)     = object [ "tag" .= ("GetValues" :: Text)
                                       , "contents" .= xs]
    toJSON (RunJSFunction  x) = object [ "tag" .= ("RunJSFunction" :: Text)
                                       , "contents" .= x]
    toJSON (CallJSFunction x) = object [ "tag" .= ("CallJSFunction" :: Text)
                                       , "contents" .= x]
    toJSON (Delete x)         = object [ "tag" .= ("Delete" :: Text)
                                       , "contents" .= x]

instance NFData Instruction where
    rnf (Debug    x  ) = rnf x
    rnf (SetToken x  ) = rnf x
    rnf (Bind     x y) = rnf x `seq` rnf y
    rnf (GetValues xs) = rnf xs
    rnf (RunJSFunction  x) = rnf x
    rnf (CallJSFunction x) = rnf x
    rnf (Delete x)     = rnf x

-- | A signal (mostly events) that are sent from the client to the server.
data Signal
  = Quit ()
  | Event ElementId EventId [Maybe String]
  | Values [String]
  | FunctionCallValues [Maybe String]
  | FunctionResult JSON.Value
  deriving (Typeable,Show)

instance FromJSON Signal where
  parseJSON (Object v) = do
    let quit  = Quit <$> v .: "Quit"
        event = do
          e         <- v .: "Event"
          elid      <- e .: "Element"
          eventId   <- e .: "EventId"
          arguments <- e .: "Params"
          args      <- mapM nullable arguments
          return $ Event elid eventId args
        values = Values <$> v .: "Values"
        fcallvalues = do
          FunctionCallValues <$> (v .: "FunctionCallValues" >>= mapM nullable)
        fresult = FunctionResult <$> v .: "FunctionResult"
    quit <|> event <|> values <|> fcallvalues <|> fresult
  parseJSON _        = mzero

-- | Read a JSON Value that may be null.
nullable :: FromJSON a => JSON.Value -> JSON.Parser (Maybe a)
nullable Null = return Nothing
nullable v    = Just <$> parseJSON v

