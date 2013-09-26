{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.UI.Threepenny.Internal.Types where

import Prelude              hiding (init)

import Control.Applicative
import Control.Concurrent
import qualified Reactive.Threepenny    as E
import           Data.ByteString.Char8  (ByteString)
import qualified Data.ByteString.Char8  as BS
import Data.Map                         (Map)
import Data.String                      (fromString)
import Data.Time

import Network.URI
import Text.JSON.Generic
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
data ElementId   = ElementId BS.ByteString
                   deriving (Data,Typeable,Show,Eq,Ord)

type EventId  = String
type Handlers = Map EventId (E.Handler EventData)
type Events   = EventId -> E.Event EventData


-- Marshalling ElementId
instance JSON ElementId where
  showJSON (ElementId o) = showJSON o
  readJSON obj = do
    obj <- readJSON obj
    ElementId <$> valFromObj "Element" obj


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


-- | An opaque reference to a closure that the event manager uses to
--   trigger events signalled by the client.
data Closure = Closure (ElementId,EventId)
    deriving (Typeable,Data,Show)


{-----------------------------------------------------------------------------
    Public types
------------------------------------------------------------------------------}
-- | The client browser window.
type Window = Session

-- | Data from an event. At the moment it is empty.
data EventData = EventData [Maybe String]

-- | Record for configuring the Threepenny GUI server.
data Config = Config
  { tpPort       :: Int                 -- ^ Port number.
  , tpCustomHTML :: Maybe FilePath      -- ^ Custom HTML file to replace the default one.
  , tpStatic     :: Maybe FilePath      -- ^ Directory that is served under @/static@.
  , tpLog        :: ByteString -> IO () -- ^ Print a single log message.
  }

-- | Default configuration.
--
-- Port 10000, no custom HTML, no static directory, logging to stderr.
defaultConfig :: Config
defaultConfig = Config
    { tpPort       = 10000
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
  | GetElementsByClassName String
  | GetElementsById [String]
  | GetElementsByTagName String
  | Bind EventId ElementId
  | GetValues [ElementId]
  | RunJSFunction String
  | CallJSFunction String
  | CallDeferredFunction (Closure,String,[String])
  | Delete ElementId
  deriving (Typeable,Data,Show)

instance JSON Instruction where
    readJSON _ = error "JSON.Instruction.readJSON: No method implemented."
    showJSON x = toJSON x 

-- | A signal (mostly events) that are sent from the client to the server.
data Signal
  = Quit ()
  | Elements [ElementId]
  | Event ElementId EventId [Maybe String]
  | Values [String]
  | FunctionCallValues [Maybe String]
  | FunctionResult JSValue
  deriving (Typeable,Show)

instance JSON Signal where
  showJSON _ = error "JSON.Signal.showJSON: No method implemented."
  readJSON obj = do
    obj <- readJSON obj
    let quit     = Quit <$> valFromObj "Quit" obj
        elements = Elements <$> valFromObj "Elements" obj
        event = do
          e         <- valFromObj "Event" obj
          elid      <- valFromObj "Element" e
          eventId   <- valFromObj "EventId" e
          arguments <- valFromObj "Params"  e
          args      <- mapM nullable arguments
          return $ Event elid eventId args
        values = Values <$> valFromObj "Values" obj
        fcallvalues = do
          FunctionCallValues <$> (valFromObj "FunctionCallValues" obj >>= mapM nullable)
        fresult = FunctionResult <$> valFromObj "FunctionResult" obj
    quit <|> elements <|> event <|> values <|> fcallvalues <|> fresult

-- | Read a JSValue that may be null.
nullable :: JSON a => JSValue -> Result (Maybe a)
nullable JSNull = return Nothing
nullable v      = Just <$> readJSON v

