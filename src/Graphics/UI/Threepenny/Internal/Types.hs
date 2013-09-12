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

import qualified System.Mem.Coupon as Foreign

{-----------------------------------------------------------------------------
    Elements and ElementIds
------------------------------------------------------------------------------}
-- | Reference to an element in the DOM of the client window.
type Element     = Foreign.Item ElementData
data ElementData = ElementData
    { elTagName :: String
    , elSession :: Session
    }
data ElementId   = ElementId BS.ByteString
                   deriving (Data,Typeable,Show,Eq,Ord)

getSession :: Element -> Session
getSession = elSession . Foreign.getValue

-- Marshalling ElementId

instance JSON ElementId where
  showJSON (ElementId o) = showJSON o
  readJSON obj = do
    obj <- readJSON obj
    ElementId <$> valFromObj "Element" obj


getElementId :: Element -> ElementId
getElementId e = ElementId $ case tag of
        ""     -> coupon
        "head" -> "head"
        "body" -> "body" 
        tag    -> BS.concat ["*",coupon,":",tag]
    where
    coupon = Foreign.getCoupon e
    tag    = fromString . elTagName . Foreign.getValue $ e

-- | Look up an element in the browser window.
lookupElement :: ElementId -> Session -> IO Element
lookupElement (ElementId xs) Session{..} = case xs of
        "head"      -> return sHeadElement
        "body"      -> return sBodyElement
        xs          -> maybe (error msg) id <$> Foreign.lookup (coupon xs) sRemoteBooth
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
  , sEventHandlers  :: MVar (Map EventKey (E.Handler EventData))
  , sElementEvents  :: MVar (Map ElementId ElementEvents)
  , sEventQuit      :: (E.Event (), E.Handler ())
  , sClosures       :: MVar [Integer]
  , sRemoteBooth    :: Foreign.RemoteBooth ElementData
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

type EventKey      = (String, String)
type ElementEvents = String -> E.Event EventData

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
  | SetStyle ElementId [(String,String)]
  | SetAttr ElementId String String
  | Append ElementId ElementId
  | SetText ElementId String
  | SetHtml ElementId String
  | Bind String ElementId Closure
  | GetValue ElementId
  | GetValues [ElementId]
  | SetTitle String
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
  | Event (String,String,[Maybe String])
  | Value String
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
          (cid,typ,arguments) <- valFromObj "Event" obj
          args <- mapM nullable arguments
          return $ Event (cid,typ,args)
        value = Value <$> valFromObj "Value" obj
        values = Values <$> valFromObj "Values" obj
        fcallvalues = do
          FunctionCallValues <$> (valFromObj "FunctionCallValues" obj >>= mapM nullable)
        fresult = FunctionResult <$> valFromObj "FunctionResult" obj
    quit <|> elements <|> event <|> value <|> values <|> fcallvalues <|> fresult

-- | Read a JSValue that may be null.
nullable :: JSON a => JSValue -> Result (Maybe a)
nullable JSNull = return Nothing
nullable v      = Just <$> readJSON v

-- | An opaque reference to a closure that the event manager uses to
--   trigger events signalled by the client.
data Closure = Closure EventKey
    deriving (Typeable,Data,Show)

