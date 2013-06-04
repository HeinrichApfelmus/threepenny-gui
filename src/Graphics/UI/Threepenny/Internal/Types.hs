{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All Threepenny's types. See "Graphics.UI.Threepenny.Types" for only public
--   types. Non-public types can be manipulated at your own risk, if you
--   know what you're doing and you want to add something that the
--   library doesn't do.

module Graphics.UI.Threepenny.Internal.Types
  where

import Prelude              hiding ((++),init)

import Control.Applicative
import Control.Concurrent
import qualified Control.Event as E
import Control.Monad.Reader
import Data.ByteString               (ByteString)
import Data.Map             (Map)
import Data.Time

import Network.URI
import Text.JSON.Generic

--------------------------------------------------------------------------------
-- Public types

-- | Reference to an element in the DOM of the client window.
data Element = Element
    { elId      :: ElementId
    , elSession :: Session
    }

instance Show Element where
    show = show . elId

-- | An opaque reference to an element in the DOM.
data ElementId = ElementId String
  deriving (Data,Typeable,Show)

instance JSON ElementId where
  showJSON (ElementId o) = showJSON o
  readJSON obj = do
    obj <- readJSON obj
    ElementId <$> valFromObj "Element" obj

  
-- | A client session. This type is opaque, you don't need to inspect it.
data Session = Session
  { sSignals        :: Chan Signal
  , sInstructions   :: Chan Instruction
  , sEvent          :: EventKey -> E.Event EventData
  , sEventHandler   :: E.Handler (EventKey, EventData)
  , sClosures       :: MVar [Integer]
  , sElementIds     :: MVar [Integer]
  , sToken          :: Integer
  , sMutex          :: MVar ()
  , sConnectedState :: MVar ConnectedState
  , sThreadId       :: ThreadId
  , sStartInfo      :: (URI,[(String,String)])
  , sServerState    :: ServerState
  }

type Sessions  = Map Integer Session
type Filepaths = (Integer, Map ByteString FilePath)

data ServerState = ServerState
    { sSessions :: MVar Sessions
    , sFiles    :: MVar Filepaths
    , sDirs     :: MVar Filepaths
    }

type EventKey = (String, String)

-- | The client browser window.
type Window = Session

data ConnectedState
  = Disconnected UTCTime -- ^ The time that the poll disconnected, or
                         -- the first initial connection time.
  | Connected            -- ^ The client is connected, we don't care
                         -- since when.
  deriving (Show)

-- | Data from an event. At the moment it is empty.
data EventData = EventData [Maybe String]

--------------------------------------------------------------------------------
-- Internal types

-- | An instruction that is sent to the client as JSON.
data Instruction
  = Debug String
  | Begin ()
  | End ()
  | SetToken Integer
  | Clear ()
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
  | GetLocation ()
  | CallFunction (String,[String])
  | CallDeferredFunction (Closure,String,[String])
  | EmptyEl ElementId
  | Delete ElementId
  deriving (Typeable,Data,Show)

-- | A signal (mostly events) that are sent from the client to the server.
data Signal
  = Init ()
  | Elements [ElementId]
  | Event (String,String,[Maybe String])
  | Value String
  | Values [String]
  | Location String
  | FunctionCallValues [Maybe String]
  deriving (Show)

instance JSON Signal where
  showJSON _ = error "JSON.Signal.showJSON: No method implemented."
  readJSON obj = do
    obj <- readJSON obj
    let init = Init <$> valFromObj "Init" obj
        elements = Elements <$> valFromObj "Elements" obj
        event = do
          (cid,typ,arguments) <- valFromObj "Event" obj
          args <- mapM nullable arguments
          return $ Event (cid,typ,args)
        value = Value <$> valFromObj "Value" obj
        location = Location <$> valFromObj "Location" obj
        values = Values <$> valFromObj "Values" obj
        fcallvalues = do
          FunctionCallValues <$> (valFromObj "FunctionCallValues" obj >>= mapM nullable)
    init <|> elements <|> event <|> value <|> values <|> location <|> fcallvalues

-- | Read a JSValue that may be null.
nullable :: JSON a => JSValue -> Result (Maybe a)
nullable JSNull = return Nothing
nullable v      = Just <$> readJSON v

-- | An opaque reference to a closure that the event manager uses to
--   trigger events signalled by the client.
data Closure = Closure (String,String)
  deriving (Typeable,Data,Show)

-- | Record for configuring the Threepenny GUI server.
data Config = Config
  { tpPort       :: Int               -- ^ Port number.
  , tpCustomHTML :: Maybe FilePath    -- ^ Custom HTML file to replace the default one.
  , tpStatic     :: FilePath          -- ^ Directory that is served under @/static@.
  }
