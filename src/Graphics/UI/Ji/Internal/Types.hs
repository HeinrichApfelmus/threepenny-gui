{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | All Ji's types. See "Graphics.UI.Ji.Types" for only public
--   types. Non-public types can be manipulated at your own risk, if you
--   know what you're doing and you want to add something that the
--   library doesn't do.

module Graphics.UI.Ji.Internal.Types
  where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Data.Map             (Map)
import Data.Time
import Prelude              hiding ((++),init)
import Text.JSON.Generic
import Network.URI

--------------------------------------------------------------------------------
-- Public types

-- | An opaque reference to an element in the DOM.
data Element = Element String
  deriving (Data,Typeable,Show)

instance JSON Element where
  showJSON _ = error "JSON.Element.showJSON: No method implemented."
  readJSON obj = do
    obj <- readJSON obj
    Element <$> valFromObj "Element" obj

-- | A monad for running a Ji session. Please implement this if you
--   need to run your own monad. It merely needs to access a 'Session'
--   data type which contains its events and things like that.
class MonadIO m => MonadJi m where
  askSession :: m (Session m)

-- | A simple monad implementing 'MonadJi' for reading the 'Session' state.
newtype Ji a = Ji { getJi :: ReaderT (Session Ji) IO a }
  deriving (Monad,MonadIO,MonadReader (Session Ji))

-- | Simple reader implementation.
instance MonadJi Ji where
  askSession = ask
  
-- | A Ji session. This type is opaque, you don't need to inspect it,
--   just be able to carry it in the 'MonadJi' monad.
data Session m = Session
  { sSignals :: Chan Signal
  , sInstructions :: Chan Instruction
  , sEventHandlers :: MVar (Map (String,String) ([Maybe String] -> m ()))
  , sClosures :: MVar [Integer]
  , sElementIds :: MVar [Integer]
  , sToken :: Integer
  , sMutex :: MVar ()
  , sConnectedState :: MVar ConnectedState
  , sThreadId :: ThreadId
  , sStartInfo :: (URI,[(String,String)])
  }

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
  | GetElementById String
  | GetElementsByTagName String
  | SetStyle Element [(String,String)]
  | SetAttr Element String String
  | Append Element Element
  | SetText Element String
  | SetHtml Element String
  | Bind String String Closure
  | GetValue Element
  | GetValues [Element]
  | SetTitle String
  | GetLocation ()
  | CallFunction (String,[String])
  | CallDeferredFunction (Closure,String,[String])
  | EmptyEl Element
  | Delete Element
  deriving (Typeable,Data,Show)

-- | A signal (mostly events) that are sent from the client to the
-- server.

data Signal
  = Init ()
  | Elements [Element]
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

data Config m a = Config
  { jiPort   :: Int                        -- ^ Port.
  , jiRun    :: (Session m -> m a -> IO a) -- ^ How to run the worker monad.
  , jiWorker :: m a                        -- ^ The worker.
  , jiStatic :: FilePath                   -- ^ Static files path.
  }
