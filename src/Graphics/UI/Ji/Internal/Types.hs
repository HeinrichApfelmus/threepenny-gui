{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Prelude              hiding ((++),init)
import Text.JSON.Generic

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
  , sEventHandlers :: MVar (Map (String,String) (EventData -> m ()))
  , sElementIds :: MVar [Integer]
  , sToken :: Integer
  }
  
-- | Data from an event. At the moment it is empty.
data EventData = EventData

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
  | Bind String Element Closure
  | GetValue Element
  | GetValues [Element]
  deriving (Typeable,Data,Show)

-- | A signal (mostly events) that are sent from the client to the
-- server.

data Signal
  = Init ()
  | Elements [Element]
  | SingleElement Element
  | Event (String,String)
  | Value String
  | Values [String]
  deriving (Show)

instance JSON Signal where
  showJSON _ = error "JSON.Signal.showJSON: No method implemented."
  readJSON obj = do
    obj <- readJSON obj
    let init = Init <$> valFromObj "Init" obj
        elements = Elements <$> valFromObj "Elements" obj
        element = SingleElement <$> valFromObj "SingleElement" obj
        event = Event <$> valFromObj "Event" obj
        value = Value <$> valFromObj "Value" obj
        values = Values <$> valFromObj "Values" obj
    init <|> elements <|> element <|> event <|> value <|> values

-- | An opaque reference to a closure that the event manager uses to
--   trigger events signalled by the client.
data Closure = Closure (String,String)
  deriving (Typeable,Data,Show)
