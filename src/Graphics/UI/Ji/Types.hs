{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Graphics.UI.Ji.Types
  where

import Control.Applicative
import Control.Concurrent
import Control.Monad.Reader
import Data.Map             (Map)
import Prelude              hiding ((++),init)
import Text.JSON.Generic

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

data Signal
  = Init ()
  | Elements [Element]
  | SingleElement Element
  | Event (String,String)
  | Value String
  | Values [String]
  deriving (Show)

data Element = Element String
  deriving (Data,Typeable,Show)

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
  
instance JSON Element where
  showJSON _ = error "JSON.Element.showJSON: No method implemented."
  readJSON obj = do
    obj <- readJSON obj
    Element <$> valFromObj "Element" obj

data Session m = Session
  { sSignals :: Chan Signal
  , sInstructions :: Chan Instruction
  , sEventHandlers :: MVar (Map (String,String) (EventData -> m ()))
  , sElementIds :: MVar [Integer]
  , sToken :: Integer
  }

data EventData = EventData

data Closure = Closure (String,String)
  deriving (Typeable,Data,Show)

class MonadIO m => MonadJi m where
  askSession :: m (Session m)

newtype Ji a = Ji { getJi :: ReaderT (Session Ji) IO a }
  deriving (Monad,MonadIO,MonadReader (Session Ji))

instance MonadJi Ji where
  askSession = ask
