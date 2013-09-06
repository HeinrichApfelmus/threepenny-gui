{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Graphics.UI.Threepenny.Internal.Types where

import Prelude              hiding (init)

import Control.Applicative
import Control.Concurrent
import qualified Reactive.Threepenny as E
import Data.ByteString               (ByteString, hPut)
import Data.Map                      (Map)
import Data.String                   (fromString)
import Data.Time

import Network.URI
import Text.JSON.Generic
import System.IO (stderr)

{-----------------------------------------------------------------------------
    Public types
------------------------------------------------------------------------------}

-- | Reference to an element in the DOM of the client window.
data Element = Element
    { elId      :: ElementId
    , elSession :: Session
    }

instance Show Element where
    show = show . elId

-- | An opaque reference to an element in the DOM.
data ElementId = ElementId String
  deriving (Data,Typeable,Show,Eq,Ord)

instance JSON ElementId where
  showJSON (ElementId o) = showJSON o
  readJSON obj = do
    obj <- readJSON obj
    ElementId <$> valFromObj "Element" obj


-- | A client session. This type is opaque, you don't need to inspect it.
data Session = Session
  { sSignals        :: Chan Signal
  , sInstructions   :: Chan Instruction
  , sMutex          :: MVar ()
  , sEventHandlers  :: MVar (Map EventKey (E.Handler EventData))
  , sElementEvents  :: MVar (Map ElementId ElementEvents)
  , sEventQuit      :: (E.Event (), E.Handler ())
  , sClosures       :: MVar [Integer]
  , sElementIds     :: MVar [Integer]
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
    , tpLog        = \s -> hPut stderr s >> hPut stderr (fromString "\n")
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
  | EmptyEl ElementId
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

{-----------------------------------------------------------------------------
    JavaScript Code and Foreign Function Interface
------------------------------------------------------------------------------}
-- | JavaScript code snippet.
newtype JSCode = JSCode { unJSCode :: String }
    deriving (Eq, Ord, Show, Data, Typeable)

-- | Class for rendering Haskell types as JavaScript code.
class ToJS a where
    render :: a -> JSCode

instance ToJS String    where render   = JSCode . show
instance ToJS Int       where render   = JSCode . show
instance ToJS Bool      where render b = JSCode $ if b then "false" else "true"
instance ToJS JSValue   where render x = JSCode $ showJSValue x ""
instance ToJS ElementId where
    render (ElementId x) = apply "elidToElement(%1)" [render x]
instance ToJS Element   where render (Element e _) = render e


-- | Representation of a JavaScript expression
-- with a girven output type.
data JSFunction a = JSFunction
    { code    :: JSCode                          -- code snippet
    , marshal :: Window -> JSValue -> Result a   -- convert to Haskell value
    }

instance Functor JSFunction where
    fmap f = fmapWindow (const f)

fmapWindow :: (Window -> a -> b) -> JSFunction a -> JSFunction b
fmapWindow f (JSFunction c m) = JSFunction c (\w v -> f w <$> m w v)

fromJSCode :: JSCode -> JSFunction ()
fromJSCode c = JSFunction { code = c, marshal = \_ _ -> Ok () }

-- | Helper class for making a simple JavaScript FFI
class FFI a where
    fancy :: ([JSCode] -> JSCode) -> a

instance (ToJS a, FFI b) => FFI (a -> b) where
    fancy f a = fancy $ f . (render a:)

instance FFI (JSFunction ())        where fancy f = fromJSCode $ f []
instance FFI (JSFunction String)    where fancy   = mkResult "%1.toString()"
instance FFI (JSFunction JSValue)   where fancy   = mkResult "%1"
instance FFI (JSFunction ElementId) where
    fancy   = mkResult "{ Element: elementToElid(%1) }"
instance FFI (JSFunction Element)   where
    fancy   = fmapWindow (\w elid -> Element elid w) . fancy

mkResult :: JSON a => String -> ([JSCode] -> JSCode) -> JSFunction a
mkResult client f = JSFunction
    { code    = apply client [f []]
    , marshal = \w -> readJSON
    }

-- | Simple JavaScript FFI with string substitution.
--
-- Inspired by the Fay language. <http://fay-lang.org/>
--
-- > example :: String -> Int -> JSFunction String
-- > example = ffi "$(%1).prop('checked',%2)"
--
-- The 'ffi' function takes a string argument representing the JavaScript
-- code to be executed on the client.
-- Occurrences of the substrings @%1@ to @%9@ will be replaced by
-- subequent arguments.
--
-- Note: Always specify a type signature! The types automate
-- how values are marshalled between Haskell and JavaScript.
-- The class instances for the 'FFI' class show which conversions are supported.
--
ffi :: FFI a => String -> a
ffi macro = fancy (apply macro)

testFFI :: String -> Int -> JSFunction String
testFFI = ffi "$(%1).prop('checked',%2)"

-- | String substitution.
-- Substitute occurences of %1, %2 up to %9 with the argument strings.
-- The types ensure that the % character has no meaning in the generated output.
-- 
-- > apply "%1 and %2" [x,y] = x ++ " and " ++ y
apply :: String -> [JSCode] -> JSCode
apply code args = JSCode $ go code
    where
    argument i = unJSCode (args !! i)
    
    go []         = []
    go ('%':c:cs) = argument index ++ go cs
        where index = fromEnum c - fromEnum '1'
    go (c:cs)     = c : go cs

