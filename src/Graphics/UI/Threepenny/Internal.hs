{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.UI.Threepenny.Internal (
    -- * Synopsis
    -- | Internal core:
    -- 'UI' monad, integrating FRP and JavaScript FFI. garbage collection

    -- * Documentation
    Window, disconnect,
    startGUI,

    UI, runUI, liftIOLater, askWindow,

    FFI, FromJS, ToJS, JSFunction, JSObject, ffi,
    runFunction, callFunction, ffiExport, debug, timestamp,

    Element, fromJSObject, getWindow,
    mkElementNamespace, mkElement, delete, appendChild, clearChildren,

    EventData, domEvent, unsafeFromJSON,
    ) where

import           Control.Applicative                   (Applicative)
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import qualified Control.Monad.Trans.RWS.Lazy as Monad
import           Data.Dynamic                          (Typeable)

import qualified Data.Aeson              as JSON
import qualified Foreign.JavaScript      as JS
import qualified Foreign.RemotePtr       as Foreign

import qualified Reactive.Threepenny     as E

import Foreign.JavaScript hiding (runFunction, callFunction, debug, timestamp, Window)

{-----------------------------------------------------------------------------
    Custom Window type
------------------------------------------------------------------------------}
-- | The type 'Window' represents a browser window.
data Window = Window
    { jsWindow    :: JS.Window  -- JavaScript window
    , eDisconnect :: E.Event () -- event that happens when client disconnects
    , wEvents     :: Foreign.Vendor Events
                     -- events associated to 'Element's
    , wChildren   :: Foreign.Vendor ()
                     -- children reachable from 'Element's
    }

-- | Start server for GUI sessions.
startGUI
    :: Config               -- ^ Server configuration.
    -> (Window -> UI ())    -- ^ Action to run whenever a client browser connects.
    -> IO ()
startGUI config init = JS.serve config $ \w -> do
    -- set up disconnect event
    (eDisconnect, handleDisconnect) <- E.newEvent
    JS.onDisconnect w $ handleDisconnect ()

    -- make window
    wEvents   <- Foreign.newVendor
    wChildren <- Foreign.newVendor
    let window = Window
            { jsWindow    = w
            , eDisconnect = eDisconnect
            , wEvents     = wEvents
            , wChildren   = wChildren
            }

    -- run initialization
    runUI window $ init window

-- | Event that occurs whenever the client has disconnected,
-- be it by closing the browser window or by exception.
--
-- Note: DOM Elements in a browser window that has been closed
-- can no longer be manipulated.
disconnect :: Window -> E.Event ()
disconnect = eDisconnect

{-----------------------------------------------------------------------------
    Elements
------------------------------------------------------------------------------}
type Events = String -> E.Event JSON.Value

-- Reachability information for children of an 'Element'.
-- The children of an element are always reachable from this RemotePtr.
type Children = Foreign.RemotePtr ()

data Element = Element
    { toJSObject  :: JS.JSObject -- corresponding JavaScript object
    , elEvents    :: Events      -- FRP event mapping
    , elChildren  :: Children    -- The children of this element
    , elWindow    :: Window      -- Window in which the element was created
    } deriving (Typeable)

instance ToJS Element where
    render = render . toJSObject

getWindow :: Element -> IO Window
getWindow = return . elWindow

-- | Lookup or create reachability information for the children of
-- an element that is represented by a JavaScript object.
getChildren :: JS.JSObject -> Window -> IO Children
getChildren el window@Window{ wChildren = wChildren } =
    Foreign.withRemotePtr el $ \coupon _ -> do
        mptr <- Foreign.lookup coupon wChildren
        case mptr of
            Nothing -> do
                -- Create new pointer for reachability information.
                ptr <- Foreign.newRemotePtr coupon () wChildren
                Foreign.addReachable el ptr
                return ptr
            Just p  ->
                -- Return existing information
                return p

-- | Convert JavaScript object into an Element by attaching relevant information.
-- The JavaScript object may still be subject to garbage collection.
fromJSObject0 :: JS.JSObject -> Window -> IO Element
fromJSObject0 el window = do
    events   <- getEvents   el window
    children <- getChildren el window
    return $ Element el events children window

-- | Convert JavaScript object into an element.
--
-- FIXME: For the purpose of garbage collection, this element
-- will always be reachable from the root.
fromJSObject :: JS.JSObject -> UI Element
fromJSObject el = do
    window <- askWindow
    liftIO $ do
        Foreign.addReachable (JS.root $ jsWindow window) el
        fromJSObject0 el window

-- | Add lazy FRP events to a JavaScript object.
addEvents :: JS.JSObject -> Window -> IO Events
addEvents el Window{ jsWindow = w, wEvents = wEvents } = do
    -- Lazily create FRP events whenever they are needed.
    let initializeEvent (name,_,handler) = do
            handlerPtr <- JS.exportHandler w handler
            -- make handler reachable from element
            Foreign.addReachable el handlerPtr
            JS.runFunction w $
                ffi "Haskell.bind(%1,%2,%3)" el name handlerPtr

    events <- E.newEventsNamed initializeEvent

    -- Create new pointer and add reachability.
    Foreign.withRemotePtr el $ \coupon _ -> do
        ptr <- Foreign.newRemotePtr coupon events wEvents
        Foreign.addReachable el ptr

    return events

-- | Lookup or create lazy events for a JavaScript object.
getEvents :: JS.JSObject -> Window -> IO Events
getEvents el window@Window{ wEvents = wEvents } = do
    Foreign.withRemotePtr el $ \coupon _ -> do
        mptr <- Foreign.lookup coupon wEvents
        case mptr of
            Nothing -> addEvents el window
            Just p  -> Foreign.withRemotePtr p $ \_ -> return

-- | Events may carry data. At the moment, they may return
-- a single JSON value, as defined in the "Data.Aeson" module.
type EventData = JSON.Value

-- | Convert event data to a Haskell value.
-- Throws an exception when the data cannot be converted.
unsafeFromJSON :: JSON.FromJSON a => EventData -> a
unsafeFromJSON x = let JSON.Success y = JSON.fromJSON x in y

-- | Obtain DOM event for a given element.
domEvent
    :: String
        -- ^ Event name. A full list can be found at
        --   <http://www.w3schools.com/jsref/dom_obj_event.asp>.
        --   Note that the @on@-prefix is not included,
        --   the name is @click@ and so on.
    -> Element          -- ^ Element where the event is to occur.
    -> E.Event EventData
domEvent name el = elEvents el name

-- | Make a new DOM element with a given tag name.
mkElement :: String -> UI Element
mkElement = mkElementNamespace Nothing

-- | Make a new DOM element with a namespace and a given tag name.
--
-- A namespace 'Nothing' corresponds to the default HTML namespace.
mkElementNamespace :: Maybe String -> String -> UI Element
mkElementNamespace namespace tag = do
    window <- askWindow
    let w = jsWindow window
    liftIO $ do
        el <- JS.unsafeCreateJSObject w $ case namespace of
            Nothing -> ffi "document.createElement(%1)" tag
            Just ns -> ffi "document.createElementNS(%1,%2)" ns tag
        fromJSObject0 el window

-- | Delete the given element.
delete :: Element -> UI ()
delete el = liftJSWindow $ \w -> do
    JS.runFunction w $ ffi "$(%1).detach()" el
    Foreign.destroy $ toJSObject el

-- | Remove all child elements.
clearChildren :: Element -> UI ()
clearChildren element = liftJSWindow $ \w -> do
    let el = toJSObject element
    Foreign.withRemotePtr el $ \_ _ -> do
        -- Previous children are no longer reachable from this element
        JS.runFunction w $ ffi "$(%1).contents().detach()" el
        Foreign.clearReachable (elChildren element)

-- | Append a child element.
appendChild :: Element -> Element -> UI ()
appendChild parent child = liftJSWindow $ \w -> do
    -- FIXME: We have to stop the child being reachable from its
    -- /previous/ parent.
    Foreign.addReachable (elChildren parent) (toJSObject child)
    JS.runFunction w $ ffi "$(%1).append($(%2))" (toJSObject parent) (toJSObject child)


{-----------------------------------------------------------------------------
    UI monad
------------------------------------------------------------------------------}
{- |

User interface elements are created and manipulated in the 'UI' monad.

This monad is essentially just a thin wrapper around the familiar 'IO' monad.
Use the 'liftIO' function to access 'IO' operations like reading
and writing from files.

There are several subtle reasons why Threepenny
uses a custom 'UI' monad instead of the standard 'IO' monad:

* More convenience when calling JavaScript.
The monad keeps track of a browser 'Window' context
in which JavaScript function calls are executed.

* Recursion for functional reactive programming.

-}
newtype UI a = UI { unUI :: Monad.RWST Window [IO ()] () IO a }
    deriving (Typeable)

liftJSWindow :: (JS.Window -> IO a) -> UI a
liftJSWindow f = askWindow >>= liftIO . f . jsWindow

instance Functor UI where
    fmap f = UI . fmap f . unUI

instance Applicative UI where
    pure  = return
    (<*>) = ap

instance Monad UI where
    return  = UI . return
    m >>= k = UI $ unUI m >>= unUI . k

instance MonadIO UI where
    liftIO = UI . liftIO

instance MonadFix UI where
    mfix f = UI $ mfix (unUI . f)

-- | Execute an 'UI' action in a particular browser window.
-- Also runs all scheduled 'IO' actions.
runUI :: Window -> UI a -> IO a
runUI window m = do
    (a, _, actions) <- Monad.runRWST (unUI m) window ()
    sequence_ actions
    return a

-- | Retrieve current 'Window' context in the 'UI' monad.
askWindow :: UI Window
askWindow = UI Monad.ask

-- | Schedule an 'IO' action to be run later.
liftIOLater :: IO () -> UI ()
liftIOLater x = UI $ Monad.tell [x]

{-----------------------------------------------------------------------------
    FFI
------------------------------------------------------------------------------}
-- | Run the given JavaScript function and carry on. Doesn't block.
--
-- The client window uses JavaScript's @eval()@ function to run the code.
runFunction :: JSFunction () -> UI ()
runFunction fun = liftJSWindow $ \w -> JS.runFunction w fun

-- | Run the given JavaScript function and wait for results. Blocks.
--
-- The client window uses JavaScript's @eval()@ function to run the code.
callFunction :: JSFunction a -> UI a
callFunction fun = liftJSWindow $ \w -> JS.callFunction w fun

-- | Export the given Haskell function so that it can be called
-- from JavaScript code.
--
-- FIXME: At the moment, the function is not garbage collected.
ffiExport :: JS.IsHandler a => a -> UI JSObject
ffiExport fun = liftJSWindow $ \w -> do
    handlerPtr <- JS.exportHandler w fun
    Foreign.addReachable (JS.root w) handlerPtr
    return handlerPtr

-- | Print a message on the client console if the client has debugging enabled.
debug :: String -> UI ()
debug s = liftJSWindow $ \w -> JS.debug w s

-- | Print a timestamp and the difference to the previous timestamp
-- on the client console if the client has debugging enabled.
timestamp :: UI ()
timestamp = liftJSWindow JS.timestamp

