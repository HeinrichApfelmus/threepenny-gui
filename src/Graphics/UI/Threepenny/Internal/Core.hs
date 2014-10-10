{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.UI.Threepenny.Internal.Core (
    -- * Synopsis
    -- | Internal core:
    -- 'UI' monad, integrating FRP and JavaScript FFI. garbage collection
   
    -- * Documentation
    UI, runUI, liftIOLater, askWindow,
    
    Window, FFI, ToJS, JSFunction, JSObject, ffi,
    runFunction, callFunction, ffiExport, debug,
    HsEvent,
    
    Element, fromJSObject, getWindow,
    mkElement, delete, appendChild, clearChildren,
    
    EventData, domEvent,
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

import Foreign.JavaScript hiding (runFunction, callFunction, debug)

{-----------------------------------------------------------------------------
    Elements
------------------------------------------------------------------------------}
type Events = String -> E.Event [JSON.Value]

data Element = Element
    { eEvents    :: Events      -- FRP event mapping
    , toJSObject :: JS.JSObject -- corresponding JavaScript object
    , eWindow    :: Window      -- Window in which the element was created
    }

instance ToJS Element where
    render = render . toJSObject

-- Convert JavaScript object into an element
-- FIXME: Add events and current window.
fromJSObject :: JS.JSObject -> UI Element
fromJSObject e = do
    w <- askWindow
    liftIO $ Foreign.addReachable (JS.root w) e
    return $ Element (error "Not implemented!") e w

getWindow :: Element -> IO Window
getWindow = return . eWindow


type EventData = [String]

-- | Obtain DOM event for a given element.
domEvent
    :: String
        -- ^ Event name. A full list can be found at
        --   <http://www.w3schools.com/jsref/dom_obj_event.asp>.
        --   Note that the @on@-prefix is not included,
        --   the name is @click@ and so on.
    -> Element          -- ^ Element where the event is to occur.
    -> E.Event EventData
domEvent name el = fmap (fromSuccess . JSON.fromJSON . head) $ eEvents el name
    where
    fromSuccess (JSON.Success x) = x

-- | Make a new DOM element with a given tag name
mkElement :: String -> UI Element
mkElement tag = liftWindow $ \w -> do
    el <- JS.callFunction w $ ffi "document.createElement(%1)" tag

    -- Lazily create FRP events whenever they are needed.
    let initializeEvent (name,_,handler) = do
            handlerPtr <- JS.exportHandler handler w
            -- make handler reachable from element
            Foreign.addReachable el handlerPtr
            JS.runFunction w $
                ffi "Haskell.bind(%1,%2,%3)" el name handlerPtr

    events <- E.newEventsNamed initializeEvent
    
    -- FIXME: Add support for JavaScript functions that /return/ elements.
    return $ Element events el w

-- | Delete the given element.
delete :: Element -> UI ()
delete el = liftWindow $ \w -> do
    JS.runFunction w $ ffi "$(%1).detach()" el
    Foreign.destroy $ toJSObject el

-- | Remove all child elements.
clearChildren :: Element -> UI ()
clearChildren (Element _ el _) = liftWindow $ \w -> do
    Foreign.withRemotePtr el $ \_ _ -> do
        Foreign.clearReachable el
        JS.runFunction w $ ffi "$(%1).contents().detach()" el

-- | Append a child element.
appendChild :: Element -> Element -> UI ()
appendChild (Element _ eParent _) (Element _ eChild _) = liftWindow $ \w -> do
    -- FIXME: We have to stop the child being reachable from its
    -- /previous/ parent.
    Foreign.addReachable eParent eChild
    JS.runFunction w $ ffi "$(%1).append($(%2))" eParent eChild


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

liftWindow :: (Window -> IO a) -> UI a
liftWindow f = askWindow >>= liftIO . f

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
runFunction fun = liftWindow $ \w -> JS.runFunction w fun 

-- | Run the given JavaScript function and wait for results. Blocks.
--
-- The client window uses JavaScript's @eval()@ function to run the code.
callFunction :: JSFunction a -> UI a
callFunction fun = liftWindow $ \w -> JS.callFunction w fun

-- | Export the given Haskell function so that it can be called
-- from JavaScript code.
--
-- TODO: At the moment, the function is not garbage collected.
ffiExport :: IO () -> UI HsEvent
ffiExport fun = liftWindow $ \w -> do
    handlerPtr <- JS.exportHandler (const fun) w
    Foreign.addReachable (JS.root w) handlerPtr
    return handlerPtr

-- | Print a message on the client console if the client has debugging enabled.
debug :: String -> UI ()
debug s = liftWindow $ JS.debug s
