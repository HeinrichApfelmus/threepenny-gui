{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
module Graphics.UI.Threepenny.Core (
    -- * Synopsis
    -- | Core functionality of the Threepenny GUI library.
    
    -- * Server
    -- $server
    Config(..), defaultConfig, startGUI,
    loadFile, loadDirectory,
    
    -- * UI monad
    -- $ui
    UI, withWindow,
    module Control.Monad.IO.Class,
    
    -- * Browser Window
    Window, title, cookies, getRequestLocation,
    
    -- * DOM elements
    -- | Create and manipulate DOM elements.
    Element, mkElement, getWindow, delete, (#+), string,
        getHead, getBody,
        children, text, html, attr, style, value,
    getValuesList,
    getElementsByTagName, getElementById, getElementsByClassName,
    
    -- * Layout
    -- | Combinators for quickly creating layouts.
    -- They can be adjusted with CSS later on.
    grid, row, column,
    
    -- * Events
    -- | For a list of predefined events, see "Graphics.UI.Threepenny.Events".
    EventData(..), domEvent, on, disconnect,
    module Reactive.Threepenny,
    
    -- * Attributes
    -- | For a list of predefined attributes, see "Graphics.UI.Threepenny.Attributes".
    (#), (#.),
    Attr, WriteAttr, ReadAttr, ReadWriteAttr(..),
    set, sink, get, mkReadWriteAttr, mkWriteAttr, mkReadAttr,
    
    -- * Widgets
    Widget(..), element, widget,
    
    -- * JavaScript FFI
    -- | Direct interface to JavaScript in the browser window.
    debug,
    ToJS, FFI, ffi, JSFunction, runFunction, callFunction,
    callDeferredFunction, atomic,
    
    -- * Internal and oddball functions
    fromProp, toElement,
    audioPlay, audioStop,
    
    ) where

import Data.Dynamic
import Data.IORef
import qualified Data.Map as Map
import Data.Maybe (listToMaybe)
import Data.Functor
import Data.String (fromString)

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.Reader as Reader

import Network.URI
import Text.JSON
import Reactive.Threepenny

import qualified Graphics.UI.Threepenny.Internal.Driver  as Core
import Graphics.UI.Threepenny.Internal.Driver
    ( getRequestLocation
    , callDeferredFunction, atomic, )
import Graphics.UI.Threepenny.Internal.FFI
import Graphics.UI.Threepenny.Internal.Types as Core
    ( Window, Config, defaultConfig, Events, EventData
    , ElementData(..), withElementData,)


import Graphics.UI.Threepenny.Internal.Types as Core
    (unprotectedGetElementId, withElementData, ElementData(..))


type ReaderT = Reader.ReaderT

{-----------------------------------------------------------------------------
    UI monad
------------------------------------------------------------------------------}
{- $ui

User interface elements are created and manipulated in the 'UI' monad.

This monad is essentially just a thin wrapper around the familiar 'IO' monad.
Use the 'liftIO' function to access 'IO' operations like reading
and writing from files.

There are several subtle reasons why Threepenny
uses a custom 'UI' monad instead of the standard 'IO' monad:

* More convenience when calling JavaScript.

* Recursion for functional reactive programming.

-}

newtype UI a = UI { unUI :: ReaderT Window IO a }

instance Functor UI where
    fmap f = UI . fmap f . unUI

instance Monad UI where
    return  = UI . return
    m >>= k = UI $ unUI m >>= unUI . k

instance MonadIO UI where
    liftIO = UI . liftIO

instance MonadFix UI where
    mfix f = UI $ mfix (unUI . f)  

-- | Execute an 'UI' action in a particular browser window.
withWindow :: Window -> UI a -> IO a
withWindow w m = Reader.runReaderT (unUI m) w

getWindowUI :: UI Window
getWindowUI = UI Reader.ask

{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
{- $server

To display the user interface, you have to start a server using 'startGUI'.
Then, visit the URL <http://localhost:10000/> in your browser
(assuming that you have set the port number to @tpPort=10000@
in the server configuration).

The server is multithreaded,
a separate thread is used to communicate with a single browser 'Window'.
However, each window should only be accessed from a single thread,
otherwise the behavior will be undefined,
i.e. you could run an element search and get a click event as a result
if you don't access each window in a single-threaded fashion.

-}

-- | Start server for GUI sessions.
startGUI
    :: Config               -- ^ Server configuration.
    -> (Window -> UI ())    -- ^ Action to run whenever a client browser connects.
    -> IO ()
startGUI config handler = Core.serve config (\w -> withWindow w $ handler w)


-- | Make a local file available as a relative URI.
loadFile
    :: String     -- ^ MIME type
    -> FilePath   -- ^ Local path to the file
    -> UI String  -- ^ Generated URI
loadFile mime path = getWindowUI >>= \w -> liftIO $
    Core.loadFile w (fromString mime) path

-- | Make a local directory available as a relative URI.
loadDirectory :: FilePath -> UI String
loadDirectory path = getWindowUI >>= \w -> liftIO $
    Core.loadDirectory w path

{-----------------------------------------------------------------------------
    Browser window
------------------------------------------------------------------------------}
-- | Title of the client window.
title :: WriteAttr Window String
title = mkWriteAttr $ \s _ ->
    runFunction $ ffi "document.title = %1;" s

-- | Cookies on the client.
cookies :: ReadAttr Window [(String,String)]
cookies = mkReadAttr (liftIO . Core.getRequestCookies)

{-----------------------------------------------------------------------------
    Elements
------------------------------------------------------------------------------}
data Element = Element { eEvents :: Core.Events, toElement :: Core.Element }

fromElement :: Core.Element -> IO Element
fromElement e = do
    events <- Core.withElementData e $ \_ x -> return $ elEvents x 
    return $ Element events e

instance ToJS Element where
    render = render . toElement

-- | Make a new DOM element.
mkElement
    :: String           -- ^ Tag name
    -> UI Element
mkElement tag = mdo
    -- create events and initialize them when element becomes Alive
    let initializeEvent (name,_,handler) = Core.bind name el handler
    events  <- liftIO $ newEventsNamed initializeEvent
    
    window  <- getWindowUI
    el      <- liftIO $ Core.newElement window tag events
    return $ Element events el

-- | Retrieve the browser 'Window' in which the element resides.
getWindow :: Element -> IO Window
getWindow e = Core.getWindow (toElement e)

-- | Delete the given element.
delete :: Element -> UI ()
delete = liftIO . Core.delete . toElement

-- | Append DOM elements as children to a given element.
(#+) :: UI Element -> [UI Element] -> UI Element
(#+) mx mys = do
    x  <- mx
    ys <- sequence mys
    liftIO $ mapM_ (Core.appendElementTo (toElement x) . toElement) ys
    return x

-- | Child elements of a given element.
children :: WriteAttr Element [Element]
children = mkWriteAttr set
    where
    set xs x = liftIO $ do
        Core.emptyEl $ toElement x
        mapM_ (Core.appendElementTo (toElement x) . toElement) xs

-- | Child elements of a given element as a HTML string.
html :: WriteAttr Element String
html = mkWriteAttr $ \s el ->
    runFunction $ ffi "$(%1).html(%2)" el s

-- | HTML attributes of an element.
attr :: String -> WriteAttr Element String
attr name = mkWriteAttr $ \s el ->
    runFunction $ ffi "$(%1).attr(%2,%3)" el name s

-- | Set CSS style of an Element
style :: WriteAttr Element [(String,String)]
style = mkWriteAttr $ \xs el -> forM_ xs $ \(name,val) -> 
    runFunction $ ffi "%1.style[%2] = %3" el name val

-- | Value attribute of an element.
-- Particularly relevant for control widgets like 'input'.
value :: Attr Element String
value = mkReadWriteAttr get set
    where
    get   el = callFunction $ ffi "$(%1).val()" el
    set v el = runFunction  $ ffi "$(%1).val(%2)" el v

-- | Get values from inputs. Blocks. This is faster than many 'getValue' invocations.
getValuesList
    :: [Element]   -- ^ A list of elements to get the values of.
    -> UI [String] -- ^ The list of plain text values.
getValuesList = mapM (get value)
    -- TODO: improve this to use Core.getValuesList

-- | Text content of an element.
text :: WriteAttr Element String
text = mkWriteAttr $ \s el ->
    runFunction $ ffi "$(%1).text(%2)" el s

-- | Make a @span@ element with a given text content.
string :: String -> UI Element
string s = mkElement "span" # set text s


-- | Get the head of the page.
getHead :: Window -> UI Element
getHead w = liftIO $ fromElement =<< Core.getHead w

-- | Get the body of the page.
getBody :: Window -> UI Element
getBody w = liftIO $ fromElement =<< Core.getBody w

-- | Get all elements of the given tag name.  Blocks.
getElementsByTagName
    :: Window        -- ^ Browser window
    -> String        -- ^ The tag name.
    -> UI [Element]  -- ^ All elements with that tag name.
getElementsByTagName window name = liftIO $
    mapM fromElement =<< Core.getElementsByTagName window name

-- | Get an element by a particular ID.  Blocks.
getElementById
    :: Window              -- ^ Browser window
    -> String              -- ^ The ID string.
    -> UI (Maybe Element)  -- ^ Element (if any) with given ID.
getElementById window id = liftIO $
    fmap listToMaybe $ mapM fromElement =<< Core.getElementsById window [id]

-- | Get a list of elements by particular class.  Blocks.
getElementsByClassName
    :: Window        -- ^ Browser window
    -> String        -- ^ The class string.
    -> UI [Element]  -- ^ Elements with given class.
getElementsByClassName window cls = liftIO $
    mapM fromElement =<< Core.getElementsByClassName window cls


{-----------------------------------------------------------------------------
    FFI
------------------------------------------------------------------------------}
-- | Run the given JavaScript function and carry on. Doesn't block.
--
-- The client window uses JavaScript's @eval()@ function to run the code.
runFunction :: JSFunction () -> UI ()
runFunction fun = do
    window <- getWindowUI
    liftIO $ Core.runFunction window fun 

-- | Run the given JavaScript function and wait for results. Blocks.
--
-- The client window uses JavaScript's @eval()@ function to run the code.
callFunction :: JSFunction a -> UI a
callFunction fun = do
    window <- getWindowUI
    liftIO $ Core.callFunction window fun


{-----------------------------------------------------------------------------
    Oddball
------------------------------------------------------------------------------}
-- | Print a message on the client console if the client has debugging enabled.
debug :: String -> UI ()
debug s = getWindowUI >>= \w -> liftIO $ Core.debug w s

-- | Invoke the JavaScript expression @audioElement.play();@.
audioPlay :: Element -> UI ()
audioPlay el = runFunction $ ffi "%1.play()" el

-- | Invoke the JavaScript expression @audioElement.stop();@.
audioStop :: Element -> UI ()
audioStop el = runFunction $ ffi "prim_audio_stop(%1)" el

-- Turn a jQuery property @.prop()@ into an attribute.
fromProp :: String -> (JSValue -> a) -> (a -> JSValue) -> Attr Element a
fromProp name from to = mkReadWriteAttr get set
    where
    set v el = runFunction $ ffi "$(%1).prop(%2,%3)" el name (to v)
    get   el = fmap from $ callFunction $ ffi "$(%1).prop(%2)" el name

{-----------------------------------------------------------------------------
    Layout
------------------------------------------------------------------------------}
-- | Align given elements in a row. Special case of 'grid'.
row :: [UI Element] -> UI Element
row xs = grid [xs]

-- | Align given elements in a column. Special case of 'grid'.
column :: [UI Element] -> UI Element
column = grid . map (:[])

-- | Align given elements in a rectangular grid.
--
-- Layout is achieved by using the CSS @display:table@ property.
-- The following element tree will be generated
--
-- >  <div class="table">
-- >    <div class="table-row">
-- >      <div class="table-cell"> ... </div>
-- >      <div class="table-cell"> ... </div>
-- >    </div>
-- >    <div class="table-row">
-- >      ...
-- >    </div>
-- >   ...
-- >   </div>
--
-- You can customatize the actual layout by assigning an @id@ to the element
-- and changing the @.table@, @.table-row@ and @table-column@
-- classes in a custom CSS file.
grid    :: [[UI Element]] -> UI Element
grid mrows = do
        rows0 <- mapM (sequence) mrows
    
        rows  <- forM rows0 $ \row0 -> do
            row <- forM row0 $ \entry ->
                wrap "table-cell" [entry]
            wrap "table-row" row
        wrap "table" rows

    where
    wrap c xs = mkElement "div" # set (attr "class") c #+ map element xs

{-----------------------------------------------------------------------------
    Events
------------------------------------------------------------------------------}
-- | Obtain DOM event for a given element.
domEvent
    :: String
        -- ^ Event name. A full list can be found at
        --   <http://www.w3schools.com/jsref/dom_obj_event.asp>.
        --   Note that the @on@-prefix is not included,
        --   the name is @click@ and so on.
    -> Element          -- ^ Element where the event is to occur.
    -> Event EventData
domEvent name (Element events _) = events name


-- | Event that occurs whenever the client has disconnected,
-- be it by closing the browser window or by exception.
--
-- Note: DOM Elements in the browser window that has been closed
-- can no longer be manipulated.
disconnect :: Window -> Event ()
disconnect = Core.disconnect

-- | Convenience function to register 'Event's for 'Element's.
--
-- Example usage.
--
-- > on click element $ \_ -> ...
on :: (element -> Event a) -> element -> (a -> UI void) -> UI ()
on f x h = do
    window <- getWindowUI
    liftIO $ register (f x) (void . withWindow window . h)
    return ()


{-----------------------------------------------------------------------------
    Attributes
------------------------------------------------------------------------------}
infixl 8 #
infixl 8 #+
infixl 8 #.

-- | Reverse function application.
-- Allows convenient notation for setting properties.
--
-- Example usage.
--
-- > mkElement "div"
-- >     # set style     [("color","#CCAABB")]
-- >     # set draggable True
-- >     # set children  otherElements
(#) :: a -> (a -> b) -> b
(#) = flip ($)

-- | Convenient combinator for setting the CSS class on element creation.
(#.) :: UI Element -> String -> UI Element
(#.) mx s = mx # set (attr "class") s

-- | Attributes can be 'set' and 'get'.
type Attr x a = ReadWriteAttr x a a

-- | Attribute that only supports the 'get' operation.
type ReadAttr x o = ReadWriteAttr x () o

-- | Attribute that only supports the 'set' operation.
type WriteAttr x i = ReadWriteAttr x i ()

-- | Generalized attribute with different types for getting and setting.
data ReadWriteAttr x i o = ReadWriteAttr
    { get' :: x -> UI o
    , set' :: i -> x -> UI ()
    }

-- | Set value of an attribute in the 'UI' monad.
-- Best used in conjunction with '#'.
set :: ReadWriteAttr x i o -> i -> UI x -> UI x
set attr i mx = do { x <- mx; set' attr i x; return x; }

-- | Set the value of an attribute to a 'Behavior', that is a time-varying value.
--
-- Note: For reasons of efficiency, the attribute is only
-- updated when the value changes.
sink :: ReadWriteAttr x i o -> Behavior i -> UI x -> UI x
sink attr bi mx = do
    x <- mx
    do
        i <- liftIO $ currentValue bi
        set' attr i x
        window <- getWindowUI
        liftIO $ onChange bi $ \i -> withWindow window $ set' attr i x  
    return x

-- | Get attribute value.
get :: ReadWriteAttr x i o -> x -> UI o
get attr = get' attr

-- | Build an attribute from a getter and a setter.
mkReadWriteAttr
    :: (x -> UI o)          -- ^ Getter.
    -> (i -> x -> UI ())    -- ^ Setter.
    -> ReadWriteAttr x i o
mkReadWriteAttr get set = ReadWriteAttr { get' = get, set' = set }

-- | Build attribute from a getter.
mkReadAttr :: (x -> UI o) -> ReadAttr x o
mkReadAttr get = mkReadWriteAttr get (\_ _ -> return ())

-- | Build attribute from a setter.
mkWriteAttr :: (i -> x -> UI ()) -> WriteAttr x i
mkWriteAttr set = mkReadWriteAttr (\_ -> return ()) set


{-----------------------------------------------------------------------------
    Widget class
------------------------------------------------------------------------------}
-- | Widgets are data types that have a visual representation.
class Widget w where
    getElement :: w -> Element

instance Widget Element where
    getElement = id


-- | Convience synonym for 'return' to make elements work well with 'set'.
-- Also works on 'Widget's.
--
-- Example usage.
--
-- > e <- mkElement "button"
-- > element e # set text "Ok"
element :: MonadIO m => Widget w => w -> m Element
element = return . getElement

-- | Convience synonym for 'return' to make widgets work well with 'set'.
widget  :: Widget w => w -> UI w
widget  = return

