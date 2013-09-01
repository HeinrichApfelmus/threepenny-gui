{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Graphics.UI.Threepenny.Core (
    -- * Guide
    -- $guide
    
    -- * Server
    -- $server
    Config(..), startGUI,
    loadFile, loadDirectory,
    
    -- * Browser Window
    Window, title, cookies, getRequestLocation,
    
    -- * DOM elements
    -- | Create and manipulate DOM elements.
    Element, mkElement, getWindow, delete, (#+), string,
        getHead, getBody,
        children, text, html, attr, style, value,
    getValuesList,
    getElementsByTagName, getElementByTagName, 
    getElementsById, getElementById,
    getElementsByClassName,
    
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
    (#), (#.), element,
    Attr, WriteAttr, ReadAttr, ReadWriteAttr(..),
    set, sink, get, mkReadWriteAttr, mkWriteAttr, mkReadAttr,
    
    -- * JavaScript FFI
    -- | Direct interface to JavaScript in the browser window.
    debug, clear,
    ToJS, FFI, ffi, JSFunction, runFunction, callFunction,
    callDeferredFunction, atomic,
    
    -- * Internal and oddball functions
    updateElement, manifestElement, fromProp,
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
import Control.Monad.IO.Class
import Network.URI
import Text.JSON
import Reactive.Threepenny

import qualified Graphics.UI.Threepenny.Internal.Core  as Core
import Graphics.UI.Threepenny.Internal.Core
    (getRequestLocation,
     ToJS, FFI, ffi, JSFunction,
     debug, clear, callFunction, runFunction, callDeferredFunction, atomic, )
import qualified Graphics.UI.Threepenny.Internal.Types as Core
import Graphics.UI.Threepenny.Internal.Types (Window, Config, EventData, Session(..))

{-----------------------------------------------------------------------------
    Guide
------------------------------------------------------------------------------}
{- $guide

Threepenny runs a small web server that displays the user interface
as a web page to any browser that connects to it.
To start the web server, use the 'startGUI' function.

Creating of DOM elements is easy,
the '(#+)' combinator allows a style similar to HTML combinator libraries.

Existing DOM elements can be accessed much in the same way they are
accessed from JavaScript; they can be searched, updated, moved and
inspected. Events can be bound to DOM elements and handled.


Applications written in Threepenny are multithreaded. Each client (user)
has a separate thread which runs with no awareness of the asynchronous
protocol below. Each session should only be accessed from one
thread. There is not yet any clever architecture for accessing the
(single threaded) web browser from multi-threaded Haskell. That's
my recommendation. You can choose to ignore it, but don't blame me
when you run an element search and you get a click event as a
result.

This project was originally called Ji.

-}


{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
{- $server

To display the user interface, you have to start a server using 'startGUI'.
Then, visit the URL <http://localhost:10000/> in your browser
(assuming that you have set the port number to @tpPort=10000@
in the server configuration).

-}

-- | Start server for GUI sessions.
startGUI
    :: Config               -- ^ Server configuration.
    -> (Window -> IO ())    -- ^ Action to run whenever a client browser connects.
    -> IO ()
startGUI config handler = Core.serve config handler


-- | Make a local file available as a relative URI.
loadFile
    :: Window     -- ^ Browser window
    -> String     -- ^ MIME type
    -> FilePath   -- ^ Local path to the file
    -> IO String  -- ^ Generated URI
loadFile w mime path = Core.loadFile w (fromString mime) path

-- | Make a local directory available as a relative URI.
loadDirectory :: Window -> FilePath -> IO String
loadDirectory = Core.loadDirectory

{-----------------------------------------------------------------------------
    Browser window
------------------------------------------------------------------------------}
-- | Title of the client window.
title :: WriteAttr Window String
title = mkWriteAttr Core.setTitle

-- | Cookies on the client.
cookies :: ReadAttr Window [(String,String)]
cookies = mkReadAttr Core.getRequestCookies

{-----------------------------------------------------------------------------
    Elements
------------------------------------------------------------------------------}
type Value = String

-- | Reference to an element in the DOM of the client window.
data Element = Element Core.ElementEvents (MVar Elem) deriving (Typeable)
-- Element events mvar
--      events = Events associated to this element
--      mvar   = Current state of the MVar
data    Elem
    = Alive Core.Element                       -- element exists in a window
    | Limbo Value (Window -> IO Core.Element)  -- still needs to be created

-- Turn a live reference into an 'Element'.
-- Note that multiple MVars may now point to the same live reference,
-- but this is ok since live references never change.
fromAlive :: Core.Element -> IO Element
fromAlive e@(Core.Element elid Session{..}) = do
    Just events <- Map.lookup elid <$> readMVar sElementEvents
    Element events <$> newMVar (Alive e)

-- Update an element that may be in Limbo.
updateElement :: (Core.Element -> IO ()) -> Element -> IO ()
updateElement f (Element _ me) = do
    e <- takeMVar me
    case e of
        Alive e -> do   -- update immediately
            f e
            putMVar me $ Alive e
        Limbo value create ->      -- update on creation
            putMVar me $ Limbo value $ \w -> create w >>= \e -> f e >> return e

-- Given a browser window, make sure that the element exists there.
-- TODO: 1. Throw exception if the element exists in another window.
--       2. Don't throw exception, but move the element across windows.
manifestElement :: Window -> Element -> IO Core.Element
manifestElement w (Element events me) = do
        e1 <- takeMVar me
        e2 <- case e1 of
            Alive e        -> return e
            Limbo v create -> do
                e2 <- create w
                Core.setAttr "value" v e2
                rememberEvents events e2    -- save events in session data
                return e2
        putMVar me $ Alive e2
        return e2
    
    where
    rememberEvents events (Core.Element elid Session{..}) =
        modifyMVar_ sElementEvents $ return . Map.insert elid events


-- Append a child element to a parent element. Non-blocking.
appendTo
    :: Element   -- ^ Parent.
    -> Element   -- ^ Child.
    -> IO ()
appendTo parent child = do
    flip updateElement parent $ \x -> do
        y <- manifestElement (Core.getWindow x) child
        Core.appendElementTo x y

-- | Make a new DOM element.
mkElement
    :: String           -- ^ Tag name
    -> IO Element
mkElement tag = do
    -- create element in Limbo
    ref <- newMVar (Limbo "" $ \w -> Core.newElement w tag)
    -- create events and initialize them when element becomes Alive
    let
        initializeEvent (name,_,handler) = 
            flip updateElement (Element undefined ref) $ \e -> do
                Core.bind name e handler
    events  <- newEventsNamed initializeEvent
    return $ Element events ref

-- | Retrieve the browser 'Window' in which the element resides.
-- 
-- Note that elements do not reside in any browser window when they are first created.
-- To move the element to a particular browser window,
-- you have to append it to a parent, for instance with the `(#+)` operator.
--
-- WARNING: The ability to move elements from one browser window to another
-- is currently not implemented yet.
getWindow :: Element -> IO (Maybe Window)
getWindow (Element _ ref) = do
    e1 <- readMVar ref
    return $ case e1 of
        Alive e   -> Just $ Core.getWindow e
        Limbo _ _ -> Nothing

-- | Delete the given element.
delete :: Element -> IO ()
delete = updateElement (Core.delete)

-- | Append DOM elements as children to a given element.
(#+) :: IO Element -> [IO Element] -> IO Element
(#+) mx mys = do
    x  <- mx
    ys <- sequence mys
    mapM_ (appendTo x) ys
    return x

-- | Child elements of a given element.
children :: WriteAttr Element [Element]
children = mkWriteAttr set
    where
    set xs x = do
        updateElement Core.emptyEl x
        mapM_ (appendTo x) xs

-- | Child elements of a given element as a HTML string.
html :: WriteAttr Element String
html = mkWriteAttr (updateElement . Core.setHtml)

-- | HTML attributes of an element.
attr :: String -> WriteAttr Element String
attr name = mkWriteAttr (updateElement . Core.setAttr name)

-- | Set CSS style of an Element
style :: WriteAttr Element [(String,String)]
style = mkWriteAttr (updateElement . Core.setStyle)

-- | Value attribute of an element.
-- Particularly relevant for control widgets like 'input'.
value :: Attr Element String
value = mkReadWriteAttr get set
    where
    get   (Element _ ref) = getValue =<< readMVar ref
    set v (Element _ ref) = updateMVar (setValue v) ref
    
    getValue (Limbo v _) = return v
    getValue (Alive e  ) = Core.getValue e
    
    setValue v (Limbo _ f) = return $ Limbo v f
    setValue v (Alive e  ) = Core.setAttr "value" v e >> return (Alive e)
    
    updateMVar f ref = do
        x <- takeMVar ref
        y <- f x
        putMVar ref y

-- | Get values from inputs. Blocks. This is faster than many 'getValue' invocations.
getValuesList
    :: [Element]   -- ^ A list of elements to get the values of.
    -> IO [String] -- ^ The list of plain text values.
getValuesList = mapM (get value)
    -- TODO: improve this to use Core.getValuesList

-- | Text content of an element.
text :: WriteAttr Element String
text = mkWriteAttr (updateElement . Core.setText)

-- | Make a @span@ element with a given text content.
string :: String -> IO Element
string s = mkElement "span" # set text s


-- | Get the head of the page.
getHead :: Window -> IO Element
getHead = fromAlive <=< Core.getHead

-- | Get the body of the page.
getBody :: Window -> IO Element
getBody = fromAlive <=< Core.getBody

-- | Get an element by its tag name.  Blocks.
getElementByTagName
    :: Window             -- ^ Browser window
    -> String             -- ^ The tag name.
    -> IO (Maybe Element) -- ^ An element (if any) with that tag name.
getElementByTagName window = liftM listToMaybe . getElementsByTagName window

-- | Get all elements of the given tag name.  Blocks.
getElementsByTagName
    :: Window        -- ^ Browser window
    -> String        -- ^ The tag name.
    -> IO [Element]  -- ^ All elements with that tag name.
getElementsByTagName window name =
    mapM fromAlive =<< Core.getElementsByTagName window name

-- | Get an element by a particular ID.  Blocks.
getElementById
    :: Window              -- ^ Browser window
    -> String              -- ^ The ID string.
    -> IO (Maybe Element)  -- ^ Element (if any) with given ID.
getElementById window id = listToMaybe `fmap` getElementsById window [id]

-- | Get a list of elements by particular IDs.  Blocks.
getElementsById
    :: Window        -- ^ Browser window
    -> [String]      -- ^ The ID string.
    -> IO [Element]  -- ^ Elements with given ID.
getElementsById window name =
    mapM fromAlive =<< Core.getElementsById window name

-- | Get a list of elements by particular class.  Blocks.
getElementsByClassName
    :: Window        -- ^ Browser window
    -> String        -- ^ The class string.
    -> IO [Element]  -- ^ Elements with given class.
getElementsByClassName window cls =
    mapM fromAlive =<< Core.getElementsByClassName window cls

{-----------------------------------------------------------------------------
    Oddball
------------------------------------------------------------------------------}
-- | Invoke the JavaScript expression @audioElement.play();@.
audioPlay = updateElement $ \el -> Core.runFunction (Core.getWindow el) $
    ffi "%1.play()" el

-- | Invoke the JavaScript expression @audioElement.stop();@.
audioStop = updateElement $ \el -> Core.runFunction (Core.getWindow el) $
    ffi "prim_audio_stop(%1)" el

-- Turn a jQuery property @.prop()@ into an attribute.
fromProp :: String -> (JSValue -> a) -> (a -> JSValue) -> Attr Element a
fromProp name from to = mkReadWriteAttr get set
    where
    set x = updateElement (Core.setProp name $ to x)
    get (Element _ ref) = do
        me <- readMVar ref
        case me of
            Limbo _ _ -> error "'checked' attribute: element must be in a browser window"
            Alive e   -> from <$> Core.getProp name e

{-----------------------------------------------------------------------------
    Layout
------------------------------------------------------------------------------}
-- | Align given elements in a row. Special case of 'grid'.
row :: [IO Element] -> IO Element
row xs = grid [xs]

-- | Align given elements in a column. Special case of 'grid'.
column :: [IO Element] -> IO Element
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
grid    :: [[IO Element]] -> IO Element
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

{-   
    ref <- newIORef $ return ()
    let
        -- register handler and remember unregister function
        register' = flip updateElement element $ \e -> do
            unregister <- register (Core.bind name e) handler
            writeIORef ref unregister
        
        -- update element to unregister the event handler
        unregister' = flip updateElement element $ \_ -> do
            join $ readIORef ref
    
    register'
    return unregister'
-}

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
on :: (element -> Event a) -> element -> (a -> IO void) -> IO ()
on f x h = register (f x) (void . h) >> return ()


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
(#.) :: IO Element -> String -> IO Element
(#.) mx s = mx # set (attr "class") s


-- | Convience synonym for 'return' to make elements work well with 'set'.
--
-- Example usage.
--
-- > e <- mkElement "button"
-- > element e # set text "Ok"
element :: Element -> IO Element
element = return


-- | Attributes can be 'set' and 'get'.
type Attr x a = ReadWriteAttr x a a

-- | Attribute that only supports the 'get' operation.
type ReadAttr x o = ReadWriteAttr x () o

-- | Attribute that only supports the 'set' operation.
type WriteAttr x i = ReadWriteAttr x i ()

-- | Generalized attribute with different types for getting and setting.
data ReadWriteAttr x i o = ReadWriteAttr
    { get' :: x -> IO o
    , set' :: i -> x -> IO ()
    }

-- | Set value of an attribute in the 'IO' monad.
-- Best used in conjunction with '#'.
set :: MonadIO m => ReadWriteAttr x i o -> i -> m x -> m x
set attr i mx = do { x <- mx; liftIO (set' attr i x); return x; }

-- | Set the value of an attribute to a 'Behavior', that is a time-varying value.
--
-- Note: For reasons of efficiency, the attribute is only
-- updated when the value changes.
sink :: ReadWriteAttr x i o -> Behavior i -> IO x -> IO x
sink attr bi mx = do
    x <- mx
    do
        i <- currentValue bi
        set' attr i x
        onChange bi $ \i -> set' attr i x  
    return x

-- | Get attribute value.
get :: ReadWriteAttr x i o -> x -> IO o
get = get'

-- | Build an attribute from a getter and a setter.
mkReadWriteAttr
    :: (x -> IO o)          -- ^ Getter.
    -> (i -> x -> IO ())    -- ^ Setter.
    -> ReadWriteAttr x i o
mkReadWriteAttr get set = ReadWriteAttr { get' = get, set' = set }

-- | Build attribute from a getter.
mkReadAttr :: (x -> IO o) -> ReadAttr x o
mkReadAttr get = mkReadWriteAttr get (\_ _ -> return ())

-- | Build attribute from a setter.
mkWriteAttr :: (i -> x -> IO ()) -> WriteAttr x i
mkWriteAttr set = mkReadWriteAttr (\_ -> return ()) set

