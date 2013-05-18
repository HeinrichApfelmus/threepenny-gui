module Graphics.UI.Threepenny.Core (
    -- * Guide
    -- $guide
    
    -- * Server
    -- $server
    Config(..), startGUI,
    loadFile, loadDirectory,
    
    -- * Manipulate DOM elements
    Window, title, getHead, getBody, getWindow, cookies, getRequestLocation,
    Element, newElement, delete, appendTo,
        children, text, html, attr, value,
        getValuesList,
    getElementsByTagName, getElementByTagName, getElementById,
    
    -- * Create DOM elements
    Dom, withWindow, mkElement, string, (#+),
    
    -- * Layout
    -- | Combinators for quickly creating layouts.
    -- They can be adjusted with CSS later on.
    grid, row, column,
    
    -- * Events
    -- | For a list of predefined events, see "Graphics.UI.Threepenny.Events".
    EventData(..), domEvent, on,
    module Control.Event,
    
    -- * Attributes
    -- | For a list of predefined attributes, see "Graphics.UI.Threepenny.Attributes".
    (#), (#.), element,
    Attr, WriteAttr, ReadAttr, ReadWriteAttr(..),
    set, get, mkReadWriteAttr, mkWriteAttr, mkReadAttr,
    
    -- * Utilities
    -- | A few raw JavaScript utilities.
    debug, clear,
    callFunction, runFunction, callDeferredFunction,
    atomic,
    
    ) where

import Control.Event
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader as Reader

import Network.URI

import qualified Graphics.UI.Threepenny.Internal.Core  as Core
import Graphics.UI.Threepenny.Internal.Core
    (getHead, getBody, getRequestLocation, delete, getValuesList,
     getElementById, getElementsByTagName, getElementByTagName,
     debug, clear, callFunction, runFunction, callDeferredFunction,
     atomic, newElement, )
import Graphics.UI.Threepenny.Internal.Types as Core


{-----------------------------------------------------------------------------
    Guide
------------------------------------------------------------------------------}
{- $guide

Threepenny runs a small web server that displays the user interface
as a web page to any browser that connects to it.
To start the web server, use the 'startGUI' function.

Existing DOM elements can be accessed much in the same way they are
accessed from JavaScript; they can be searched, updated, moved and
inspected. Events can be bound to DOM elements and handled.

Creating DOM elements can be done by hand, but it is more convenient
to use the 'Dom' monad which offers functionality similar
to HTML combinator libaries.


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
startGUI config handler =
    Core.serve config $ \w -> handler w >> Core.handleEvents w


-- | Make a local file available as a relative URI.
loadFile :: Window -> FilePath -> IO String
loadFile = flip Core.loadFile

-- | Make a local directory available as a relative URI.
loadDirectory :: Window -> FilePath -> IO String
loadDirectory = flip Core.loadDirectory

{-----------------------------------------------------------------------------
    Manipulate DOM
------------------------------------------------------------------------------}
-- | Retrieve 'Window' that a given 'Element' resides in.
getWindow :: Element -> Window
getWindow = elSession

-- | Title of the client window.
title :: WriteAttr Window String
title = mkWriteAttr Core.setTitle

-- | Cookies on the client.
cookies :: ReadAttr Window [(String,String)]
cookies = mkReadAttr Core.getRequestCookies

-- | Child elements of a given element.
children :: WriteAttr Element [Element]
children = mkWriteAttr set
    where
    set xs x = do
        Core.emptyEl x
        mapM_ (Core.appendElementTo x) xs

-- | Append a child element to a parent element. Non-blocking.
appendTo :: MonadIO m
    => Element     -- ^ Parent.
    -> m Element   -- ^ Child.
    -> m Element   -- ^ Returns a reference to the child element again.
appendTo x my = do { y <- my; liftIO $ Core.appendElementTo x y; }

-- | Child elements of a given element as a HTML string.
html :: WriteAttr Element String
html = mkWriteAttr (\i x -> Core.setHtml i x # void)

-- | HTML attributes of an element.
attr :: String -> WriteAttr Element String
attr name = mkWriteAttr (\i x -> Core.setAttr name i x # void)

-- | Value attribute of an element.
-- Particularly relevant for control widgets like 'input'.
value :: Attr Element String
value = mkReadWriteAttr Core.getValue (set' $ attr "value")

-- | Text content of an element.
text :: WriteAttr Element String
text = mkWriteAttr (\i x -> Core.setText i x # void)

{-----------------------------------------------------------------------------
    Create DOM
------------------------------------------------------------------------------}
-- | Monad for creating 'Element' in a specific 'Window'
type Dom = ReaderT Window IO

-- | Build elements in a particular window
withWindow :: Window -> Dom a -> IO a
withWindow w m = runReaderT m w

-- | Make a new DOM element.
mkElement
    :: String           -- ^ Tag name
    -> Dom Element
mkElement tag = ReaderT $ \w -> Core.newElement w tag

-- | Make a @span@ element with a given text content.
string :: String -> Dom Element
string s = mkElement "span" # set text s

-- | Append dom elements as children to a given element.
(#+) :: MonadIO m => m Element -> [Dom Element] -> m Element
(#+) mx mys = do
    x  <- mx
    ys <- liftIO $ withWindow (elSession x) $ sequence mys
    liftIO $ mapM_ (Core.appendElementTo x) ys
    return x

{-----------------------------------------------------------------------------
    Layout
------------------------------------------------------------------------------}
-- | Align given elements in a row. Special case of 'grid'.
row :: [Dom Element] -> Dom Element
row xs = grid [xs]

-- | Align given elements in a column. Special case of 'grid'.
column :: [Dom Element] -> Dom Element
column = grid . map (:[])

-- | Align given elements in a rectangular grid.
--
-- Layout is achieved by using the CSS @display:table@ property.
-- The following element tree will be generated
--
-- >  <div class="table">
-- >    <div class="table-row">
-- >      <div class="table-cell> ... </div>
-- >      <div class="table-cell> ... </div>
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
grid    :: [[Dom Element]] -> Dom Element
grid mrows = do
        rows0 <- mapM (sequence) mrows
    
        rows  <- forM rows0 $ \row0 -> do
            row <- forM row0 $ \entry ->
                wrap "table-cell" [entry]
            wrap "table-row" row
        wrap "table" rows

    where
    wrap c xs =
        mkElement "div"
            # set (attr "class") c
            # set children xs

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
domEvent = Core.bind

-- | Convenience function to register 'Event's for 'Element's.
--
-- Example:
--
-- > on click element $ \_ -> ...
on :: MonadIO m => (element -> Event a) -> element -> (a -> IO void) -> m ()
on f x h = liftIO $ register (f x) (void . h) >> return ()


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
-- > newElement window "div"
-- >     # set style     [("color","#CCAABB")]
-- >     # set draggable True
-- >     # set children  otherElements
(#) :: a -> (a -> b) -> b
(#) = flip ($)

-- | Convenient combinator for setting the CSS class on element creation.
(#.) :: Dom Element -> String -> Dom Element
(#.) mx s = mx # set (attr "class") s


-- | Convience synonym for 'return' to make elements work well with 'set'
-- and with the 'Dom' monad.
--
-- Example usage.
--
-- > e <- newElement window "button"
-- > element e # set text "Ok"
element :: Monad m => Element -> m Element
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

