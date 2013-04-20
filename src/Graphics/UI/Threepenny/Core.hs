module Graphics.UI.Threepenny.Core (
    -- * Guide
    -- $guide
    
    -- * Server
    -- $server
    Config(..), startGUI,
    
    -- * DOM manipulation
    Window, title, getHead, getBody, cookies, getRequestLocation,
    Element, newElement, delete, appendTo,
        children, text, html, attr, value,
        getValuesList,
    getElementsByTagName, getElementByTagName, getElementById,
    
    -- * Layout
    -- | Combinators for quickly creating layouts.
    -- They can be adjusted with CSS later on.
    grid, row, column,
    
    -- * Events
    -- | For a list of predefined events, see "Graphics.UI.Threepenny.Events".
    EventData(..), domEvent, on,
    module Control.Event,
    
    -- * Properties
    -- | For a list of predefined properties, see "Graphics.UI.Threepenny.Properties".
    (#), element,
    Property(..), WriteOnlyProperty, ReadOnlyProperty,
    mkProperty, set, get,
    
    -- * Utilities
    -- | A few raw JavaScript utilities.
    debug, clear,
    callFunction, runFunction, callDeferredFunction,
    atomic,
    
    ) where

import Control.Event
import Control.Monad

import Graphics.UI.Threepenny.Internal.Core  as Core
import Graphics.UI.Threepenny.Internal.Types as Core


{-----------------------------------------------------------------------------
    Guide
------------------------------------------------------------------------------}
{- $guide

Threepenny runs a small web server that displays the user interface
as a web page to any browser that connects to it.
To start the web server, use the 'startGUI' function.

The DOM is accessed much in the same way it is accessed from
JavaScript; elements can be created, searched, updated, moved and
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
startGUI config handler =
    Core.serve config $ \w -> handler w >> Core.handleEvents w

{-----------------------------------------------------------------------------
    DOM
------------------------------------------------------------------------------}
-- | Title of the client window.
title :: WriteOnlyProperty Window String
title = mkProperty Core.setTitle undefined

-- | Cookies on the client.
cookies :: ReadOnlyProperty Window [(String,String)]
cookies = mkProperty undefined Core.getRequestCookies

-- | Child elements of a given element.
children :: WriteOnlyProperty Element [Element]
children = mkProperty set undefined
    where
    set xs x = do
        Core.emptyEl x
        mapM_ (Core.appendElementTo x) xs

-- | Append a child element to a parent element. Non-blocking.
appendTo :: Element     -- ^ Parent.
         -> IO Element  -- ^ Child.
         -> IO Element  -- ^ Returns a reference to the child element again.
appendTo x my = do { y <- my; Core.appendElementTo x y; }

-- | Child elements of a given element as a HTML string.
html :: WriteOnlyProperty Element String
html = mkProperty (\v a -> Core.setHtml v a # void) undefined

-- | Attributes of an element.
attr :: String -> WriteOnlyProperty Element String
attr name = mkProperty (\v a -> Core.setAttr name v a # void) undefined

-- | Value attribute of an element.
-- Particularly relevant for control widgets like 'input'.
value :: Property Element String
value = mkProperty (set' $ attr "value") getValue

-- | Text content of an element.
text :: WriteOnlyProperty Element String
text = mkProperty (\v a -> Core.setText v a # void) undefined

{-----------------------------------------------------------------------------
    Layout
------------------------------------------------------------------------------}
-- | Align given elements in a row. Special case of 'grid'.
row     :: [IO Element]   -> IO Element
row xs = grid [xs]

-- | Align given elements in a column. Special case of 'grid'.
column  :: [IO Element]   -> IO Element
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
grid    :: [[IO Element]] -> IO Element
grid mrows = do
    rows0 <- mapM (sequence) mrows
    let window = elSession $ head $ head rows0
    let wrap xs = newElement window "div" # set children xs
    
    rows  <- forM rows0 $ \row0 -> do
        row <- forM row0 $ \entry ->
            wrap [entry] # set (attr "class") "table-cell"
        wrap row # set (attr "class") "table-row"

    wrap rows # set (attr "class") "table"

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
on :: (a -> Event b) -> a -> Handler b -> IO ()
on f a h = register (f a) h >> return ()


{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
infixl 8 #

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

-- | Convience synonym for 'return' to make elements work well with 'set'.
--
-- Example usage.
--
-- > e <- newElement window "button"
-- > e # set text "Ok"
element :: Element -> IO Element
element = return

-- | Properties are things that you can set and get.
data Property a value = Property
    { set' :: value -> a -> IO ()
    , get' :: a     -> IO value
    }

-- | Properties that only support the 'get' operation.
type ReadOnlyProperty  = Property

-- | Properties that only support the 'set' operation.
type WriteOnlyProperty = Property

-- | Set value of a property in the 'IO' monad.
-- Best used in conjunction with '#'.
set :: Property a value -> value -> IO a -> IO a
set prop value ma = do { a <- ma; set' prop value a; return a; }

-- | Get property value.
get :: Property a value -> a -> IO value
get = get'

-- | Build a property from a getter and a setter.
mkProperty
    :: (value -> a -> IO ())    -- ^ Setter.
    -> (a -> IO value)          -- ^ Getter.
    -> Property a value
mkProperty = Property

