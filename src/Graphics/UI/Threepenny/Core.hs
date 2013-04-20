module Graphics.UI.Threepenny.Core (
    -- * Guide
    -- $guide
    
    -- * Server
    -- $server
    Config(..), startGUI,
    
    -- * DOM manipulation
    Window, title, getHead, getBody, cookies, getRequestLocation,
    Element, newElement, delete, appendTo,
        children, text, attr, value, getValuesList,
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
    -- | For a list of predefined events, see "Graphics.UI.Threepenny.Properties".
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
title :: WriteOnlyProperty Window String
title = mkProperty Core.setTitle undefined

cookies :: ReadOnlyProperty Window [(String,String)]
cookies = mkProperty undefined Core.getRequestCookies


children :: WriteOnlyProperty Element [Element]
children = mkProperty set undefined
    where
    set xs x = do
        Core.emptyEl x
        mapM_ (Core.appendTo x) xs

attr :: String -> WriteOnlyProperty Element String
attr name = mkProperty (\v a -> Core.setAttr name v a # void) undefined

value :: Property Element String
value = mkProperty (set' $ attr "value") getValue

text :: WriteOnlyProperty Element String
text = mkProperty (\v a -> Core.setText v a # void) undefined

{-----------------------------------------------------------------------------
    Layout
------------------------------------------------------------------------------}
row     :: [IO Element]   -> IO Element
row xs = grid [xs]

column  :: [IO Element]   -> IO Element
column = grid . map (:[])

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
domEvent
    :: String -> Element -> Event EventData
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
(#) :: a -> (a -> b) -> b
(#) = flip ($)

element :: Element -> IO Element
element = return

data Property a value = Property
    { set' :: value -> a -> IO ()
    , get' :: a     -> IO value
    }

type ReadOnlyProperty  = Property
type WriteOnlyProperty = Property

set :: Property a value -> value -> IO a -> IO a
set prop value ma = do { a <- ma; set' prop value a; return a; }

get :: Property a value -> a -> IO value
get = get'

mkProperty
    :: (value -> a -> IO ())    -- ^ setter
    -> (a -> IO value)          -- ^ getter
    -> Property a value
mkProperty = Property

