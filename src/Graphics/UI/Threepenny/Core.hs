{-# LANGUAGE FlexibleContexts #-}
module Graphics.UI.Threepenny.Core (
    -- * Synopsis
    -- | Core functionality of the Threepenny GUI library.
    
    -- * Server
    -- $server
    Config(..), defaultConfig, startGUI,
    
    -- * UI monad
    -- $ui
    UI, runUI, askWindow, liftIOLater,
    module Control.Monad.IO.Class,
    module Control.Monad.Fix,
    
    -- * Browser Window
    Window, title,
    
    -- * DOM elements
    -- | Create and manipulate DOM elements.
    Element, getWindow, mkElement, mkElementNamespace, delete,
        string,
        getHead, getBody,
        (#+), children, text, html, attr, style, value,
    getElementsByTagName, getElementById, getElementsByClassName,
    
    -- * Layout
    -- | Combinators for quickly creating layouts.
    -- They can be adjusted with CSS later on.
    grid, row, column,
    
    -- * Events
    -- | For a list of predefined events, see "Graphics.UI.Threepenny.Events".
    EventData, domEvent, unsafeFromJSON, disconnect, on, onEvent, onChanges,
    module Reactive.Threepenny,
    
    -- * Attributes
    -- | For a list of predefined attributes, see "Graphics.UI.Threepenny.Attributes".
    (#), (#.),
    Attr, WriteAttr, ReadAttr, ReadWriteAttr(..),
    set, sink, get, mkReadWriteAttr, mkWriteAttr, mkReadAttr,
    bimapAttr, fromObjectProperty,
    
    -- * Widgets
    Widget(..), element, widget,
    
    -- * JavaScript FFI
    -- | Direct interface to JavaScript in the browser window.
    debug, timestamp,
    ToJS, FFI,
    JSFunction, ffi, runFunction, callFunction,
    ffiExport,
    
    -- * Internal and oddball functions
    fromJQueryProp,
    
    ) where

import Control.Monad          (forM_, forM, void)
import Control.Monad.Fix
import Control.Monad.IO.Class

import qualified Data.Aeson                      as JSON
import qualified Foreign.JavaScript              as JS
import qualified Graphics.UI.Threepenny.Internal as Core
import qualified Reactive.Threepenny             as Reactive

-- exports
import Foreign.JavaScript                   (Config(..), defaultConfig)
import Graphics.UI.Threepenny.Internal
import Reactive.Threepenny                  hiding (onChange)


{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
{- $server

To display the user interface, you have to start a server using 'startGUI'.
Then, visit the URL <http://localhost:8023/> in your browser
(assuming that you use the default server configuration 'defaultConfig',
or have set the port number to @jsPort=Just 8023@.)

The server is multithreaded.
FFI calls can be made concurrently, but events are handled sequentially.

-}

{-----------------------------------------------------------------------------
    Browser window
------------------------------------------------------------------------------}
-- | Title of the client window.
title :: WriteAttr Window String
title = mkWriteAttr $ \s _ ->
    runFunction $ ffi "document.title = %1;" s

{-----------------------------------------------------------------------------
    DOM Elements
------------------------------------------------------------------------------}
-- | Append DOM elements as children to a given element.
(#+) :: UI Element -> [UI Element] -> UI Element
(#+) mx mys = do
    x  <- mx
    ys <- sequence mys
    mapM_ (Core.appendChild x) ys
    return x

-- | Child elements of a given element.
children :: WriteAttr Element [Element]
children = mkWriteAttr set
    where
    set xs x = do
        Core.clearChildren x
        mapM_ (Core.appendChild x) xs

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

-- | Text content of an element.
text :: WriteAttr Element String
text = mkWriteAttr $ \s el ->
    runFunction $ ffi "$(%1).text(%2)" el s

-- | Make a @span@ element with a given text content.
string :: String -> UI Element
string s = mkElement "span" # set text s

-- | Get the head of the page.
getHead :: Window -> UI Element
getHead _ = fromJSObject =<< callFunction (ffi "document.head")

-- | Get the body of the page.
getBody :: Window -> UI Element
getBody _ = fromJSObject =<< callFunction (ffi "document.body")

-- | Get all elements of the given tag name.
getElementsByTagName
    :: Window        -- ^ Browser window
    -> String        -- ^ The tag name.
    -> UI [Element]  -- ^ All elements with that tag name.
getElementsByTagName _ tag =
    mapM fromJSObject =<< callFunction (ffi "document.getElementsByTagName(%1)" tag)

-- | Get an element by a particular ID.
--
-- FIXME: Misleading type, throws a JavaScript exception when element not found.
getElementById
    :: Window              -- ^ Browser window
    -> String              -- ^ The ID string.
    -> UI (Maybe Element)  -- ^ Element (if any) with given ID.
getElementById _ id = do
    x <- fromJSObject =<< callFunction (ffi "document.getElementById(%1)" id)
    return $ Just x

-- | Get a list of elements by particular class.
getElementsByClassName
    :: Window        -- ^ Browser window
    -> String        -- ^ The class string.
    -> UI [Element]  -- ^ Elements with given class.
getElementsByClassName window s =
    mapM fromJSObject =<< callFunction (ffi "document.getElementsByClassName(%1)" s)

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
-- | Convenience function to register 'Event's for 'Element's.
--
-- Example usage.
--
-- > on click element $ \_ -> ...
on :: (element -> Event a) -> element -> (a -> UI void) -> UI ()
on f x = void . onEvent (f x)

-- | Register an 'UI' action to be executed whenever the 'Event' happens.
-- 
-- FIXME: Should be unified with 'on'?
onEvent :: Event a -> (a -> UI void) -> UI (UI ())
onEvent e h = do
    window <- askWindow
    unregister <- liftIO $ register e (void . runUI window . h)
    return (liftIO unregister)

-- | Execute a 'UI' action whenever a 'Behavior' changes.
-- Use sparingly, it is recommended that you use 'sink' instead.
onChanges :: Behavior a -> (a -> UI void) -> UI ()
onChanges b f = do
    window <- askWindow
    liftIO $ Reactive.onChange b (void . runUI window . f)

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

instance Functor (ReadWriteAttr x i) where
    fmap f = bimapAttr id f

-- | Map input and output type of an attribute.
bimapAttr :: (i' -> i) -> (o -> o')
          -> ReadWriteAttr x i o -> ReadWriteAttr x i' o'
bimapAttr from to attr = attr
    { get' = fmap to . get' attr
    , set' = \i' -> set' attr (from i')
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
    window <- askWindow
    liftIOLater $ do
        i <- currentValue bi
        runUI window $ set' attr i x
        Reactive.onChange bi  $ \i -> runUI window $ set' attr i x  
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

-- | Turn a jQuery property @.prop()@ into an attribute.
fromJQueryProp :: String -> (JSON.Value -> a) -> (a -> JSON.Value) -> Attr Element a
fromJQueryProp name from to = mkReadWriteAttr get set
    where
    set v el = runFunction $ ffi "$(%1).prop(%2,%3)" el name (to v)
    get   el = fmap from $ callFunction $ ffi "$(%1).prop(%2)" el name

-- | Turn a JavaScript object property @.prop = ...@ into an attribute.
fromObjectProperty :: (FromJS a, ToJS a, FFI (JSFunction a)) => String -> Attr Element a
fromObjectProperty name = mkReadWriteAttr get set
    where
    set v el = runFunction  $ ffi ("%1." ++ name ++ " = %2") el v    
    get   el = callFunction $ ffi ("%1." ++ name) el

{-----------------------------------------------------------------------------
    Widget class
------------------------------------------------------------------------------}
-- | Widgets are data types that have a visual representation.
class Widget w where
    getElement :: w -> Element

instance Widget Element where
    getElement = id

-- | Convenience synonym for 'return' to make elements work well with 'set'.
-- Also works on 'Widget's.
--
-- Example usage.
--
-- > e <- mkElement "button"
-- > element e # set text "Ok"
element :: MonadIO m => Widget w => w -> m Element
element = return . getElement

-- | Convenience synonym for 'return' to make widgets work well with 'set'.
widget  :: Widget w => w -> UI w
widget  = return
