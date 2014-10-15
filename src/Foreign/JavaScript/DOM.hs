module Foreign.JavaScript.DOM (
    Element,
    newElement, addHandler, appendChild,
    ) where

import qualified Data.Aeson as JSON

import Foreign.JavaScript
import Foreign.JavaScript.Types
import Foreign.RemotePtr

{-----------------------------------------------------------------------------
    DOM
------------------------------------------------------------------------------}
type Element = JSObject

-- | Create a new element of the given tag name.
newElement :: String -> Window -> IO Element
newElement tag w = callFunction w $
    ffi "document.createElement(%1)" tag

-- | Add an event handler to an element.
addHandler :: String -> ([JSON.Value] -> IO ()) -> Element -> Window -> IO ()
addHandler name handler e w = do
    handlerPtr <- exportHandler handler w
    addReachable e handlerPtr                   -- make handler reachable from element
    runFunction w $ ffi "Haskell.bind(%1,%2,%3)" e name handlerPtr

-- | Append a child element to a parent element.
appendChild :: Element -> Element -> Window -> IO ()
appendChild eParent eChild w = do
    -- FIXME: We have to stop the child being reachable from its
    -- /previous/ parent.
    addReachable eParent eChild
    runFunction w $ ffi "$(%1).append($(%2))" eParent eChild


