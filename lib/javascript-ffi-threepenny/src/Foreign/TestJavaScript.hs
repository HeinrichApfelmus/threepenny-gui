{-----------------------------------------------------------------------------
    Quick test of Foreign.JavaScript
------------------------------------------------------------------------------}
module Foreign.TestJavaScript where

import Data.IORef
import Foreign.JavaScript
    ( Window, JSObject, IsHandler
    , withBrowserWindow, callFunction, runFunction, ffi, debug
    , exportHandler
    )

import qualified Foreign.RemotePtr as RemotePtr

{-----------------------------------------------------------------------------
    Create an element and an on-click handler
------------------------------------------------------------------------------}
main :: IO ()
main = withPreventGC $ \ref -> withBrowserWindow $ \w -> do
    pure ()
    body <- getBody w
    writeIORef ref $ Just body

    button <- newElement "button" w
    runFunction w $ ffi "$(%1).text('Click me!')" button

    let handler = do
            msg <- newElement "div" w
            let s = "I have been clicked."
            runFunction w $ ffi "$(%1).text(%2)" msg s
            appendChild body msg w

            msg <- newElement "div" w
            runFunction w $ ffi "$(%1).text((%2).toString())" msg [1,2,3::Int]
            appendChild body msg w
    addHandler "click" handler button w

    appendChild body button w
    pure ()

getBody :: Window -> IO Element
getBody w = callFunction w $
    ffi "document.getElementsByTagName('body')[0]"

withPreventGC :: (IORef (Maybe a) -> IO ()) -> IO ()
withPreventGC f = do
    ref <- newIORef Nothing
    f ref
    mx  <- readIORef ref
    case mx of
        Nothing -> pure ()
        Just x  -> x `seq` pure ()

{-----------------------------------------------------------------------------
    DOM Manipulation
------------------------------------------------------------------------------}
type Element = JSObject

-- | Create a new element of the given tag name.
newElement :: String -> Window -> IO Element
newElement tag w = callFunction w $
    ffi "document.createElement(%1)" tag

-- | Add an event handler to an element.
addHandler :: IsHandler a => String -> a -> Element -> Window -> IO ()
addHandler name handler e w = do
    handlerPtr <- exportHandler w handler
    RemotePtr.addReachable e handlerPtr     -- make handler reachable from element
    runFunction w $ ffi "Haskell.on(%1,%2,%3)" e name handlerPtr

-- | Append a child element to a parent element.
appendChild :: Element -> Element -> Window -> IO ()
appendChild eParent eChild w = do
    -- FIXME: We have to stop the child being reachable from its
    -- /previous/ parent.
    RemotePtr.addReachable eParent eChild
    runFunction w $ ffi "$(%1).append($(%2))" eParent eChild
