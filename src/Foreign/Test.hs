{-----------------------------------------------------------------------------
    threepenny-gui
    
    This module is for quick testing of the Foreign.JavaScript code.
------------------------------------------------------------------------------}

import Data.Aeson as JSON
import Data.IORef
import Foreign.JavaScript
import Foreign.RemotePtr

main = test2

{-----------------------------------------------------------------------------
    Call a JS function and route result through server
------------------------------------------------------------------------------}
{-
test1 = serve defaultConfig $ \w -> do
    x <- callEval "document.lastModified" w
    runEval ("document.write('" ++ toString x ++ "')") w

toString :: JSON.Value -> String
toString x = let Success y = JSON.fromJSON x in y
-}

{-----------------------------------------------------------------------------
    Create an element and an on-click handler
------------------------------------------------------------------------------}
test2 = withPreventGC $ \ref -> serve defaultConfig $ \w -> do
    body   <- getBody w
    writeIORef ref body

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

getBody :: Window -> IO Element
getBody w = callFunction w $
    ffi "document.getElementsByTagName('body')[0]"

withPreventGC f = do
    ref <- newIORef $ error "It's fine, just keeping value alive."
    f ref
    x  <- readIORef ref
    x `seq` return ()


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
    addReachable e handlerPtr                   -- make handler reachable from element
    runFunction w $ ffi "Haskell.bind(%1,%2,%3)" e name handlerPtr

-- | Append a child element to a parent element.
appendChild :: Element -> Element -> Window -> IO ()
appendChild eParent eChild w = do
    -- FIXME: We have to stop the child being reachable from its
    -- /previous/ parent.
    addReachable eParent eChild
    runFunction w $ ffi "$(%1).append($(%2))" eParent eChild

