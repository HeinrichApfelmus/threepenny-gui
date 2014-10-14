import Data.Aeson as JSON
import Data.IORef
import Foreign.JavaScript
import Foreign.JavaScript.DOM

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
    let handler _ = do
            msg <- newElement "div" w
            runFunction w $ ffi "$(%1).text('I have been clicked.')" msg
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


