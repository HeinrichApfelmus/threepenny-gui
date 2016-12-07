{- Test the effect of exceptions in the UI monad

    https://github.com/HeinrichApfelmus/threepenny-gui/issues/145
-}

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

main = startGUI defaultConfig $ \w -> do
    getBody w #+ [UI.h1 # set UI.text "before error"]
    -- liftIO $ ioError (userError "ouch")
    runFunction $ ffi "throw('ouch')"
    getBody w #+ [UI.h1 # set UI.text "after error"]
    return ()
