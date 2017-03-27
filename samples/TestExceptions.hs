{- Test the effect of exceptions in the UI monad

    https://github.com/HeinrichApfelmus/threepenny-gui/issues/145
-}

import qualified Graphics.UI.Threepenny       as UI
import           Graphics.UI.Threepenny.Core

main = startGUI defaultConfig { jsWindowReloadOnDisconnect = False } $ \w -> do
    getBody w #+ [UI.h1 # set UI.text "before error"]

    let err = 1
    case err of
            -- FIXME: This function should produce a useful error message
        1 -> runFunction $ ffi "alert(%1)" (error ("ouch " ++ show err) :: String)
        2 -> UI.div # set UI.text (error ("ouch " ++ show err) :: String) >> return ()
        3 -> error $ "ouch " ++ show err
        4 -> liftIO . ioError . userError $ "ouch " ++ show err
        5 -> runFunction $ ffi "throw('ouch')"

    getBody w #+ [UI.h1 # set UI.text "after error"]
    return ()
