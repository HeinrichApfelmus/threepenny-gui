{-----------------------------------------------------------------------------
    Threepenny

    A simple counter with functional reactive programming (FRP)
------------------------------------------------------------------------------}
{-# LANGUAGE ScopedTypeVariables #-}
module E04c_counter_frp_behavior where

import Data.IORef

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}

main = startGUI defaultConfig { jsStatic = Just "." } setup

setup :: Window -> UI ()
setup window = do
    UI.addStyleSheet window "foundation-5.css"
    return  window # set UI.title "Simple counter"
    getBody window #+ [ mkDisplay ]
    return ()

-- | Create counter element
mkDisplay :: UI Element
mkDisplay = do
    bup     <- UI.button # set UI.text "Up"
    bdown   <- UI.button # set UI.text "Down"
    counter <- UI.div    # set UI.text "0"

    -- current count as a time-varying value
    (count :: Behavior Int) <- accumB 0 $ unionWith const
        ( (+1)         <$ UI.click bup   )
        ( (subtract 1) <$ UI.click bdown )

    element counter # sink UI.text (show <$> count)

    -- visual style
    UI.div #. "row" #+
        [ UI.ul #. "button-group round" #+
            map (\x -> UI.li #+ [element x]) [bup, bdown]
        , element counter #. "small-2 columns panel"
        ]
