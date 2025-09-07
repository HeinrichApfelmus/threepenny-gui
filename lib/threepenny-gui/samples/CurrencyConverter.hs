import Control.Monad (void)
import Text.Printf
import Text.Read (readMaybe)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startBrowserGUI setup

setup :: Window -> UI ()
setup window = void $ do
    pure window # set title "Currency Converter"

    dollar <- UI.input
    euro   <- UI.input

    getBody window #+ [
            column [
                grid [[string "Dollar:", element dollar]
                     ,[string "Euro:"  , element euro  ]]
            , string "Amounts update while typing."
            ]]

    euroIn   <- stepper "0" $ UI.valueChange euro
    dollarIn <- stepper "0" $ UI.valueChange dollar
    let
        rate = 0.7 :: Double
        withString f = maybe "-" (printf "%.2f") . fmap f . readMaybe
    
        dollarOut = withString (/ rate) <$> euroIn
        euroOut   = withString (* rate) <$> dollarIn
    
    element euro   # sink value euroOut
    element dollar # sink value dollarOut

