{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core


-- | Main entry point.
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    -- active elements
    return w # set title "BarTab"

    elAdd    <- UI.button # set UI.text "Add"
    elRemove <- UI.button # set UI.text "Remove"
    elResult <- UI.span

    inputs   <- liftIO $ newIORef []
    
    -- functionality
    let
        displayTotal = void $ do
            xs <- mapM (get value) =<< liftIO (readIORef inputs)
            element elResult # set text (showNumber . sum $ map readNumber xs)
        
        redoLayout :: UI ()
        redoLayout = void $ do
            layout <- mkLayout =<< liftIO (readIORef inputs)
            getBody w # set children [layout]
            displayTotal

        mkLayout :: [Element] -> UI Element
        mkLayout xs = column $
            [row [element elAdd, element elRemove]
            ,UI.hr]
            ++ map element xs ++
            [UI.hr
            ,row [UI.span # set text "Sum: ", element elResult]
            ]
        
        addInput :: UI ()
        addInput = do
            elInput <- UI.input # set value "0"
            on (domEvent "livechange") elInput $ \_ -> displayTotal
            liftIO $ modifyIORef inputs (elInput:)
        
        removeInput :: UI ()
        removeInput = liftIO $ modifyIORef inputs (drop 1)
    
    on UI.click elAdd    $ \_ -> addInput    >> redoLayout
    on UI.click elRemove $ \_ -> removeInput >> redoLayout
    addInput >> redoLayout


{-----------------------------------------------------------------------------
    Functionality
------------------------------------------------------------------------------}
type Number = Maybe Double

instance Num Number where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    abs = fmap abs
    signum = fmap signum
    fromInteger = pure . fromInteger

readNumber :: String -> Number
readNumber s = listToMaybe [x | (x,"") <- reads s]    
showNumber   = maybe "--" show
