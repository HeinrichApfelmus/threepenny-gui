{-# LANGUAGE CPP, PackageImports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

import Paths

#ifdef CABAL
import qualified "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
#endif

-- | Main entry point. Starts a TP server.
main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig
        { tpPort       = 10000
        , tpStatic     = Just static
        } setup

setup :: Window -> IO ()
setup w = do
    -- active elements
    return w # set title "BarTab"

    elAdd    <- UI.button # set UI.text "Add"
    elRemove <- UI.button # set UI.text "Remove"
    elResult <- UI.span

    inputs   <- newIORef []
    
    -- functionality
    let
        displayTotal = void $ do
            xs <- getValuesList =<< readIORef inputs
            element elResult # set text (showNumber . sum $ map readNumber xs)
        
        redoLayout :: IO ()
        redoLayout = void $ do
            layout <- mkLayout =<< readIORef inputs
            getBody w # set children [layout]
            displayTotal

        mkLayout :: [Element] -> IO Element
        mkLayout xs = column $
            [row [element elAdd, element elRemove]
            ,UI.hr]
            ++ map element xs ++
            [UI.hr
            ,row [UI.span # set text "Sum: ", element elResult]
            ]
        
        addInput :: IO ()
        addInput = do
            elInput <- UI.input # set value "0"
            on (domEvent "livechange") elInput $ \_ -> displayTotal
            modifyIORef inputs (elInput:)
    
    on UI.click elAdd    $ \_ -> addInput                    >> redoLayout
    on UI.click elRemove $ \_ -> modifyIORef inputs (drop 1) >> redoLayout
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
