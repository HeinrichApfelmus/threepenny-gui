{-# LANGUAGE CPP, PackageImports #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

import Paths

#ifdef CABAL
import qualified "threepenny-gui" Graphics.UI.Threepenny
import "threepenny-gui" Graphics.UI.Threepenny.Core
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
#endif

-- | Main entry point. Starts a TP server.
main :: IO ()
main = do
    static <- getStaticDir
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Nothing
        , tpStatic     = static
        } setup

setup :: Window -> IO ()
setup w = do
    -- active elements
    return w # set title "BarTab"

    elAdd    <- withWindow w $ UI.button # set UI.text "Add"
    elRemove <- withWindow w $ UI.button # set UI.text "Remove"
    elResult <- withWindow w $ UI.span

    inputs   <- newIORef []
    
    -- functionality
    let
        displayTotal = do
            is <- readIORef inputs
            xs <- getValuesList is
            element elResult # set text (showNumber . sum $ map readNumber xs)
            return ()

        mkInput :: IO ()
        mkInput = do
            elInput <- withWindow w $ UI.input
            on UI.blur elInput $ \_ -> displayTotal
            is      <- readIORef inputs
            writeIORef inputs $ elInput : is
            redoLayout
        
        redoLayout :: IO ()
        redoLayout = void $ do
            layout <- withWindow w . mkLayout =<< readIORef inputs
            getBody w # set children [layout]
        
        mkLayout :: [Element] -> Dom Element
        mkLayout xs = column $
            [row [element elAdd, element elRemove]
            ,UI.hr]
            ++ map element xs ++
            [UI.hr
            ,row [UI.span # set text "Sum: ", element elResult]
            ]
        
    on UI.click elAdd $ \_ -> mkInput
    mkInput


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
