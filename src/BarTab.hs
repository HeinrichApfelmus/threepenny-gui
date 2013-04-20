{-# LANGUAGE CPP, RecursiveDo, TypeSynonymInstances, FlexibleInstances #-}
module BarTab where

import Prelude hiding (div,span)
import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

#ifdef CABAL
import "threepenny-gui" Graphics.UI.Threepenny
#else
import Graphics.UI.Threepenny
#endif

-- | Main entry point. Starts a TP server.
main :: IO ()
main = startGUI Config
    { tpPort      = 10001
    , tpWorker    = setup
    , tpInitHTML  = Nothing
    , tpStatic    = "wwwroot"
    , tpCustomCSS = Nothing
    }


setup :: Window -> IO ()
setup w = do
    -- active elements
    return w # set title "BarTab"

    elAdd    <- button w # set text "Add"
    elRemove <- button w # set text "Remove"
    elResult <- span      w

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
            elInput <- input w
            on blur elInput $ \_ -> displayTotal
            is      <- readIORef inputs
            writeIORef inputs $ elInput : is
            redoLayout
        
        redoLayout :: IO ()
        redoLayout = do
            layout <- mkLayout =<< readIORef inputs
            getBody w # set children [layout]
            return ()
        
        mkLayout :: [Element] -> IO Element
        mkLayout xs = column $
            [row [element elAdd, element elRemove]
            ,hr w]
            ++ map element xs ++
            [hr w
            ,row [span w # set text "Sum: ", element elResult]
            ]
        
    on click elAdd $ \_ -> mkInput
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
