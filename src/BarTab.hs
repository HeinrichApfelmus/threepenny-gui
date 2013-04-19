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
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.Events
import Graphics.UI.Threepenny.Internal.Types
#endif

-- | Main entry point. Starts a TP server.
main :: IO ()
main = serve Config
    { tpPort      = 10001
    , tpWorker    = \window -> setup window >> handleEvents window
    , tpInitHTML  = Nothing
    , tpStatic    = "wwwroot"
    , tpCustomCSS = Nothing
    }


setup :: Window -> IO ()
setup w = do
    -- active elements
    return w # set title "BarTab"

    elAdd    <- button w "Add"
    elRemove <- button w "Remove"
    elResult <- span      w

    inputs   <- newIORef []
    
    -- functionality
    let
        displayTotal = do
            is <- readIORef inputs
            xs <- getValuesList is
            setText (showNumber . sum $ map readNumber xs) elResult
            return ()

        mkInput :: IO Element
        mkInput = do
            input <- inputText w
            on blur input $ \_ -> displayTotal
            is    <- readIORef inputs
            writeIORef inputs $ input : is
            redoLayout
            return input
        
        redoLayout :: IO ()
        redoLayout = do
            layout <- mkLayout =<< readIORef inputs
            body w # set children [layout]
            return ()
        
        mkLayout :: [Element] -> IO Element
        mkLayout xs = column $
            [row [element elAdd, element elRemove]
            ,hr w]
            ++ map element xs ++
            [hr w
            ,row [text w "Sum: ", element elResult]
            ]
        
    on click elAdd $ \_ -> mkInput >> return ()

    mkInput

    return ()


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

{-----------------------------------------------------------------------------
    Layout utilities
------------------------------------------------------------------------------}

{- Questions:

* Refer to the span there by  id  or by  variable name?
* How to deal with simple layout? Should #+ force block and #- inline alignment?
* Make layout overridable by CSS?

-}

infixl 8 #

-- reverse function application, from the diagrams library
(#) :: a -> (a -> b) -> b
(#) = flip ($)

row     :: [IO Element]   -> IO Element
row xs = grid [xs]

column  :: [IO Element]   -> IO Element
column = grid . map (:[])

grid    :: [[IO Element]] -> IO Element
grid mrows = do
    rows0 <- mapM (sequence) mrows
    let window = elSession $ head $ head rows0
    let wrap xs = new window # set children xs
    
    rows  <- forM rows0 $ \row0 -> do
        row <- forM row0 $ \entry ->
            wrap [entry] # set cssClass "table-cell"
        wrap row # set cssClass "table-row"

    wrap rows # set cssClass "table"

element :: Element -> IO Element
element = return

{-----------------------------------------------------------------------------
    Elements
------------------------------------------------------------------------------}
hr :: Window -> IO Element
hr = flip newElement "hr"

span :: Window -> IO Element
span = flip newElement "span"

inputText :: Window -> IO Element
inputText = newInput

body = getBody

text w s = span w >>= setText s

new = flip newElement "div"

button w s = newElement w "button" >>= setText s

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
data Property value a = Property
    { set :: value -> IO a -> IO a
    , get :: IO a  -> IO value
    }

mkProperty :: (a -> value -> IO ()) -> (a -> IO value) -> Property value a
mkProperty set' get' = Property
    { set = \v ma -> do { a <- ma; set' a v; return a }
    , get = \ma   -> ma >>= get'
    }

-- TODO: get all child elements
children :: Property [Element] Element
children = mkProperty set undefined
    where
    set x xs = do
        emptyEl x
        mapM_ (appendTo x) xs

-- TODO: get style
style :: Property [(String,String)] Element
style = mkProperty set undefined
    where
    set a vs = setStyle vs a >> return ()

title :: Property String Window
title = mkProperty setTitle undefined

cssClass :: Property String Element
cssClass = mkProperty set undefined
    where
    set a v = setAttr "class" v a >> return ()
