{-# LANGUAGE CPP #-}
module BarTab where

import Prelude hiding (div,span)
import Control.Monad

#ifdef CABAL
import "threepenny-gui" Graphics.UI.Threepenny
#else
import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.Internal.Types
#endif

-- | Main entry point. Starts a TP server.
main :: IO ()
main = serve Config
    { tpPort = 10001
    , tpWorker = \window -> setup window >> handleEvents window
    , tpInitHTML = Nothing
    , tpStatic = "wwwroot"
    }


setup :: Window -> IO ()
setup w = do
    return w # set title "BarTab"

    elAdd    <- button w "Add"
    elRemove <- button w "Remove"
    elInput1 <- inputText w
    elResult <- span      w

    layout <- column
            [row [element elAdd, element elRemove]
            ,hr w
            ,element elInput1
            ,hr w
            ,row [text w "Sum:", element elResult]
            ]

    body w # set children [layout]

    return ()


{- Questions:

* Refer to the span there by  id  or by  variable name?
* How to deal with simple layout? Should #+ force block and #- inline alignment?
* Make layout overridable by CSS?

-}

{-----------------------------------------------------------------------------
    Layout utilities
------------------------------------------------------------------------------}
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
            wrap [entry] # set style [("display","table-cell")]
        wrap row # set style [("display","table-row")]

    wrap rows # set style [("display","table")]

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

button w s = newElement w "button" >>= setHtml s

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
    set x = mapM_ (appendTo x)

-- TODO: get style
style :: Property [(String,String)] Element
style = mkProperty set undefined
    where
    set x xs = setStyle xs x >> return ()

title :: Property String Window
title = mkProperty setTitle undefined

