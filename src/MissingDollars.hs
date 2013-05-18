{-# LANGUAGE CPP, PackageImports #-}

import Control.Monad
import Control.Monad.Extra
import Safe

#ifdef CABAL
import qualified  "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core
import 
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
#endif
import Paths

{-----------------------------------------------------------------------------
    HTML utilities
------------------------------------------------------------------------------}
-- Make a @span@ element with a given text content.
string :: String -> Dom Element
string s = UI.span # set UI.text s

{-----------------------------------------------------------------------------
    Missing Dollars
------------------------------------------------------------------------------}
main :: IO ()
main = do
    static <- getStaticDir
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Nothing
        , tpStatic     = static
        } setup


setup :: Window -> IO ()
setup w = void $ do
    return w # set title "Missing Dollars"
    UI.addStyleSheet w "missing-dollars.css"
    
    body <- getBody w
    withWindow w $ do
        (headerView,headerMe) <- mkHeader
        riddle                <- mkMissingDollarRiddle headerMe
        let layout = [element headerView] ++ riddle ++ attributionSource
        
        element body #+ [UI.div #. "wrap" #+ layout]


mkHeader :: Dom (Element, Element)
mkHeader = do
    headerMe <- UI.span # set text "..."
    view     <- UI.h1   # set text "The " #+
                    [element headerMe, UI.span # set text " Dollars"]
    return (view, headerMe)

attributionSource :: [Dom Element]
attributionSource =
    [ UI.p #+
        [ UI.anchor #. "view-source" # set UI.href urlSource
            # set UI.text "View source code"
        ]
    , UI.p # set text "Originally by" #+
        [ UI.anchor # set UI.href urlAttribution
            # set UI.text "Albert Lai"
        ]
    ]
  where
    urlSource      = "https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/src/MissingDollars.hs"
    urlAttribution = "http://www.vex.net/~trebla/humour/missing_dollar.html"


mkMissingDollarRiddle :: Element -> Dom [Dom Element]
mkMissingDollarRiddle headerMe = do
    -- declare input and display values
    (hotelOut : hotelCost : hotelHold : _)
        <- sequence . replicate 3 $
            UI.input # set (attr "size") "3" # set (attr "type") "text"

    (hotelChange : hotelRet     : hotelBal : hotelPocket :
     hotelBal2   : hotelPocket2 : hotelSum : hotelMe     : _)
        <- sequence . replicate 8 $
            UI.span  # set text ""
    
    -- update procedure
    let updateDisplay out cost hold = do
        let change = out - cost
            ret    = change - hold
            bal    = out - ret
            sum    = bal + hold
            diff   = sum - out
        element hotelOut     # set value (show out)
        element hotelCost    # set value (show cost)
        element hotelHold    # set value (show hold)
        element hotelChange  # set text  (show change)
        element hotelRet     # set text  (show ret)
        element hotelBal     # set text  (show bal)
        element hotelPocket  # set text  (show hold)
        element hotelBal2    # set text  (show bal)
        element hotelPocket2 # set text  (show hold)
        element hotelSum     # set text  (show sum)
        
        if diff >= 0
           then do element hotelMe  # set text ("extra $" ++ show diff ++ " come from")
                   element headerMe # set text "Extra"
           else do element hotelMe  # set text ("missing $" ++ show (-diff) ++ " go")
                   element headerMe # set text "Missing"
        return ()
    
    -- initialize values
    updateDisplay 30 25 2
    
    -- calculate button
    calculate <- UI.button # set text "Calculate"
    on UI.click calculate $ \_ -> do
        result <- mapM readMay `liftM` getValuesList [hotelOut,hotelCost,hotelHold]
        case result of
            Just [getout,getcost,gethold] -> updateDisplay getout getcost gethold
            _ -> return ()
    
    return $
        [ UI.h2 #+ [string "The Guests, The Bellhop, And The Pizza"]
        , UI.p  #+
            [ string "Three guests went to a hotel and gave $"
            , element hotelOut
            , string " to the bellhop to buy pizza. The pizza cost only $"
            , element hotelCost
            , string ". Of the $"
            , element hotelChange
            , string " change, the bellhop kept $"
            , element hotelHold
            , string " to himself and returned $"
            , element hotelRet
            , string " to the guests."
            ]
        , UI.p  #+
            [ string "So the guests spent $"
            , element hotelBal
            , string ", and the bellhop pocketed $"
            , element hotelPocket
            , string ". Now "
            , string "$"
            , element hotelBal2
            , string "+$"
            , element hotelPocket2
            , string "=$"
            , element hotelSum
            , string ". Where did the "
            , element hotelMe
            , string "?"
            ]
        , element calculate
        ]    

