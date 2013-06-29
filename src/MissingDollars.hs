{-# LANGUAGE CPP, PackageImports #-}

import Control.Monad
import Control.Monad.Extra
import Safe

#ifdef CABAL
import qualified  "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
#endif
import Paths

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
    (headerView,headerMe) <- mkHeader
    riddle                <- mkMissingDollarRiddle headerMe
    let layout = [element headerView] ++ riddle ++ attributionSource
        
    element body #+ [UI.div #. "wrap" #+ layout]


mkHeader :: IO (Element, Element)
mkHeader = do
    headerMe <- string "..."
    view     <- UI.h1   #+ [string "The ", element headerMe, string " Dollars"]
    return (view, headerMe)

attributionSource :: [IO Element]
attributionSource =
    [ UI.p #+
        [ UI.anchor #. "view-source" # set UI.href urlSource
            #+ [string "View source code"]
        ]
    , UI.p #+
        [ string "Originally by "
        , UI.anchor # set UI.href urlAttribution #+ [string "Albert Lai"]
        ]
    ]
  where
    urlSource      = "https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/src/MissingDollars.hs"
    urlAttribution = "http://www.vex.net/~trebla/humour/missing_dollar.html"


mkMissingDollarRiddle :: Element -> IO [IO Element]
mkMissingDollarRiddle headerMe = do
    -- declare input and display values
    (hotelOut : hotelCost : hotelHold : _)
        <- sequence . replicate 3 $
            UI.input # set (attr "size") "3" # set (attr "type") "text"

    (hotelChange : hotelRet     : hotelBal : hotelPocket :
     hotelBal2   : hotelPocket2 : hotelSum : hotelMe     : _)
        <- sequence . replicate 8 $ UI.span
    
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
    calculate <- UI.button #+ [string "Calculate"]
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

