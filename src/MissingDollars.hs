{-# LANGUAGE CPP, PackageImports #-}

import Control.Monad
import Control.Monad.Extra
import Safe

#ifdef CABAL
import "threepenny-gui" Graphics.UI.Threepenny as UI
#else
import Graphics.UI.Threepenny as UI
#endif

-- | Main entry point. Starts a TP server.
main :: IO ()
main = startGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = "../wwwroot"
    } setup


setup :: Window -> IO ()
setup w = do
    return w # set title "Missing Dollars"
    addStyleSheet w "missing-dollars.css"
    
    body <- getBody w
    wrap <- new w # set cssClass "wrap" # appendTo body

    headerMe <- doheader w wrap
    missingDollarRiddle w wrap headerMe
    attributionsource w wrap

doheader :: Window -> Element -> IO Element
doheader w body = do
    header   <- h1 w      # set text "The " # appendTo body
    headerMe <- UI.span w # set text "..."  # appendTo header
    UI.span w # set text " Dollars"  # appendTo header
    return headerMe

attributionsource :: Window -> Element -> IO ()
attributionsource w body = do
    p <- paragraph w # appendTo body    
    anchor w
        # set (attr "href") "https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/src/MissingDollars.hs"
        # set text "View source code"
        # set cssClass "view-source"
        # appendTo p
    
    author <- paragraph w # set text "Originally by" # appendTo body
    anchor w
        # set (attr "href") "http://www.vex.net/~trebla/humour/missing_dollar.html"
        # set text "Albert Lai"
        # appendTo author

    return ()

missingDollarRiddle :: Window -> Element -> Element -> IO ()
missingDollarRiddle w body headerMe = do
    h2 w
        # set text "The Guests, The Bellhop, And The Pizza"
        # appendTo body

    -- User input area.
    intro <- paragraph w # appendTo body
    let write      = void . writeRef
        writeRef s = UI.span w # set text s # appendTo intro
        input      = UI.input w
                    # set (attr "type") "text"
                    # set (attr "size") "3"
                    # appendTo intro
  
    write "Three guests went to a hotel and gave $"
    hotelOut <- input
    write " to the bellhop to buy pizza. The pizza cost only $"
    hotelCost <- input
    write ". Of the $"
    hotelChange <- writeRef ""
    write " change, the bellhop kept $"
    hotelHold <- input
    write " to himself and returned $"
    hotelRet <- writeRef ""
    write " to the guests."
    
    -- Conclusion paragraph.
    conclude <- paragraph w # appendTo body
    let write      = void . writeRef
        writeRef s = UI.span w # set text s # appendTo conclude
    write "So the guests spent $"
    hotelBal <- writeRef ""
    write ", and the bellhop pocketed $"
    hotelPocket <- writeRef ""
    write ". Now "
    write "$"
    hotelBal2 <- writeRef ""
    write "+$"
    hotelPocket2 <- writeRef ""
    write "=$"
    hotelSum <- writeRef ""
    write ". Where did the "
    hotelMe <- writeRef ""
    write "!"

    -- Update procedure.
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
    
    -- Calculate button.
    calculate <- button w # set text "Calculate" # appendTo body
    on click calculate $ \_ -> do
        result <- mapM readMay `liftM` getValuesList [hotelOut,hotelCost,hotelHold]
        case result of
            Just [getout,getcost,gethold] -> updateDisplay getout getcost gethold
            _ -> return ()
 
    updateDisplay 30 25 2

