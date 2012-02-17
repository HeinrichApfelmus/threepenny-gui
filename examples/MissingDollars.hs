{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# OPTIONS -fno-warn-type-defaults #-}

module Main where

import Control.Monad.Extra
import Graphics.UI.Ji

-- | Main entry point. Starts a ji server.
main :: IO ()
main = serve Config
    { jiPort = 10002
    , jiRun = runJi
    , jiWorker = worker
    , jiInitHTML = "missing-dollars.html"
    , jiStatic = "wwwroot"
    }

-- | A per-user worker thread. Each user session has a thread.
worker :: MonadJi m => m ()
worker = do
  setTitle "Missing Dollars"
  body <- getBody
  wrap <- newElement "div" >>= setAttr "class" "wrap" >>= appendTo body
  headerMe <- doheader wrap
  missingDollarRiddle wrap headerMe
  attributionsource wrap
  handleEvents

doheader :: MonadJi m => Element -> m Element
doheader body = do
  header <- newElementText body "h1" "The "
  headerMe <- newElementText header "span" "…"
  newElementText' header "span" " Dollars"  
  return headerMe

attributionsource :: MonadJi m => Element -> m ()
attributionsource body = do
  p <- newElementText body "p" ""
  vex <- link "https://github.com/chrisdone/ji/blob/master/examples/MissingDollars.hs"
              "View source code" >>= setAttr "class" "view-source"
  appendTo p vex
  author <- newElementText body "p" "Originally by "
  vex <- link "http://www.vex.net/~trebla/humour/missing_dollar.html"
              "Albert Lai"
  appendTo author vex
  return ()

missingDollarRiddle :: MonadJi m => Element -> Element -> m ()
missingDollarRiddle body headerMe = do
  newElementText' body "h2" "The Guests, The Bellhop, And The Pizza"
  -- User input area.
  intro <- newElementText body "p" ""
  let write text = newElementText intro "span" text >> return ()
      writeRef = newElementText intro "span"
      input = inputText intro
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
  conclude <- newElementText body "p" ""
  let write text = newElementText conclude "span" text >> return ()
      writeRef = newElementText conclude "span"
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
            ret = change - hold
            bal = out - ret
            sum = bal + hold
            diff = sum - out
        setAttr "value" (show out) hotelOut
        setAttr "value" (show cost)  hotelCost
        setAttr "value" (show hold)  hotelHold
        setText (show change)  hotelChange
        setText (show ret)  hotelRet
        setText (show bal)  hotelBal
        setText (show hold)  hotelPocket
        setText (show bal)  hotelBal2
        setText (show hold)  hotelPocket2
        setText (show sum)  hotelSum
        if diff >= 0
           then do setText ("extra $" ++ show diff ++ " come from") hotelMe
                   setText "Extra" headerMe
           else do setText ("missing $" ++ show (-diff) ++ " go")  hotelMe
                   setText "Missing" headerMe
        return ()
  -- Calculate button.
  calculate <- newElementText body "button" "Calculate"
  onClick calculate $ \_ -> do
    result <- readValuesList [hotelOut,hotelCost,hotelHold]
    case result of
      Just [getout,getcost,gethold] -> updateDisplay getout getcost gethold
      _ -> return ()
  -- 
  updateDisplay 30 25 2

  where inputText parent = do
          el <- newElement "input"
          setAttr "type" "text" el
          setAttr "size" "3" el
          appendTo parent el
          return el

link :: MonadJi m => String -> String -> m Element
link url text = do
  el <- newElement "a"
  setAttr "href" url el
  setText text el
  return el

newElementText :: MonadJi m => Element -> String -> String -> m Element
newElementText parent tagName text = do
  el <- newElement tagName
  appendTo parent el
  setText text el
  return el

newElementText' :: MonadJi m => Element -> String -> String -> m ()
newElementText' parent tagName text =
  newElementText parent tagName text >> return ()
