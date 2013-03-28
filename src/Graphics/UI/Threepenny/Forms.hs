{-# OPTIONS -fno-warn-name-shadowing #-}

-- | Forms.

module Graphics.UI.Threepenny.Forms where

import Control.Monad
import Data.List.Extra
import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.DOM
import Graphics.UI.Threepenny.Elements

-- | Make a form and do stuff with it.
withForm :: MonadJi m
         => String -> (Element -> Element -> Element -> (m () -> m ()) -> m ()) -> m Element
withForm buttonTitle cont = do
  form <- newForm #. "review-form"
  inputArea <- new #. "input-area" #+ form
  forComplete <- new #. "form-complete" #+ form
  inputs <- new #. "form-inputs" #+ inputArea
  save <- newAnchor #. "form-button"
  let submitFor submitHandler = do
        onClick save (\_ -> submitHandler)
        bind "submit" form (\_ -> submitHandler)
  cont form forComplete inputs submitFor
  buttons <- new #. "form-input" #+ inputs
  return save #+ buttons #= buttonTitle # unit
  return form

-- | Make a labelled input.
labelled :: (MonadJi m) => Element -> String -> String -> (Element -> m (a,Element)) -> m a
labelled inputs text name input = do
  li <- new #. "form-input" #+ inputs
  newLabel #. "form-label" #+ li #= text # set "for" name # unit
  (a,el) <- input li
  return el # set "name" name # unit
  return a

-- | Make a suggestions box.
suggestionsBox :: (Show a,MonadJi m)
               => Element
               -> Element
               -> String
               -> (String -> m [(a,String)])
               -> (String -> m a)
               -> m Element
suggestionsBox parent input startMsg suggestFor insertFor = do
  (box,suggestions) <- suggester
  choice <- newInput # set "type" "hidden" #+ box
  bind "livechange" input (suggest box choice suggestions) 

  return choice

  where suggest box choice suggestions e =
          case e of
            EventData [Just v] -> updateSuggestions box choice suggestions v
            _                  -> return ()

        updateSuggestions box choice suggestions v = do
          sgs <- suggestFor v
          emptyEl suggestions # unit
          if length sgs == 0
             then let msg | null (trim v) = startMsg
                          | otherwise     = "No results for that. Keep going?"
                  in addCreate box msg choice suggestions v
             else do
               addCreate box "Choose below or create…" choice suggestions v
               forM_ sgs $ \(sid,label) -> do
                  li <- new #. "suggestion" #= label #+ suggestions
                  onClick li $ \_ -> do
                    set "value" (show sid) choice # unit
                    setChoice box label

        addCreate box msg choice suggestions v = do
          li <- new #+ suggestions #. "intro-create"
          new #+ li #. "intro-note" #= msg # unit
          when (not (null (trim v))) $ do
            btn <- newAnchor #+ li #. "suggest-create small-form-button" #= "Create it!"
            bind "click" btn $ \_ -> do
              _eid <- insertFor v
              updateSuggestions box choice suggestions v
          addClear li

        suggester = do
          suggestarea <- new #+ parent
          suggestions <- new #. "input-suggestions" #+ suggestarea
          new #. "suggestion-spacing" #+ suggestarea # unit
          return (suggestarea,suggestions)

        setChoice box label = do
          set "style" "display:none" input # unit
          set "style" "display:none" box # unit
          chosen <- new #. "chosen-display" #+ parent #= label
          btn <- newAnchor #= "Change" #. "resume-choose" #+ parent
          onClick btn $ \_ -> do
            delete chosen
            delete btn
            set "style" "" input # unit
            set "style" "" box # unit
