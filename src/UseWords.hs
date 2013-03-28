{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative hiding ((<|>),many)

import Control.Monad
import Control.Arrow
import Control.Monad.IO
import Data.Maybe
import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.DOM
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny.JQuery
import Text.Parsec

main :: IO ()
main = serve Config
    { jiPort = 10003
    , jiRun = runJi
    , jiWorker = worker
    , jiInitHTML = "use-words.html"
    , jiStatic = "wwwroot"
    }

worker :: Ji ()
worker = do
  andthen <- io (readFile fname)
  case parts fname andthen of
    Left parseerror -> debug (show parseerror)
    Right parts -> do
      body <- getBody
      wrap <- new # setClass "wrap" # addTo body
      codeLink wrap
      heading <- new # setClass "header" # addTo wrap
      ul <- new # setClass "vars" # addTo wrap
      let (header,drop 2 -> rest) = splitAt 3 parts
      r <- liftM catMaybes $ forM header (addPart heading)
      rs <- liftM catMaybes $ forM rest (addPart wrap)
      forM_ vars (addVarChoice ul (r ++ rs))
      handleEvents

addVarChoice ul refs (label,(name,def)) = do
  li <- new # addTo ul
  newLabel # addTo li # setText (label ++ ":") # unit
  input <- newInput # addTo li # set "value" def # setClass "var-value"
  bind "livechange" input $ \(EventData (concat . catMaybes -> text)) ->
    forM_ (filter ((==name).fst) refs) $ \(_,el) -> do
      setText text el

addPart wrap part = do
  case part of
    Text str -> do
      new # addTo wrap # setClass "text" # setText str # unit
      return Nothing
    Ref var -> do
      v <- new # addTo wrap
               # setClass "var"
               # maybe (setText ("{" ++ var ++ "}"))
                       (either setHtml setText)
                       (lookup var templatevars)
      return (Just (var,v))

codeLink wrap = do
  newAnchor # set "href" "https://github.com/chrisdone/ji/blob/master/examples/UseWords.hs"
            # setText "View source code"
            # setClass "code-link"
            # addTo wrap
            # unit

fname = "wwwroot/and-then-haskell.txt"

templatevars = map (second Right . snd) vars ++ map (second Left) exts
vars = [("Favourite technology",("favourite-language","Haskell"))
       ,("Technology used at work",("work-language","Python"))
       ,("Cool forum",("bar","LtU"))
       ,("Particular to technology",("particular-stuff","monads"))]
exts = [("br","<br><br>")]

data Part = Text String | Ref String deriving Show

parts :: SourceName -> String -> Either ParseError [Part]
parts = parse (many (ref <|> text)) where
  text = Text <$> many1 (notFollowedBy (string "{") *> anyChar)
  ref = Ref <$> (string "{" *> many1 (noneOf "}") <* (string "}"))
