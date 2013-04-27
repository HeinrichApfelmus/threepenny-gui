{-# LANGUAGE CPP, PackageImports #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Control.Applicative hiding ((<|>),many)

import Control.Monad
import Control.Arrow (second)
import Data.Maybe
#ifdef CABAL
import "threepenny-gui" Graphics.UI.Threepenny
import "threepenny-gui" Graphics.UI.Threepenny.Elements as H
#else
import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Elements as H
#endif
import Text.Parsec


main :: IO ()
main = startGUI Config
    { tpPort       = 10000
    , tpCustomHTML = Nothing
    , tpStatic     = "../wwwroot"
    } setup


filename = "../wwwroot/and-then-haskell.txt"

setup :: Window -> IO ()
setup w = do
    andthen <- readFile filename
    case parts filename andthen of
        Left parseerror -> debug w $ show parseerror
        Right parts     -> do
            body    <- getBody w
            addStyleSheet w "use-words.css"

            wrap    <- new w # set cssClass "wrap"   # appendTo body
            codeLink w wrap
            
            heading <- new w # set cssClass "header" # appendTo wrap
            ul      <- new w # set cssClass "vars"   # appendTo wrap
            
            let (header, Prelude.drop 2 -> rest) = splitAt 3 parts
            r  <- liftM catMaybes $ forM header (addPart w heading)
            rs <- liftM catMaybes $ forM rest   (addPart w wrap)
            forM_ vars (addVarChoice w ul (r ++ rs))

addVarChoice w ul refs (label,(name,def)) = do
    li <- new w # appendTo ul
    H.span w
        # set text (label ++ ":")
        # appendTo li
    input <- H.input w
        # set cssClass "var-value"
        # set value def
        # appendTo li
    
    on (domEvent "livechange") input $ \(EventData xs) -> do
        let s = concat $ catMaybes xs
        forM_ (filter ((==name).fst) refs) $ \(_,el) -> do
            element el # set text s

addPart w wrap (Text str) = do
    new w
        # set cssClass "text"
        # set text str
        # appendTo wrap
    return Nothing
addPart w wrap (Ref  var) = do
    v <- new w
        # set cssClass "var"
        # maybe (set text $ "{" ++ var ++ "}") (either (set html) (set text))
                (lookup var templatevars)
        # appendTo wrap
    return (Just (var,v))

codeLink w wrap = do
    anchor w
        # set (attr "href") "https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/src/UseWords.hs"
        # set text "View source code"
        # set cssClass "code-link"
        # appendTo wrap

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
    ref  = Ref  <$> (string "{" *> many1 (noneOf "}") <* (string "}"))
