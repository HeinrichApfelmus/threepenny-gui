{-# LANGUAGE ViewPatterns #-}

import Control.Applicative hiding ((<|>),many)
import Control.Monad
import Control.Arrow (second)
import Data.Maybe
import Text.Parsec
import System.FilePath ((</>))

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (string, (<|>), many)
import Paths

{-----------------------------------------------------------------------------
    GUI
------------------------------------------------------------------------------}
main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig { tpStatic = Just static } setup


setup :: Window -> UI ()
setup w = do
    filename <- liftIO $ fmap (</> "and-then-haskell.txt") getStaticDir 
    andthen  <- liftIO $ readFile filename
    case parts filename andthen of
        Left parseerror -> debug $ show parseerror
        Right parts     -> void $ do
            UI.addStyleSheet w "use-words.css"

            let (header, Prelude.drop 2 -> rest) = splitAt 3 parts            
            
            (views1, vars1) <- renderParts header
            (views2, vars2) <- renderParts rest
            varChoices      <- mapM (renderVarChoice (vars1 ++ vars2)) vars
        
            getBody w #+
                [ viewSource
                , UI.div #. "wrap" #+ (
                    [ UI.div #. "header" #+ map element views1
                    , UI.ul  #. "vars"   #+ map element varChoices
                    ]
                    ++ map element views2 )
                ]
            
type VariableViews = [(Name, Element)]

renderVarChoice :: VariableViews -> Variable -> UI Element
renderVarChoice views (label,(name,def)) = do
    input <- UI.input #. "var-value" # set value def
    
    on (domEvent "livechange") input $ \(EventData xs) -> do
        let s = concat $ catMaybes xs
        forM_ (filter ((==name).fst) views) $ \(_,el) -> do
            element el # set text s
    
    UI.li #+ [UI.string (label ++ ":"), element input]

renderParts :: [Part] -> UI ([Element], VariableViews)
renderParts parts = do
    views <- mapM renderPart parts
    let variables = [(var, view) | (Ref var, view) <- zip parts views]
    return (views, variables)

renderPart :: Part -> UI Element
renderPart (Text str) = UI.div #. "text" #+ [UI.string str]
renderPart (Ref  var) = UI.div #. "var"
    # maybe (set text $ "{" ++ var ++ "}") (either (set html) (set text))
            (lookup var templatevars)

viewSource :: UI Element
viewSource = UI.p #+
    [UI.anchor #. "view-source" # set UI.href url #+ [UI.string "View source code"]]
    where
    url = samplesURL ++ "UseWords.hs"

{-----------------------------------------------------------------------------
    Parsing
------------------------------------------------------------------------------}
type Name     = String
type Variable = (String, (Name, String))

templatevars = map (second Right . snd) vars ++ map (second Left) exts

vars :: [Variable]
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
