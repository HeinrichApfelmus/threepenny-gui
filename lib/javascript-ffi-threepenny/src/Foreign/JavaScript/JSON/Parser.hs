-- | Minimalistic JSON parser for use with MicroHs.
module Foreign.JavaScript.JSON.Parser
    ( Result (..)
    , decodeJSON
    ) where

import Prelude hiding ( null, exponent )

import Control.Monad
    ( ap )
import Data.Char
    ( isDigit )
import Data.List
    ( foldl' )
import Data.Text
    ( Text, pack, unpack )
import Foreign.JavaScript.JSON.Types
    ( Number (..), Value (..) )
import Text.ParserCombinators.ReadP
    ( ReadP, (<++), char, eof, munch, munch1
    , option, readP_to_S, satisfy, sepBy, skipSpaces
    )

{-----------------------------------------------------------------------------
    JSON grammar
------------------------------------------------------------------------------}
json :: ReadP Value
json = whitespace *> value <* eof

-- Useful convention for composability:
-- Every non-terminal of the grammar expects non-whitespace at the beginning,
-- but eats up all whitespace at the end.
symbol c = char c *> whitespace
token cs = mapM_ char cs *> whitespace

value :: ReadP Value
value = object <++ array <++ (String <$> string) <++ bool <++ null <++ number

object :: ReadP Value
object  = Object <$ symbol '{' <*> members <* symbol '}'
member  = (,) <$> string <* symbol ':' <*> value
members = member `sepBy` symbol ','

array :: ReadP Value
array    = Array <$ symbol '[' <*> elements <* symbol ']'
element  = value
elements = element `sepBy` symbol ','

string :: ReadP Text
string     = pack <$ char '\"' <*> characters <* char '\"' <* whitespace
characters = munch (\c -> not (c `elem` "\""))
    -- FIXME: Implement escapes properly!

bool = Bool <$> ((True <$ token "true") <++ (False <$ token "false"))
null = Null <$ token "null"

number   = mkNumber <$>
    integer
    <*> option Nothing (Just <$> fraction)
    <*> exponent
    <*  whitespace
integer  = sign <*> (integerFromDigits <$> munch1 isDigit)
sign     = option id (negate <$ char '-')
fraction = fractionFromDigits <$ symbol '.' <*> munch1 isDigit
exponent = pure ()
    -- FIXME: Implement exponent properly.

mkNumber i Nothing  _ = Number (Integer i)
mkNumber i (Just d) _ = Number (Double $ fromRational $ fromIntegral i + d)
    -- FIXME: Deal with rounding errors.

integerFromDigits :: [Char] -> Integer
integerFromDigits = foldl' (\x c -> 10*x + fromDigit c) 0
  where fromDigit c = fromIntegral (fromEnum c - fromEnum '0')

fractionFromDigits :: [Char] -> Rational
fractionFromDigits cs =
    fromIntegral (integerFromDigits cs) / fromIntegral (10 ^ length cs)

{-----------------------------------------------------------------------------
    Parser combinators
------------------------------------------------------------------------------}
data Result a = Error String | Success a
    deriving (Show)

instance Functor Result where
    fmap f (Error   s) = Error s
    fmap f (Success x) = Success (f x)

instance Applicative Result where
    pure x = Success x
    (<*>)  = ap

instance Monad Result where
    (Error   e) >>= _ = Error e
    (Success x) >>= g = g x

whitespace :: ReadP ()
whitespace = skipSpaces

decodeJSON :: Text -> Result Value
decodeJSON s =
    case [ x | (x,"") <- readP_to_S json (unpack s) ] of
        [x] -> Success x
        []  -> Error "parseJSON: no parse"
        _   -> Error "parseJSON: ambiguous parse"
