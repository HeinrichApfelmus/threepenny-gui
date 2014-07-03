{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.UI.Threepenny.Internal.FFI (
    -- * Synopsis
    -- | Combinators for creating JavaScript code and marhsalling.
    
    -- * Documentation
    ffi,
    FFI(..), ToJS(..), VariadicJSParam(..),
    JSFunction,
    
    showJSON,
    
    toCode, marshalResult,
    ) where

import           Data.Aeson            as JSON
import qualified Data.Aeson.Types      as JSON
import qualified Data.Aeson.Encode
import           Data.ByteString       (ByteString)
import           Data.Data
import           Data.Functor
import           Data.List
import           Data.Maybe
import           Data.String           (fromString)
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder

import Safe (atMay)

import Graphics.UI.Threepenny.Internal.Types

{-----------------------------------------------------------------------------
    Easy, if stupid conversion between String and JSON

    TODO: Use more efficient string types like ByteString, Text, etc.
------------------------------------------------------------------------------}
showJSON :: ToJSON a => a -> String
showJSON
    = Data.Text.Lazy.unpack
    . Data.Text.Lazy.Builder.toLazyText
    . Data.Aeson.Encode.fromValue . JSON.toJSON

{-----------------------------------------------------------------------------
    JavaScript Code and Foreign Function Interface
------------------------------------------------------------------------------}
-- | JavaScript code snippet.
newtype JSCode = JSCode { unJSCode :: String }
    deriving (Eq, Ord, Show, Data, Typeable)

data VariadicJSParam a = VariadicJSParam { param :: [a] }

-- | Helper class for rendering Haskell values as JavaScript expressions.
class ToJS a where
    render :: a -> JSCode

instance ToJS String     where render   = render . JSON.String . fromString
instance ToJS Float      where render   = JSCode . showJSON
instance ToJS Double     where render   = JSCode . showJSON
instance ToJS Int        where render   = JSCode . show
instance ToJS Bool       where render b = JSCode $ if b then "false" else "true"
instance ToJS JSON.Value where render   = JSCode . showJSON
-- TODO: ByteString instance may be wrong. Only needed for ElementId right now.
instance ToJS ByteString where render   = JSCode . show
instance ToJS ElementId  where
    render (ElementId x) = apply "elidToElement(%1)" [render x]
instance ToJS Element    where render = render . unprotectedGetElementId
instance ToJS a => ToJS (VariadicJSParam a) where 
    render (VariadicJSParam list) = JSCode $ intercalate "," (map (unJSCode . render) list)


-- | A JavaScript function with a given output type @a@.
data JSFunction a = JSFunction
    { code    :: JSCode                                  -- ^ code snippet
    , marshal :: Window -> JSON.Value -> JSON.Parser a
      -- ^ conversion to Haskell value
    }

-- | Render function to a textual representation using JavaScript syntax.
toCode :: JSFunction a -> String
toCode = unJSCode . code

-- | Convert function result to a Haskell value.
marshalResult
    :: JSFunction a   -- ^ Function that has been executed
    -> Window         -- ^ Browser context
    -> JSON.Value     -- ^ JSON representation of the return value 
    -> JSON.Result a  -- ^ Function result as parsed Haskell value
marshalResult fun w = JSON.parse (marshal fun w)

instance Functor JSFunction where
    fmap f = fmapWindow (const f)

fmapWindow :: (Window -> a -> b) -> JSFunction a -> JSFunction b
fmapWindow f (JSFunction c m) = JSFunction c (\w v -> f w <$> m w v)

fromJSCode :: JSCode -> JSFunction ()
fromJSCode c = JSFunction { code = c, marshal = \_ _ -> return () }

-- | Helper class for making 'ffi' a variable argument function.
class FFI a where
    fancy :: ([JSCode] -> JSCode) -> a

instance (ToJS a, FFI b) => FFI (a -> b) where
    fancy f a = fancy $ f . (render a:)

instance FFI (JSFunction ())          where fancy f = fromJSCode $ f []
instance FFI (JSFunction String)      where fancy   = mkResult "%1.toString()"
instance FFI (JSFunction JSON.Value)  where fancy   = mkResult "%1"
instance FFI (JSFunction Int)         where fancy   = mkResult "%1"
instance FFI (JSFunction Double)      where fancy   = mkResult "%1"
instance FFI (JSFunction Float)       where fancy   = mkResult "%1"
instance FFI (JSFunction [ElementId]) where fancy   = mkResult "elementsToElids(%1)"

-- FIXME: We need access to IO in order to turn a Coupon into an Element.
{- 
instance FFI (JSFunction Element)   where
    fancy   = fmapWindow (\w elid -> Element elid w) . fancy
-}

mkResult :: FromJSON a => String -> ([JSCode] -> JSCode) -> JSFunction a
mkResult client f = JSFunction
    { code    = apply client [f []]
    , marshal = \w -> parseJSON
    }

-- | Simple JavaScript FFI with string substitution.
--
-- Inspired by the Fay language. <http://fay-lang.org/>
--
-- > example :: String -> Int -> JSFunction String
-- > example = ffi "$(%1).prop('checked',%2)"
--
-- The 'ffi' function takes a string argument representing the JavaScript
-- code to be executed on the client.
-- Occurrences of the substrings @%1@ to @%9@ will be replaced by
-- subequent arguments.
--
-- Note: Always specify a type signature! The types automate
-- how values are marshalled between Haskell and JavaScript.
-- The class instances for the 'FFI' class show which conversions are supported.
--
ffi :: FFI a => String -> a
ffi macro = fancy (apply macro)

testFFI :: String -> Int -> JSFunction String
testFFI = ffi "$(%1).prop('checked',%2)"

-- | String substitution.
-- Substitute occurences of %1, %2 up to %9 with the argument strings.
-- The types ensure that the % character has no meaning in the generated output.
-- 
-- > apply "%1 and %2" [x,y] = x ++ " and " ++ y
apply :: String -> [JSCode] -> JSCode
apply code args = JSCode $ go code
    where
    at xs i = maybe (error err) id $ atMay xs i
    err     = "Graphics.UI.Threepenny.FFI: Too few arguments in FFI call!"
    argument i = unJSCode (args `at` i)
    
    go []         = []
    go ('%':c:cs) = argument index ++ go cs
        where index = fromEnum c - fromEnum '1'
    go (c:cs)     = c : go cs

