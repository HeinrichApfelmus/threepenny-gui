{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Graphics.UI.Threepenny.Internal.FFI (
    -- * Synopsis
    -- | Combinators for creating JavaScript code and marhsalling.
    
    -- * Documentation
    ffi,
    FFI(..), ToJS(..),
    JSFunction,
    
    toCode, marshalResult,
    ) where

import           Data.Functor
import           Data.ByteString       (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Text.JSON.Generic

import Graphics.UI.Threepenny.Internal.Types

{-----------------------------------------------------------------------------
    JavaScript Code and Foreign Function Interface
------------------------------------------------------------------------------}
-- | JavaScript code snippet.
newtype JSCode = JSCode { unJSCode :: String }
    deriving (Eq, Ord, Show, Data, Typeable)

-- | Helper class for rendering Haskell values as JavaScript expressions.
class ToJS a where
    render :: a -> JSCode

instance ToJS String     where render   = JSCode . encodeJSON
instance ToJS Int        where render   = JSCode . show
instance ToJS Bool       where render b = JSCode $ if b then "false" else "true"
instance ToJS JSValue    where render x = JSCode $ showJSValue x ""
instance ToJS ByteString where render   = JSCode . encodeJSON
instance ToJS ElementId  where
    render (ElementId x) = apply "elidToElement(%1)" [render x]
instance ToJS Element    where render = render . unprotectedGetElementId


-- | A JavaScript function with a given output type @a@.
data JSFunction a = JSFunction
    { code    :: JSCode                          -- code snippet
    , marshal :: Window -> JSValue -> Result a   -- convert to Haskell value
    }

-- | Render function to a textual representation using JavaScript syntax.
toCode :: JSFunction a -> String
toCode = unJSCode . code

-- | Convert function result to a Haskell value.
marshalResult
    :: JSFunction a -- ^ Function that has been executed
    -> Window       -- ^ Browser context
    -> JSValue      -- ^ JSON representation of the return value 
    -> Result a     -- ^ 
marshalResult = marshal

instance Functor JSFunction where
    fmap f = fmapWindow (const f)

fmapWindow :: (Window -> a -> b) -> JSFunction a -> JSFunction b
fmapWindow f (JSFunction c m) = JSFunction c (\w v -> f w <$> m w v)

fromJSCode :: JSCode -> JSFunction ()
fromJSCode c = JSFunction { code = c, marshal = \_ _ -> Ok () }

-- | Helper class for making 'ffi' a variable argument function.
class FFI a where
    fancy :: ([JSCode] -> JSCode) -> a

instance (ToJS a, FFI b) => FFI (a -> b) where
    fancy f a = fancy $ f . (render a:)

instance FFI (JSFunction ())         where fancy f = fromJSCode $ f []
instance FFI (JSFunction String)     where fancy   = mkResult "%1.toString()"
instance FFI (JSFunction JSValue)    where fancy   = mkResult "%1"
instance FFI (JSFunction [ElementId]) where fancy  = mkResult "elementsToElids(%1)"

-- FIXME: We need access to IO in order to turn a Coupon into an Element.
{- 
instance FFI (JSFunction Element)   where
    fancy   = fmapWindow (\w elid -> Element elid w) . fancy
-}

mkResult :: JSON a => String -> ([JSCode] -> JSCode) -> JSFunction a
mkResult client f = JSFunction
    { code    = apply client [f []]
    , marshal = \w -> readJSON
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
    argument i = unJSCode (args !! i)
    
    go []         = []
    go ('%':c:cs) = argument index ++ go cs
        where index = fromEnum c - fromEnum '1'
    go (c:cs)     = c : go cs

