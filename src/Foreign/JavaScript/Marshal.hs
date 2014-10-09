{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Foreign.JavaScript.Marshal (
    FFI, ToJS(..),
    JSFunction, toCode, marshalResult, ffi,
    ) where

import           Data.Aeson             as JSON
import qualified Data.Aeson.Encode
import qualified Data.Aeson.Types       as JSON
import           Data.Functor                     ((<$>))
import           Data.Text              as T
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector            as Vector
import           Safe                             (atMay)

import Foreign.JavaScript.EventLoop (fromJSStablePtr)
import Foreign.JavaScript.Types
import Foreign.RemotePtr

{-----------------------------------------------------------------------------
    Convert Haskell values to JavaScript values
------------------------------------------------------------------------------}
-- | JavaScript code snippet.
newtype JSCode = JSCode { unJSCode :: String }
    deriving (Eq, Ord, Show)

-- | Helper class for rendering Haskell values as JavaScript expressions.
class ToJS a where
    render :: a -> JSCode

instance ToJS String     where render   = render . JSON.String . T.pack
instance ToJS Text       where render   = render . JSON.String
instance ToJS Float      where render   = render . JSON.toJSON
instance ToJS Double     where render   = render . JSON.toJSON
instance ToJS Int        where render   = JSCode . show
instance ToJS Bool       where render b = JSCode $ if b then "true" else "false"
instance ToJS JSON.Value where render   = JSCode . showJSON

instance ToJS HsEvent    where
    render x = apply "Haskell.newEvent(%1)" [render $ unprotectedGetCoupon x]
instance ToJS JSObject   where
    render x = apply "Haskell.deRefStablePtr(%1)" [render $ unprotectedGetCoupon x]


-- | Show a type in a JSON compatible way.
showJSON :: ToJSON a => a -> String
showJSON
    = Data.Text.Lazy.unpack
    . Data.Text.Lazy.Builder.toLazyText
    . Data.Aeson.Encode.fromValue . JSON.toJSON

{-----------------------------------------------------------------------------
    Convert JavaScript values to Haskell values
------------------------------------------------------------------------------}
-- | A JavaScript function with a given output type @a@.
data JSFunction a = JSFunction
    { code    :: JSCode
      -- ^ code snippet
    , marshal :: Window -> JSON.Value -> IO (JSON.Result a)
      -- ^ conversion to Haskell value
    }

-- | Render function to a textual representation using JavaScript syntax.
toCode :: JSFunction a -> String
toCode = unJSCode . code

-- | Marshal a function result to a Haskell value.
marshalResult :: JSFunction a -> JSON.Value -> Window -> IO a
marshalResult f v w = do
    JSON.Success a <- marshal f w v
    return a

-- | Change the output type of a 'JSFunction'.
instance Functor JSFunction where
    fmap f (JSFunction c m) = JSFunction c (\w v -> fmap (fmap f) $ m w v)

-- | Helper class for making 'ffi' a variable argument function.
class FFI a where
    fancy :: ([JSCode] -> JSCode) -> a

instance (ToJS a, FFI b) => FFI (a -> b) where
    fancy f a = fancy $ f . (render a:)

instance FFI (JSFunction ()) where
    fancy f   = JSFunction { code = f [], marshal = \_ _ -> return (JSON.Success ())}

instance FFI (JSFunction String)      where fancy = mkResult "%1.toString()"
instance FFI (JSFunction Text)        where fancy = mkResult "%1.toString()"
instance FFI (JSFunction Double)      where fancy = mkResult "%1"
instance FFI (JSFunction Float)       where fancy = mkResult "%1"
instance FFI (JSFunction Int)         where fancy = mkResult "%1"
instance FFI (JSFunction JSON.Value)  where fancy = mkResult "%1"

mkResult :: FromJSON a => String -> ([JSCode] -> JSCode) -> JSFunction a
mkResult client f = JSFunction
    { code    = apply client [f []]
    , marshal = \w -> return . JSON.fromJSON
    }

instance FFI (JSFunction JSObject) where
    fancy f = JSFunction
        { code    = apply "Haskell.getStablePtr(%1)" [f []]
        , marshal = \w v -> JSON.Success <$> fromJSStablePtr v w
        }

-- FIXME: Not sure whether this instance is really a good idea.
instance FFI (JSFunction [JSObject]) where
    fancy f = JSFunction
        { code    = apply "Haskell.map(Haskell.getStablePtr, %1)" [f []]
        , marshal = \w (JSON.Array vs) -> do
            JSON.Success <$> mapM (\v -> fromJSStablePtr v w) (Vector.toList vs)
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

{-----------------------------------------------------------------------------
    String utilities
------------------------------------------------------------------------------}
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
