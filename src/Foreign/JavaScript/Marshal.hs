{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Foreign.JavaScript.Marshal (
    ToJS(..), FromJS,
    FFI, JSFunction, toCode, marshalResult, ffi,
    IsHandler, convertArguments, handle,

    NewJSObject, wrapImposeStablePtr,
    ) where

import           Data.Aeson             as JSON
#if MIN_VERSION_aeson(1,0,0)
import qualified Data.Aeson.Text        as JSON   (encodeToTextBuilder)
#else
import qualified Data.Aeson.Encode      as JSON   (encodeToTextBuilder)
#endif
import qualified Data.Aeson.Types       as JSON
import           Data.Functor                     ((<$>))
import           Data.List                        (intercalate)
import qualified Data.Text              as T
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
    render     :: a   -> IO JSCode
    renderList :: [a] -> IO JSCode

    renderList xs = do
        ys <- mapM render xs
        jsCode $ "[" ++ intercalate "," (map unJSCode ys) ++ "]"

jsCode = return . JSCode

instance ToJS Float      where render   = render . JSON.toJSON
instance ToJS Double     where render   = render . JSON.toJSON
instance ToJS Int        where render   = jsCode . show
instance ToJS Bool       where render b = jsCode $ if b then "true" else "false"
instance ToJS JSON.Value where render   = jsCode . showJSON
instance ToJS T.Text     where render   = render . JSON.String
instance ToJS Char       where
    render x   = renderList [x]
    renderList = render . JSON.String . T.pack

instance ToJS a => ToJS [a] where
    render = renderList

instance ToJS HsEvent    where
    render x   = render =<< unprotectedGetCoupon x
instance ToJS JSObject   where
    render x   = apply1 "Haskell.deRefStablePtr(%1)"
                 <$> (render =<< unprotectedGetCoupon x)

-- | Show a type in a JSON compatible way.
showJSON :: ToJSON a => a -> String
showJSON
    = Data.Text.Lazy.unpack
    . Data.Text.Lazy.Builder.toLazyText
    . JSON.encodeToTextBuilder . JSON.toJSON

{-----------------------------------------------------------------------------
    Convert JavaScript values to Haskell values
------------------------------------------------------------------------------}
data FromJS' a = FromJS'
    { wrapCode :: (JSCode -> JSCode)
    , marshal  :: Window -> JSON.Value -> IO a
    }

-- | Helper class for converting JavaScript values to Haskell values.
class FromJS a where
    fromJS   :: FromJS' a

-- | Marshal a simple type to Haskell.
simple :: FromJSON a => (JSCode -> JSCode) -> FromJS' a
simple f =
    FromJS' { wrapCode = f , marshal = \_ -> fromSuccessIO . JSON.fromJSON }
    where
    fromSuccessIO (JSON.Success a) = return a

instance FromJS String     where fromJS = simple $ apply1 "%1.toString()"
instance FromJS T.Text     where fromJS = simple $ apply1 "%1.toString()"
instance FromJS Int        where fromJS = simple id
instance FromJS Double     where fromJS = simple id
instance FromJS Float      where fromJS = simple id
instance FromJS JSON.Value where fromJS = simple id

instance FromJS ()         where
    fromJS = FromJS' { wrapCode = id, marshal = \_ _ -> return () }

instance FromJS JSObject   where
    fromJS = FromJS'
        { wrapCode = apply1 "Haskell.getStablePtr(%1)"
        , marshal  = \w v -> fromJSStablePtr v w
        }

-- FIXME: Not sure whether this instance is really a good idea.
instance FromJS [JSObject] where
    fromJS = FromJS'
        { wrapCode = apply1 "Haskell.map(Haskell.getStablePtr, %1)"
        , marshal  = \w (JSON.Array vs) -> do
            mapM (\v -> fromJSStablePtr v w) (Vector.toList vs)
        }

instance FromJS NewJSObject where
    fromJS = FromJS' { wrapCode = id, marshal = \_ _ -> return NewJSObject }

wrapImposeStablePtr :: Window -> JSFunction NewJSObject -> IO (JSFunction JSObject)
wrapImposeStablePtr w@(Window{..}) f = do
    coupon  <- newCoupon wJSObjects
    rcoupon <- render coupon
    rcode   <- code f
    return $ JSFunction
        { code = return $ apply "Haskell.imposeStablePtr(%1,%2)" [rcode, rcoupon]
        , marshalResult = \w _ -> newRemotePtr coupon (JSPtr coupon) wJSObjects
        }

{-----------------------------------------------------------------------------
    Variable argument JavaScript functions
------------------------------------------------------------------------------}
-- | A JavaScript function with a given output type @a@.
data JSFunction a = JSFunction
    { code          :: IO JSCode
      -- ^ Code snippet that implements the function.
    , marshalResult :: Window -> JSON.Value -> IO a
      -- ^ Marshal the function result to a Haskell value.
    }

-- | Change the output type of a 'JSFunction'.
instance Functor JSFunction where
    fmap f (JSFunction c m) = JSFunction c (\w v -> fmap f $ m w v)

-- | Render function to a textual representation using JavaScript syntax.
toCode :: JSFunction a -> IO String
toCode = fmap unJSCode . code


-- | Helper class for making 'ffi' a variable argument function.
class FFI a where
    fancy :: ([JSCode] -> IO JSCode) -> a

instance (ToJS a, FFI b) => FFI (a -> b) where
    fancy f a = fancy $ \xs -> do
        x <- render a
        f (x:xs)

instance FromJS b        => FFI (JSFunction b) where
    fancy f   = JSFunction
        { code          = wrapCode b <$> f []
        , marshalResult = marshal b
        }
        where b = fromJS

-- | Simple JavaScript FFI with string substitution.
--
-- Inspired by the Fay language. <https://github.com/faylang/fay/wiki>
--
-- > example :: String -> Int -> JSFunction String
-- > example = ffi "$(%1).prop('checked',%2)"
--
-- The 'ffi' function takes a string argument representing the JavaScript
-- code to be executed on the client.
-- Occurrences of the substrings @%1@ to @%9@ will be replaced by
-- subequent arguments.
-- The substring @%%@ in the original will be replaced by @%@ (character escape).
--
-- Note: Always specify a type signature! The types automate
-- how values are marshalled between Haskell and JavaScript.
-- The class instances for the 'FFI' class show which conversions are supported.
--
ffi :: FFI a => String -> a
ffi macro = fancy (return . apply macro)

testFFI :: String -> Int -> JSFunction String
testFFI = ffi "$(%1).prop('checked',%2)"

{-----------------------------------------------------------------------------
    Type classes
------------------------------------------------------------------------------}
-- | Helper class for exporting Haskell functions to JavaScript
-- as event handlers.
class IsHandler a where
    convertArgs :: a -> Int -> [JSCode]
    handle      :: a -> Window -> [JSON.Value] -> IO ()

instance (FromJS a, IsHandler b) => IsHandler (a -> b) where
    convertArgs = convertArgs'
    handle f = \w (a:as) -> do
        x <- marshal fromJS w a
        handle (f x) w as

convertArgs' :: forall a b. (FromJS a, IsHandler b) => (a -> b) -> Int -> [JSCode]
convertArgs' f n = wrap arg : convertArgs (f x) (n+1)
    where
    x    = undefined :: a
    wrap = wrapCode (fromJS :: FromJS' a)
    arg  = JSCode $ "arguments[" ++ show n ++ "]"

instance IsHandler (IO ()) where
    convertArgs _ _ = []
    handle      m   = \_ _ -> m

-- | Code needed to preconvert arguments on the JavaScript side.
convertArguments :: IsHandler a => a -> String
convertArguments f =
    "[" ++ intercalate "," (map unJSCode $ convertArgs f 0) ++ "]"


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

    go []           = []
    go ('%':'%':cs) = '%' : go cs
    go ('%':c  :cs) = argument index ++ go cs
        where index = fromEnum c - fromEnum '1'
    go (c:cs)       = c : go cs

-- | Apply string substitution that expects a single argument.
apply1 :: String -> JSCode -> JSCode
apply1 s x = apply s [x]
