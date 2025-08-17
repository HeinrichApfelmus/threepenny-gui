{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- | 'JSON' utilities needed for marshaling to/from JavaScript.
module Foreign.JavaScript.JSON
    ( -- * Type
      Value (..)

    -- * Construction
    , string
    , ToJSON, toJSON, showJSON

    -- * Elimination
    , fromArray
    , Result (Error, Success)
    , FromJSON, fromJSON
    ) where

import qualified Data.Text as T

#if defined(__MHS__)
{-----------------------------------------------------------------------------
    MicroHs
------------------------------------------------------------------------------}
import Data.List
    ( intersperse )
import Foreign.JavaScript.JSON.Parser
    ( Result (..) )
import Foreign.JavaScript.JSON.Types
    ( Value (..), Number (..) )

string :: T.Text -> Value
string = String

class ToJSON a where
    toJSON :: a -> Value

instance ToJSON Bool   where toJSON = Bool
instance ToJSON Float  where
    toJSON = Number . Double . uncurry encodeFloat . decodeFloat
instance ToJSON Double where toJSON = Number . Double
instance ToJSON Value  where toJSON = id
instance ToJSON T.Text where toJSON = String

showJSON :: ToJSON a => a -> String
showJSON = T.unpack . showValue . toJSON

showValue :: Value -> T.Text
showValue (Object xys) =
    "{" <> (mconcat . intersperse ", ")
        [ showValue (String name) <> ": " <> showValue value
        | (name, value) <- xys
        ] <> "}"
showValue (Array xs) =
    "[" <> (mconcat . intersperse ", ") (map showValue xs) <> "]"
showValue (String s) = T.pack $ "\"" <> concatMap escape (T.unpack s) <> "\""
  where
    escape '\"' = "\\\""
    escape x = [x] -- FIXME: Escape more characters!
showValue (Number (Integer i)) = T.pack $ show i
showValue (Number (Double  d)) = T.pack $ show d
showValue (Bool b)   = if b then "true" else "false"
showValue Null       = "null"

fromArray :: Value -> [Value]
fromArray (Array vs) = vs
fromArray _ = error "fromArray: JSON Value is not an array."

class FromJSON a where
    fromJSON :: Value -> Result a

instance FromJSON Bool   where
    fromJSON (Bool b) = Success b
    fromJSON _ = Error "fromJSON: Value is not a Bool"
instance FromJSON Int     where
    fromJSON (Number (Integer x)) = Success $ fromIntegral x
    fromJSON _ = Error "fromJSON: Value is not a Int"
instance FromJSON Double  where
    fromJSON (Number (Integer x)) = Success $ fromIntegral x
    fromJSON (Number (Double  x)) = Success x
    fromJSON _ = Error "fromJSON: Value is not a Double"
instance FromJSON Float   where
    fromJSON v = fmap (uncurry encodeFloat . decodeFloat) md
      where md = fromJSON v :: Result Double
instance FromJSON String  where
    fromJSON (String s) = Success $ T.unpack s
    fromJSON _ = Error "fromJSON: Value is not a String"
instance FromJSON T.Text  where
    fromJSON (String s) = Success s
    fromJSON _ = Error "fromJSON: Value is not a Text"
instance FromJSON Value   where
    fromJSON = Success
instance FromJSON [Value] where
    fromJSON (Array xs) = Success xs
    fromJSON _ = Error "fromJSON: Value is not a list of Value"

#else
{-----------------------------------------------------------------------------
   GHC
------------------------------------------------------------------------------}
import           Data.Aeson             as JSON
#if defined(CABAL)
#if MIN_VERSION_aeson(1,0,0)
import qualified Data.Aeson.Text        as JSON (encodeToTextBuilder)
#else
import qualified Data.Aeson.Encode      as JSON (encodeToTextBuilder)
#endif
#else
import qualified Data.Aeson.Text        as JSON (encodeToTextBuilder)
#endif
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Vector            as Vector

-- | Smart constructor for a JSON string.
string :: T.Text -> Value
string = JSON.String

-- | Deconstruct an array JSON 'Value'.
fromArray :: Value -> [Value]
fromArray (JSON.Array vs) = Vector.toList vs
fromArray _ = error "fromArray: JSON Value is not an array."

-- | Show a type in a JSON compatible way.
showJSON :: ToJSON a => a -> String
showJSON
    = Data.Text.Lazy.unpack
    . Data.Text.Lazy.Builder.toLazyText
    . JSON.encodeToTextBuilder . JSON.toJSON
#endif
