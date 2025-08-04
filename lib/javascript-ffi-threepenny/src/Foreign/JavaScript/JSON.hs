{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
-- | 'JSON' utilities needed for marshaling to/from JavaScript.
module Foreign.JavaScript.JSON
    ( -- * Type
      Value

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
-- JSON Values.
data Value
    = Raw T.Text    -- Unparsed textual representation.
    | Array [Value] -- FIXME: Actually parse this.

string :: T.Text -> Value
string t = Raw ("\"" <> t <> "\"")

class ToJSON a where
    toJSON :: a -> Value

instance ToJSON Bool   where toJSON b = Raw $ if b then "true" else "false"
instance ToJSON Float  where toJSON = Raw . T.pack . show -- FIXME!
instance ToJSON Double where toJSON = Raw . T.pack . show -- FIXME!
instance ToJSON Value  where toJSON = id
instance ToJSON T.Text where toJSON = string

showJSON :: ToJSON a => a -> String
showJSON x = case toJSON x of Raw y -> T.unpack y

fromArray :: Value -> [Value]
fromArray (Array vs) = vs
fromArray _ = error "fromArray: JSON Value is not an array."

data Result a = Error String | Success a

instance Functor Result where
    fmap f (Error   e) = Error e
    fmap f (Success x) = Success (f x)

class FromJSON a where
    fromJSON :: Value -> Result a

fromJSONUsingRead :: Read a => Value -> Result a
fromJSONUsingRead (Raw s) =
    case [ x | (x,"") <- readsPrec 0 (T.unpack s) ] of
        [x] -> Success x
        []  -> Error "fromJSONUsingRead: no parse"
        _   -> Error "fromJSONUsingRead: ambiguous parse"
fromJSONUsingRead _ = Error "fromJSONUsingRead: FIXME: Read list" 

instance FromJSON Bool   where
    fromJSON (Raw "true")  = Success True
    fromJSON (Raw "false") = Success False
    fromJSON _ = Error "fromJSON: Not a valid Bool"
instance FromJSON Int     where fromJSON = fromJSONUsingRead
instance FromJSON Double  where fromJSON = fromJSONUsingRead
instance FromJSON Float   where fromJSON = fromJSONUsingRead
instance FromJSON String  where fromJSON = fromJSONUsingRead
instance FromJSON T.Text  where fromJSON x = T.pack <$> fromJSONUsingRead x
instance FromJSON Value   where fromJSON = Success
instance FromJSON [Value] where fromJSON x = Success [x] -- FIXME! Parse array!

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
