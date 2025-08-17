{-# LANGUAGE OverloadedStrings #-}

-- | Minimalistic JSON type for use with MicroHs.
-- See https://www.json.org for the specification.
module Foreign.JavaScript.JSON.Types
    ( -- * Type
      Value (..)
    , Number (..)
    ) where

import Data.Text ( Text )
import qualified Data.Text as T

{-----------------------------------------------------------------------------
    MicroHs
------------------------------------------------------------------------------}
data Number
    = Integer Integer
    | Double Double
    deriving (Show)

-- | A JSON value.
data Value
    = Object [(Text, Value)]
    | Array [Value]
    | String Text
    | Number Number -- FIXME
    | Bool Bool
    | Null
    deriving (Show)