{-# LANGUAGE QuasiQuotes #-}
module Foreign.JavaScript.Resources where

import           Data.Text                          (Text)
import qualified Data.Text                  as Text
import           Foreign.JavaScript.Include


jsDriverCode :: Text
jsDriverCode = Text.unlines $ map Text.pack
    [ [include|js/lib/jquery.js|]
    , [include|js/lib/jquery-cookie.js|]
    , "Haskell = {};"
    , [include|js/comm.js|]
    , [include|js/ffi.js|]
    , [include|js/lib.js|]
    , [include|js/log.js|]
    ]

cssDriverCode :: Text
cssDriverCode = Text.pack [include|js/driver.css|]

defaultHtmlFile :: Text
defaultHtmlFile = Text.pack [include|js/index.html|]
