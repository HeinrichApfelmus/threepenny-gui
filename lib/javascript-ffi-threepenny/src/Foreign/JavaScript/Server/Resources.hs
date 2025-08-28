{-# LANGUAGE TemplateHaskell #-}
module Foreign.JavaScript.Server.Resources
    ( jsDriverCode, cssDriverCode, defaultHtmlFile
    ) where

import qualified Data.Text as T
import Foreign.JavaScript.Server.Include (include)

jsDriverCode :: T.Text
jsDriverCode = T.unlines $ map T.pack
    [ $(include "js/lib/jquery.js")
    , $(include "js/lib/jquery-cookie.js")
    , "var Haskell = {};"
    , $(include "js/comm.js")
    , $(include "js/ffi.js")
    , $(include "js/lib.js")
    , $(include "js/log.js")
    ]

cssDriverCode :: T.Text
cssDriverCode = T.pack $(include "js/haskell.css")

defaultHtmlFile :: T.Text
defaultHtmlFile = T.pack $(include "js/index.html")
