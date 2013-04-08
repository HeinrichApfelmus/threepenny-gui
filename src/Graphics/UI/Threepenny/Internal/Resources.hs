{-# LANGUAGE QuasiQuotes, CPP #-}
module Graphics.UI.Threepenny.Internal.Resources where

import           Data.Text                               (Text)
import qualified Data.Text                               as Text

#ifdef CABAL
import qualified Data.Text.IO                            as Text
import           Paths_threepenny_gui
import           System.FilePath
import           System.IO.Unsafe
#else
import           Graphics.UI.Threepenny.Internal.Include
#endif


jsDriverCode    :: Text
defaultHtmlFile :: Text


#ifdef CABAL

jsDriverCode = unsafePerformIO $
   readFiles $ words "jquery.js jquery.cookie.js driver.hs" 

defaultHtmlFile = unsafePerformIO $
    readFiles ["index.html"]

readFiles files = do
    ys <- mapM (Text.readFile . getDataFile) files
    return $ Text.unlines ys

getDataFile x = unsafePerformIO $ fmap (</> x) getDataDir


#else

jsDriverCode = Text.unlines $ map Text.pack
    [ [include|Graphics/UI/jquery.js|]
    , [include|Graphics/UI/jquery.cookie.js|]
    , [include|Graphics/UI/driver.js|]
    ]

defaultHtmlFile = Text.pack [include|Graphics/UI/index.html|]

#endif