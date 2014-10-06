{-# LANGUAGE QuasiQuotes, CPP #-}
module Graphics.UI.Threepenny.Internal.Resources where

import           Data.Text                                       (Text)
import qualified Data.Text                               as Text
import           Graphics.UI.Threepenny.Internal.Include


jsDriverCode    :: Text
cssDriverCode   :: Text
defaultHtmlFile :: Text


jsDriverCode = Text.unlines $ map Text.pack
    [ [include|Graphics/UI/jquery.js|]
    , [include|Graphics/UI/jquery-cookie.js|]
    , [include|Graphics/UI/driver.js|]
    ]

cssDriverCode = Text.pack [include|Graphics/UI/driver.css|]

defaultHtmlFile = Text.pack [include|Graphics/UI/index.html|]


{-
-- Disabled code:
-- Do not embed files with executables.

import qualified Data.Text.IO                            as Text
import           System.FilePath
import           System.IO.Unsafe

#ifdef CABAL
import qualified Paths_threepenny_gui
getDataDir = fmap (</> "src" </> "Graphics" </> "UI") Paths_threepenny_gui.getDataDir

#else
getDataDir = return $ "Graphics" </> "UI"
#endif


jsDriverCode = unsafePerformIO $
    readFiles $ words "jquery.js jquery-cookie.js driver.js" 

cssDriverCode = unsafePerformIO $
    readFiles ["driver.css"]

defaultHtmlFile = unsafePerformIO $
    readFiles ["index.html"]

readFiles files = do
    ys <- mapM (Text.readFile . getDataFile) files
    return $ Text.unlines ys

getDataFile x = unsafePerformIO $ fmap (</> x) getDataDir

-}
