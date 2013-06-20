{-# LANGUAGE CPP, PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
module VideoAnnotate.MediaPlayer where

import Control.Monad
import Control.Monad.IO.Class

#ifdef CABAL
import "threepenny-gui" Graphics.UI.Threepenny
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Elements as UI
#endif

{-----------------------------------------------------------------------------
    Video Player
------------------------------------------------------------------------------}
type Time = Int -- milliseconds

data MediaPlayer = MediaPlayer
    { mpVideo  :: Element
    }

mkMediaPlayer :: Dom MediaPlayer
mkMediaPlayer = do
    -- FIXME: video player may not have controls
    mpVideo  <- UI.video # set (attr "controls") "1"
    return $ MediaPlayer{..}

play  :: MediaPlayer -> IO ()
play = undefined

pause :: MediaPlayer -> IO ()
pause = undefined

position :: WriteAttr MediaPlayer Time
position = undefined

source :: WriteAttr MediaPlayer String
source = mkWriteAttr $ \src MediaPlayer{..} -> void $
    element mpVideo # set (attr "src") src
--        # set (attr "type") "video/mp4"

duration :: ReadAttr MediaPlayer Time
duration = undefined

view :: MonadIO m => MediaPlayer -> m Element
view = element . mpVideo


