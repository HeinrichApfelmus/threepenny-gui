{-# LANGUAGE CPP, PackageImports #-}

import Control.Monad
import Data.IORef
import Data.Functor

#ifdef CABAL
import qualified "threepenny-gui" Graphics.UI.Threepenny as UI
import "threepenny-gui" Graphics.UI.Threepenny.Core
#else
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
#endif
import Paths
import System.FilePath

{-----------------------------------------------------------------------------
    Configuration
------------------------------------------------------------------------------}
bars  = 4
beats = 4
defaultBpm = 120

bpm2ms :: Int -> Int
bpm2ms bpm = ceiling $ 1000*60 / fromIntegral bpm

-- NOTE: Samples taken from "conductive-examples"

instruments = words "kick snare hihat"

loadInstrumentSample w name = do
    static <- getStaticDir
    loadFile w "audio/wav" (static </> name <.> "wav")

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    static <- getStaticDir
    startGUI Config
        { tpPort       = 10000
        , tpCustomHTML = Nothing
        , tpStatic     = static
        } setup

setup :: Window -> IO ()
setup w = void $ do
    return w # set title "Ha-ha-ha-ks-ks-ks-ha-ha-ha-ell-ell-ell"

    elBpm  <- UI.input # set value (show defaultBpm)
    elTick <- UI.span
    (kit, elInstruments) <- mkDrumKit w
    let status = grid [[UI.string "BPM:" , element elBpm]
                      ,[UI.string "Beat:", element elTick]]
    getBody w #+ [UI.div #. "wrap" #+ (status : map element elInstruments)]
    
    timer   <- UI.timer # set UI.interval (bpm2ms defaultBpm)
    refBeat <- newIORef 0
    
    -- play sounds on timer events
    on UI.tick timer $ const $ void $ do
        -- get and increase beat count
        beat <- readIORef refBeat
        writeIORef refBeat $ (beat + 1) `mod` (beats * bars)
        element elTick # set text (show $ beat + 1)
        
        -- play corresponding sounds
        sequence_ $ map (!! beat) kit
    
    -- allow user to set BPM
    on UI.keydown elBpm $ \keycode -> when (keycode == 13) $ void $ do
        bpm <- read <$> get value elBpm
        return timer # set UI.interval (bpm2ms bpm)
    
    -- star the timer
    UI.start timer


type Kit        = [Instrument]
type Instrument = [Beat]
type Beat       = IO ()         -- play the corresponding sound

mkDrumKit :: Window -> IO (Kit, [Element])
mkDrumKit w = unzip <$> mapM (mkInstrument w) instruments

mkInstrument :: Window -> String -> IO (Instrument, Element)
mkInstrument window name = do
    elCheckboxes <-
        sequence $ replicate bars  $
        sequence $ replicate beats $
            UI.input # set UI.type_ "checkbox"

    url     <- loadInstrumentSample window name
    elAudio <- UI.audio # set (attr "preload") "1" # set UI.src url

    let
        play box = do
            checked <- get UI.checked box
            when checked $ do
                audioStop elAudio -- just in case the sound is already playing
                audioPlay elAudio
        beats    = map play . concat $ elCheckboxes
        elGroups = [UI.span #. "bar" #+ map element bar | bar <- elCheckboxes]
    
    elInstrument <- UI.div #. "instrument"
        #+ (element elAudio : UI.string name : elGroups)
    
    return (beats, elInstrument)

