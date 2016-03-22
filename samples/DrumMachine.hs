import Control.Monad
import Data.IORef
import Data.Functor

import Paths
import System.FilePath

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

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

loadInstrumentSample name = return $ "static/" ++ name ++ ".wav"

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = do
    static <- getStaticDir
    startGUI defaultConfig { jsStatic = Just static } setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "Ha-ha-ha-ks-ks-ks-ha-ha-ha-ell-ell-ell"

    elBpm  <- UI.input # set value (show defaultBpm)
    elTick <- UI.span
    (kit, elInstruments) <- mkDrumKit
    let status = grid [[UI.string "BPM:" , element elBpm]
                      ,[UI.string "Beat:", element elTick]]
    getBody w #+ [UI.div #. "wrap" #+ (status : map element elInstruments)]
    
    timer <- UI.timer # set UI.interval (bpm2ms defaultBpm)
    eBeat <- accumE (0::Int) $
        (\beat -> (beat + 1) `mod` (beats * bars)) <$ UI.tick timer
    void . onEvent eBeat $ \beat -> do
        -- display beat count
        element elTick # set text (show $ beat + 1)
        -- play corresponding sounds
        sequence_ $ map (!! beat) kit
    
    -- allow user to set BPM
    on UI.keydown elBpm $ \keycode -> when (keycode == 13) $ void $ do
        bpm <- read <$> get value elBpm
        return timer # set UI.interval (bpm2ms bpm)
    
    -- start the timer
    UI.start timer


type Kit        = [Instrument]
type Instrument = [Beat]
type Beat       = UI ()         -- play the corresponding sound

mkDrumKit :: UI (Kit, [Element])
mkDrumKit = unzip <$> mapM mkInstrument instruments

mkInstrument :: String -> UI (Instrument, Element)
mkInstrument name = do
    elCheckboxes <-
        sequence $ replicate bars  $
        sequence $ replicate beats $
            UI.input # set UI.type_ "checkbox"

    url     <- loadInstrumentSample name
    elAudio <- UI.audio # set (attr "preload") "1" # set UI.src url

    let play box = do
            checked <- get UI.checked box
            when checked $ do
                runFunction $ ffi "%1.pause()" elAudio
                runFunction $ ffi "%1.currentTime = 0" elAudio 
                runFunction $ ffi "%1.play()" elAudio
        beats    = map play . concat $ elCheckboxes
        elGroups = [UI.span #. "bar" #+ map element bar | bar <- elCheckboxes]
    
    elInstrument <- UI.div #. "instrument"
        #+ (element elAudio : UI.string name : elGroups)
    
    return (beats, elInstrument)

