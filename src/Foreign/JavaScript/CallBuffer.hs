{-# LANGUAGE RecordWildCards #-}
module Foreign.JavaScript.CallBuffer where

import Control.Concurrent
import Control.Concurrent.STM as STM
import Control.Monad

import Foreign.JavaScript.Types

{-----------------------------------------------------------------------------
    Call Buffer
------------------------------------------------------------------------------}
-- | Set the call buffering mode for the given browser window.
setCallBufferMode :: Window -> CallBufferMode -> IO ()
setCallBufferMode w@Window{..} new = do
    flushCallBuffer w
    atomically $ writeTVar wCallBufferMode new

-- | Get the call buffering mode for the given browser window.
getCallBufferMode :: Window -> IO CallBufferMode
getCallBufferMode w@Window{..} = atomically $ readTVar wCallBufferMode

-- | Flush the call buffer,
-- i.e. send all outstanding JavaScript to the client in one single message.
flushCallBuffer :: Window -> IO ()
flushCallBuffer w@Window{..} = do
    code' <- atomically $ do
        code <- readTVar wCallBuffer
        writeTVar wCallBuffer id
        return code
    let code = code' ""
    unless (null code) $
        runEval code

-- Schedule a piece of JavaScript code to be run with `runEval`,
-- depending on the buffering mode
bufferRunEval :: Window -> String -> IO ()
bufferRunEval w@Window{..} code = do
    action <- atomically $ do
        mode <- readTVar wCallBufferMode
        case mode of
            NoBuffering -> do
                return $ Just code
            _ -> do
                msg <- readTVar wCallBuffer
                writeTVar wCallBuffer (msg . (\s -> ";" ++ code ++ s))
                return Nothing
    case action of
        Nothing   -> return ()
        Just code -> runEval code
