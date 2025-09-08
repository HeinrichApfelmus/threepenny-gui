{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}

module Foreign.JavaScript.CallBuffer
    ( setCallBufferMode
    , getCallBufferMode
    , flushCallBuffer
    , bufferRunEval
    ) where

import Foreign.JavaScript.Types

{-----------------------------------------------------------------------------
   GHC
------------------------------------------------------------------------------}
import Control.Concurrent.STM as STM
import Control.Monad

-- | Set the call buffering mode for the given browser window.
setCallBufferMode :: Window -> CallBufferMode -> IO ()
setCallBufferMode w new =
    flushCallBufferWithAtomic w $ writeTVar (wCallBufferMode w) new

-- | Get the call buffering mode for the given browser window.
getCallBufferMode :: Window -> IO CallBufferMode
getCallBufferMode Window{wCallBufferMode} =
    atomically $ readTVar wCallBufferMode

-- | Flush the call buffer,
-- i.e. send all outstanding JavaScript to the client in one single message.
flushCallBuffer :: Window -> IO ()
flushCallBuffer w = flushCallBufferWithAtomic w $ pure ()

-- | Flush the call buffer, and atomically perform an additional action
flushCallBufferWithAtomic :: Window -> STM a -> IO a
flushCallBufferWithAtomic Window{wCallBuffer,runEval} action = do
    -- by taking the call buffer, we ensure that no further code
    -- is added to the buffer while we execute the current buffer's code.
    code' <- atomically $ takeTMVar wCallBuffer
    let code = code' ""
    unless (null code) $ runEval code
    atomically $ do
        putTMVar wCallBuffer id
        action

-- | Schedule a piece of JavaScript code to be run with `runEval`,
-- depending on the buffering mode
bufferRunEval :: Window -> String -> IO ()
bufferRunEval Window{wCallBufferMode,wCallBuffer,runEval} code = do
    action <- atomically $ do
        mode <- readTVar wCallBufferMode
        case mode of
            NoBuffering -> do
                pure $ Just code
            _ -> do
                msg <- takeTMVar wCallBuffer
                putTMVar wCallBuffer (msg . (\s -> ";" ++ code ++ s))
                pure Nothing
    case action of
        Nothing    -> pure ()
        Just code1 -> runEval code1

