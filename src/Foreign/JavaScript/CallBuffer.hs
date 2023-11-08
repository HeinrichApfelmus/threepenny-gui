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
setCallBufferMode w new =
    flushCallBufferWithAtomic w $ writeTVar (wCallBufferMode w) new

-- | Get the call buffering mode for the given browser window.
getCallBufferMode :: Window -> IO CallBufferMode
getCallBufferMode Window{..} = atomically $ readTVar wCallBufferMode

-- | Flush the call buffer,
-- i.e. send all outstanding JavaScript to the client in one single message.
flushCallBuffer :: Window -> IO ()
flushCallBuffer w = flushCallBufferWithAtomic w $ return ()

-- | Flush the call buffer, and atomically perform an additional action
flushCallBufferWithAtomic :: Window -> STM a -> IO a
flushCallBufferWithAtomic Window{..} action = do
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
bufferRunEval Window{..} code = do
    action <- atomically $ do
        mode <- readTVar wCallBufferMode
        case mode of
            NoBuffering -> do
                return $ Just code
            _ -> do
                msg <- takeTMVar wCallBuffer
                putTMVar wCallBuffer (msg . (\s -> ";" ++ code ++ s))
                return Nothing
    case action of
        Nothing    -> return ()
        Just code1 -> runEval code1
