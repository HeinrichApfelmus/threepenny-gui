-- | Delay a thread for n seconds or minutes.

module Control.Concurrent.Delay
  (delaySeconds
  ,delayMinutes)
  where

import Control.Concurrent

-- | Delay the current thread for at least n seconds.
delaySeconds :: Integer -> IO ()
delaySeconds 0 = return ()
delaySeconds n = do threadDelay (1000 * 1000); delaySeconds (n-1)

-- | Delay the current thread for at least n minutes.
delayMinutes :: Integer -> IO ()
delayMinutes = delaySeconds . (*60)
