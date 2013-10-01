module Graphics.UI.Threepenny.MonadUI where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Trans.RWS.Lazy

import Graphics.UI.Threepenny.Internal.Types (Window)

{-----------------------------------------------------------------------------
    UI monad
------------------------------------------------------------------------------}
{- |

User interface elements are created and manipulated in the 'UI' monad.

This monad is essentially just a thin wrapper around the familiar 'IO' monad.
Use the 'liftIO' function to access 'IO' operations like reading
and writing from files.

There are several subtle reasons why Threepenny
uses a custom 'UI' monad instead of the standard 'IO' monad:

* More convenience when calling JavaScript.

* Recursion for functional reactive programming.

-}
newtype UI a = UI { unUI :: RWST Window [IO ()] () IO a }

instance Functor UI where
    fmap f = UI . fmap f . unUI

instance Monad UI where
    return  = UI . return
    m >>= k = UI $ unUI m >>= unUI . k

instance MonadIO UI where
    liftIO = UI . liftIO

instance MonadFix UI where
    mfix f = UI $ mfix (unUI . f)  

-- | Execute an 'UI' action in a particular browser window.
runUI :: Window -> UI a -> IO a
runUI window m = do
    (a, _, actions) <- runRWST (unUI m) window ()
    sequence_ actions
    return a

getWindowUI :: UI Window
getWindowUI = UI ask

liftIOLater :: IO () -> UI ()
liftIOLater x = UI $ tell [x]

