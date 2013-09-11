{-# LANGUAGE RecordWildCards #-}
module System.Mem.RemotePtr (
    -- * Synopsis
    -- | References to remote objects that need to be garbage collected.
    
    -- * Documentation
    -- ** Types
    RemotePtr, Finalizer,
    
    -- ** High-level
    newRemotePtr, withRemotePtr,
    finalizeRemotePtr,
    addReachable, clearReachable,
    
    -- ** Low-level
    touchRemotePtr,
    
    ) where


import Control.Concurrent
import Control.Monad
import Control.Exception (evaluate)
import Data.Unique.Really

import System.Mem.Weak


{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}

-- | The type 'RemotePtr' is similar to the type 'ForeignPtr' and
-- represents references to objects that are maintained
-- in a foreign language.
-- However, the objects are generally not located in RAM,
-- but can for example correspond to data on a remote server.
-- The type @a@ is a Haskell proxy for the remote object.
-- for instance a socket connection to the remote server.
-- 
-- The main purpose of the 'RemotePtr' is to have reliable garbage collection
-- for this scenario. In particular, you can attach finalizers to 'RemotePtr',
-- which will be run when the pointer becomes unreachable.
-- For instance, this allows you to tell the server to release the remote object.
--
-- Note: While finalizers are usually run at some point after the 
-- pointer has become unreachable, there is no guarantee of promptness.
-- Also, no attempt is made to run remaining finalizers when
-- the program exits.
data RemotePtr a = RemotePtr
    { name     :: Unique    -- Key that is suitable for use with weak pointers.
    , ptr      :: Weak a
    , children :: MVar [Weak Unique]
    }

-- | Finalize a remote object given its proxy data.
type Finalizer a = a -> IO ()

{-----------------------------------------------------------------------------
    High-level
------------------------------------------------------------------------------}
-- | Create a new 'RemotePtr' with a finalizer attached.
newRemotePtr :: Finalizer a -> a -> IO (RemotePtr a)
newRemotePtr finalizer a = do
    name     <- newUnique
    ptr      <- mkWeak name a (Just $ finalizer a)
    children <- newMVar []
    return $ RemotePtr {..}

-- | This is a way to manipulating a remote object.
-- 
-- The remote object is kept alive at least during the whole action,
-- even if it is not used directly inside.
-- Note that it is not safe to return the data from the action and
-- use it after the action completes -- the remote object may have been finalized
-- by then.
withRemotePtr :: RemotePtr a -> (a -> IO b) -> IO b
withRemotePtr p f = do
    Just a <- deRefWeak (ptr p)
    b <- f a
    touchRemotePtr p
    return b

-- | Causes the finalizers attached to a remote pointer to be run immediately.
finalizeRemotePtr :: RemotePtr a -> IO ()
finalizeRemotePtr = finalize . ptr


-- | When dealing with several remote objects,
-- it is useful to model dependencies between them.
--
-- After this operation, the second pointer will be reachable
-- whenever the first one is reachable.
-- For instance, you should call this function when the second remote object
-- is actually a subobject of the first one.
--
-- Note: It is possible to model dependencies by
-- adding remote pointers in the @parent@ data,
-- but the 'addReachable' method is preferrable,
-- as it allows all child object to be garbage collected at once.
addReachable :: RemotePtr parent -> RemotePtr child -> IO ()
addReachable parent child = do
    w  <- mkWeak (name parent) (name child) Nothing
    ws <- takeMVar (children parent)
    putMVar (children parent) (w:ws)

-- | Clear all dependencies.
-- Reachability of this remote pointer no longer implies reachability
-- of other remote pointers (except through the @a@ data).
clearReachable :: RemotePtr a -> IO ()
clearReachable p = do
    xs <- takeMVar (children p)
    mapM finalize xs
    putMVar (children p) []


{-----------------------------------------------------------------------------
    Low-level
------------------------------------------------------------------------------}
-- | This function ensures that the remote object in question is alive
-- at the given place in the sequence of IO actions.
-- In particular, 'withRemotePtr' does a 'touchRemotePtr'
-- after it executes the user action.
touchRemotePtr :: RemotePtr a -> IO ()
touchRemotePtr x = void $ evaluate x
