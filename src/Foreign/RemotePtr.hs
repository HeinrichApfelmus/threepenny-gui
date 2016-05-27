{-# LANGUAGE RecordWildCards, CPP, ExistentialQuantification #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Foreign.RemotePtr (
    -- * Synopsis
    -- | Toolbox for managing remote objects in Haskell.
    
    -- * RemotePtr
    RemotePtr,
    withRemotePtr, addFinalizer, destroy, addReachable, clearReachable,
    unprotectedGetCoupon,

    -- * Coupons and Vendors
    Coupon, newCoupon,
    Vendor, newVendor, lookup,
    newRemotePtr,
    ) where

import Prelude hiding (lookup)
import Control.Monad
import           Control.Concurrent
import qualified Data.Text             as T
import qualified Data.Map              as Map
import Data.Functor
import Data.IORef

import           System.IO.Unsafe         (unsafePerformIO)
import           System.Mem.Weak          hiding (addFinalizer)
import qualified System.Mem.Weak  as Weak

import qualified GHC.Base  as GHC
import qualified GHC.Weak  as GHC
import qualified GHC.IORef as GHC
import qualified GHC.STRef as GHC

#if CABAL
#if MIN_VERSION_base(4,6,0)
#else
atomicModifyIORef' = atomicModifyIORef
#endif
#endif

mkWeakIORefValue :: IORef a -> value -> IO () -> IO (Weak value)
#if CABAL
#if MIN_VERSION_base(4,9,0)
mkWeakIORefValue r@(GHC.IORef (GHC.STRef r#)) v (GHC.IO f) = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)
#else
mkWeakIORefValue r@(GHC.IORef (GHC.STRef r#)) v f = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)
#endif
#else
mkWeakIORefValue r@(GHC.IORef (GHC.STRef r#)) v f = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)
#endif

type Map = Map.Map

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
-- | A 'Coupon' is a unique identifier.
-- 
-- It is a string of alphanumeric ASCII characters and it is intended to
-- be sent to or received from a remote program.
--
-- The data structure 'Vendor' associates 'Coupon's to 'RemotPtr' objects.
type Coupon = T.Text


-- | A 'RemotePtr' is a pointer to a foreign object.
-- 
-- Like a 'ForeignPtr', it refers to an object managed by an environment
-- external to the Haskell runtime.
-- Likewise, you can assign finalizers to a 'RemotePtr'. The finalizers
-- will be run when the Haskell runtime garbage collects this value.
-- They can perform some cleanup operations, like freeing memory.
--
-- Unlike a 'ForeignPtr', the object referenced by a 'RemotePtr' is not
-- necessarily a block of RAM. Instead, it can refer to things like an object
-- managed by a remote program.

type RemotePtr a = IORef (RemoteData a)

data RemoteData a = RemoteData
    { self     :: Weak (RemotePtr a)
    , coupon   :: Coupon
    , value    :: a
    , children :: IORef [SomeWeak]
    }

-- Existentially quantified weak pointer. We only care about its finalizer.
data SomeWeak = forall a. SomeWeak (Weak a)

-- | A 'Vendor' is a bijective mapping from 'Coupon' to 'RemotePtr'.
--
-- Every 'Coupon' has at most one 'RemotePtr' associated to it.
-- A single 'RemotePtr' will always be associated with the same 'Coupon'.

data Vendor a = Vendor
    { coupons :: MVar (Map Coupon (Weak (RemotePtr a)))
    , counter :: MVar [Integer]
    }

{-----------------------------------------------------------------------------
    Vendor and Coupons
------------------------------------------------------------------------------}
-- | Create a new 'Vendor' for trading 'Coupon's and 'RemotePtr's.
newVendor :: IO (Vendor a)
newVendor = do
    counter <- newMVar [0..]
    coupons <- newMVar Map.empty
    return $ Vendor {..}

-- | Take a 'Coupon' to a 'Vendor' and maybe you'll get a 'RemotePtr' for it.
lookup :: Coupon -> Vendor a -> IO (Maybe (RemotePtr a))
lookup coupon Vendor{..} = do
    w <- Map.lookup coupon <$> readMVar coupons
    maybe (return Nothing) deRefWeak w

-- | Create a new 'Coupon'.
--
-- WARNING: This coupon is only unique relative to this 'Vendor'.
-- There is no guarantee that this 'Coupon' is globally unique,
-- certainly not on a remote machine.
newCoupon :: Vendor a -> IO Coupon
newCoupon Vendor{..} =
    T.pack . show <$> modifyMVar counter (\(n:ns) -> return (ns,n))

-- | Create a new 'RemotePtr' from a 'Coupon' and register it with a 'Vendor'.
newRemotePtr :: Coupon -> a -> Vendor a -> IO (RemotePtr a)
newRemotePtr coupon value Vendor{..} = do
    children <- newIORef []
    let self = undefined
    ptr      <- newIORef RemoteData{..}
    
    let finalize = modifyMVar coupons $ \m -> return (Map.delete coupon m, ())
    w <- mkWeakIORef ptr finalize
    modifyMVar coupons $ \m -> return (Map.insert coupon w m, ())
    atomicModifyIORef' ptr $ \itemdata -> (itemdata { self = w }, ())
    return ptr

{-----------------------------------------------------------------------------
    RemotePtr
------------------------------------------------------------------------------}
-- | Access the data of the 'RemotePtr'.
-- 
-- While the action is being performed, it is ensured that the 'RemotePtr'
-- will not be garbage collected
-- and its 'Coupon' can be successfully redeemed at the 'Vendor'.
withRemotePtr :: RemotePtr a -> (Coupon -> a -> IO b) -> IO b
withRemotePtr ptr f = do
        RemoteData{..} <- readIORef ptr
        b <- f coupon value
        touch ptr
        return b
    where
    -- make sure that the pointer is alive at this point in the code
    touch ptr = void $ readIORef ptr

-- | Unprotected access the 'Coupon' of a 'RemotePtr'.
--
-- Note: There is no guarantee that the 'RemotePtr' is alive
-- after this operation and that the 'Coupon' can be redeemed at a 'Vendor'.
-- Most of the time, you should use 'withRemotePtr' instead.
--
-- Note: In particular, if you use this with @unsafePerformIO@,
-- the risk is high that you only refer to the 'RemotePtr' argument via
-- the result just obtained, and the pointer will be garbage collected.
unprotectedGetCoupon :: RemotePtr a -> IO Coupon
unprotectedGetCoupon ptr = coupon <$> readIORef ptr


-- | Add a finalizer that is run when the 'RemotePtr' is garbage collected.
--
-- The associated coupon cannot be redeemed anymore while the finalizer runs.
addFinalizer :: RemotePtr a -> IO () -> IO ()
addFinalizer ptr = void . mkWeakIORef ptr
-- | FIXME: Is this finalizer really run when 'destroy' is called?

-- | Destroy a 'RemotePtr' and run all finalizers for it.
-- 'Coupon's for this pointer can no longer be redeemed.
destroy :: RemotePtr a -> IO ()
destroy ptr = finalize =<< self <$> readIORef ptr


-- | When dealing with several foreign objects,
-- it is useful to model dependencies between them.
--
-- After this operation, the second 'RemotePtr' will be reachable
-- whenever the first one is reachable.
-- For instance, you should call this function when the second foreign object
-- is actually a subobject of the first one.
--
-- Note: It is possible to model dependencies in the @parent@ data,
-- but the 'addReachable' method is preferrable,
-- as it allows all child object to be garbage collected at once.
addReachable :: RemotePtr a -> RemotePtr b -> IO ()
addReachable parent child = do
    w   <- mkWeakIORefValue parent child $ return ()
    ref <- children <$> readIORef parent
    atomicModifyIORef' ref $ \ws -> (SomeWeak w:ws, ())

-- | Clear all dependencies.
-- 
-- Reachability of this 'RemotePtr' no longer implies reachability
-- of other items, as formerly implied by calls to 'addReachable'.
clearReachable :: RemotePtr a -> IO ()
clearReachable parent = do
    ref <- children <$> readIORef parent
    xs  <- atomicModifyIORef' ref $ \xs -> ([], xs)
    sequence_ [finalize x | SomeWeak x <- xs]
