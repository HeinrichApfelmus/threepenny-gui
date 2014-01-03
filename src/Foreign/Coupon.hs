{-# LANGUAGE RecordWildCards, CPP #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Foreign.Coupon (
    -- * Synopsis
    -- | References to remote objects.
    -- Offers unique tokens ('Coupon') for communication.
    -- Supports garbage collection and finalizers.
    
    -- * Documentation
    Coupon,
    PrizeBooth, newPrizeBooth, lookup,
    
    Item, newItem, withItem,
    addFinalizer, destroy, addReachable, clearReachable,
    ) where

import Prelude hiding (lookup)
import Control.Concurrent
import Control.Monad
import Control.Exception (evaluate)

import qualified Data.ByteString.Char8 as BS
import Data.Functor
import Data.IORef
import qualified Data.Map as Map

import System.Mem.Weak hiding (addFinalizer)
import qualified System.Mem.Weak as Weak

import qualified GHC.Base  as GHC
import qualified GHC.Weak  as GHC
import qualified GHC.IORef as GHC
import qualified GHC.STRef as GHC

mkWeakIORefValue :: IORef a -> value -> IO () -> IO (Weak value)
mkWeakIORefValue r@(GHC.IORef (GHC.STRef r#)) v f = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)

#if defined(CABAL) || defined(FPCOMPLETE)
#if MIN_VERSION_base(4,6,0)
#else
atomicModifyIORef' = atomicModifyIORef
#endif
#endif

debug m = m
-- debug m = return ()

type Map = Map.Map

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
-- | Coupons can be used as a proxy for 'Item'.
--
-- The important point is that coupons can be serialized and sent
-- over a remote connection.
--
-- Coupons are in bijection with items:
-- Different coupons will yield different items while
-- the same item will always be associated to the same coupon.
type Coupon = BS.ByteString

-- | Items represent foreign objects.
-- The intended use case is that these objects do not live in RAM,
-- but are only accessible via a remote connection.
-- 
-- The foreign object can be accessed by means of the item data of type @a@.
type Item a = IORef (ItemData a)

data ItemData a = ItemData
    { self     :: Weak (Item a)
    , coupon   :: Coupon
    , value    :: a
    , children :: IORef [Weak (Item a)]
    }


-- | Remote boothes are a mapping from 'Coupon' to 'Item'.
--
-- Prize boothes are neutral concerning garbage collection,
-- they do not keep items alive.
-- Moreover, items will be deleted from the booth when they are garbage collected.
data PrizeBooth a = PrizeBooth
    { bCoupons :: MVar (Map Coupon (Weak (Item a)))
    , bCounter :: MVar [Integer]
    }

{-----------------------------------------------------------------------------
    Booth and Coupons
------------------------------------------------------------------------------}
-- | Create a new prize booth for creating items and trading coupons.
newPrizeBooth :: IO (PrizeBooth a)
newPrizeBooth = do
    bCounter <- newMVar [0..]
    bCoupons <- newMVar Map.empty
    return $ PrizeBooth {..}

-- | Take a coupon to the prize booth and maybe you'll get an item for it.
lookup :: Coupon -> PrizeBooth a -> IO (Maybe (Item a))
lookup coupon PrizeBooth{..} = do
    w <- Map.lookup coupon <$> readMVar bCoupons
    maybe (return Nothing) deRefWeak w

-- | Create a new item, which can be exchanged for a coupon
-- at an associated prize booth.
--
-- The item can become unreachable,
-- at which point it will be garbage collected,
-- the finalizers will be run and its
-- coupon ceases to be valid.
newItem :: PrizeBooth a -> a -> IO (Item a)
newItem PrizeBooth{..} value = do
    coupon   <- BS.pack . show <$> modifyMVar bCounter (\(n:ns) -> return (ns,n))
    children <- newIORef []
    let self = undefined
    item     <- newIORef ItemData{..}
    
    let finalize = modifyMVar bCoupons $ \m -> return (Map.delete coupon m, ())
    w <- mkWeakIORef item finalize
    modifyMVar bCoupons $ \m -> return (Map.insert coupon w m, ())
    atomicModifyIORef' item $ \itemdata -> (itemdata { self = w }, ())
    return item

{-----------------------------------------------------------------------------
    Items
------------------------------------------------------------------------------}
-- | Perform an action with the item.
-- 
-- While the action is being performed, it is ensured that the item
-- will not be garbage collected
-- and its coupon can be succesfully redeemed at the prize booth.
withItem :: Item a -> (Coupon -> a -> IO b) -> IO b
withItem item f = do
    ItemData{..} <- readIORef item
    b <- f coupon value
    touchItem item
    return b

-- | Make Sure that the item in question is alive
-- at the given place in the sequence of IO actions.
touchItem :: Item a -> IO ()
touchItem item = item `seq` return ()

-- | Destroy an item and run all finalizers for it.
-- Coupons for this item can no longer be redeemed.
destroy :: Item a -> IO ()
destroy item = finalize =<< self <$> readIORef item

-- | Add a finalizer that is run when the item is garbage collected.
--
-- The coupon cannot be redeemed anymore while the finalizer runs.
addFinalizer :: Item a -> IO () -> IO ()
addFinalizer item = void . mkWeakIORef item

-- | When dealing with several foreign objects,
-- it is useful to model dependencies between them.
--
-- After this operation, the second 'Item' will be reachable
-- whenever the first one is reachable.
-- For instance, you should call this function when the second foreign object
-- is actually a subobject of the first one.
--
-- Note: It is possible to model dependencies in the @parent@ data,
-- but the 'addReachable' method is preferrable,
-- as it allows all child object to be garbage collected at once.
addReachable :: Item a -> Item a -> IO ()
addReachable parent child = do
    w   <- mkWeakIORefValue parent child $ return ()
    ref <- children <$> readIORef parent
    atomicModifyIORef' ref $ \ws -> (w:ws, ())

-- | Clear all dependencies.
-- 
-- Reachability of this 'Item' no longer implies reachability
-- of other items, as formerly implied by calls to 'addReachable'.
clearReachable :: Item a -> IO ()
clearReachable item = do
    ref <- children <$> readIORef item
    xs  <- atomicModifyIORef' ref $ \xs -> ([], xs)
    mapM_ finalize xs
