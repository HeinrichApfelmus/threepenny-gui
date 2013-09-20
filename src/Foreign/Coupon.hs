{-# LANGUAGE RecordWildCards #-}
module Foreign.Coupon (
    -- * Synopsis
    -- | References to remote objects.
    -- Offers unique tokens ('Coupon') for communication.
    -- Supports garbage collection and finalizers.
    
    -- * Documentation
    Coupon,
    PrizeBooth, newPrizeBooth, lookup,
    
    Item, newItem, addFinalizer, destroy, withItem,
    getCoupon, getValue,
    ) where

import Prelude hiding (lookup)
import Control.Concurrent
import Control.Monad
import Control.Exception (evaluate)

import Data.String as BS
import qualified Data.ByteString as BS
import Data.Functor
import qualified Data.Map as Map
import Data.Unique.Really

import System.Mem.Weak hiding (addFinalizer)

type Map = Map.Map

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
-- | Coupons can be used as a proxy for 'Item'.
--
-- The important point is that coupons can be serialized and sent
-- over a remote connection.
type Coupon = BS.ByteString

-- | Items represent foreign objects.
-- 
-- The foreign object can be accessed by means of the item data of type @a@.
data Item a = Item
    { iKey    :: Unique -- Key suitable for weak pointers
    , iCoupon :: Coupon
    , iValue  :: a
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
newItem PrizeBooth{..} a = do
    iCoupon <- BS.fromString . show <$> modifyMVar bCounter (\(n:ns) -> return (ns,n))
    iKey    <- newUnique
    let iValue = a
    let item   = Item {..}
    modifyMVar bCoupons $ \m -> do
        w <- mkWeak iKey item Nothing
        return (Map.insert iCoupon w m, ())
    return item

{-----------------------------------------------------------------------------
    Items
------------------------------------------------------------------------------}
-- | Destroy an item and run all finalizers for it.
-- Coupons for this item can no longer be redeemed.
destroy :: Item a -> IO ()
destroy = undefined

-- | Add a finalizer that is run when the item is garbage collected.
--
-- The coupon cannot be redeemed anymore while the finalizer runs.
addFinalizer :: Item a -> (Coupon -> a -> IO ()) -> IO ()
addFinalizer = undefined

-- | Perform an action with the item.
-- 
-- While the action is being performed, it is ensured that the item
-- will not be garbage collected
-- and its coupon can be succesfully redeemed at the prize booth.
withItem :: Item a -> (Coupon -> a -> IO b) -> IO b
withItem item f = do
    b <- f (getCoupon item) (getValue item)
    touchItem item
    return b

-- | Make Sure that the item in question is alive
-- at the given place in the sequence of IO actions.
touchItem :: Item a -> IO ()
touchItem = void . evaluate

-- | Get the coupon for an item.
--
-- Coupons are in bijection with items:
-- Different coupons will yield different items while
-- the same item will always be associated to the same coupon.
getCoupon :: Item a -> Coupon
getCoupon = iCoupon

-- | Retrieve item data.
getValue :: Item a -> a
getValue = iValue

