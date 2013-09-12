{-# LANGUAGE RecordWildCards #-}
module System.Mem.Coupon (
    -- * Synopsis
    -- | References to remote objects.
    -- Offers unique tokens ('Coupon') for communication.
    -- Supports garbage collection and finalizers.
    
    -- * Documentation
    Coupon,
    RemoteBooth, newRemoteBooth, lookup,
    
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
-- | Coupons can be used as a proxy for items.
-- The important point is that they can be serialized and sent
-- over a remote connection.
type Coupon = BS.ByteString

data Item a = Item
    { iKey    :: Unique -- Key suitable for weak pointers
    , iCoupon :: Coupon
    , iValue  :: a
    }

-- | Prize boothes keep track of items and coupons.
-- However, they do not keep items alive.
data RemoteBooth a = RemoteBooth
    { bCoupons :: MVar (Map Coupon (Weak (Item a)))
    , bCounter :: MVar [Integer]
    }

{-----------------------------------------------------------------------------
    Booth and Coupons
------------------------------------------------------------------------------}
-- | Create a new prize booth for creating items and trading coupons.
newRemoteBooth :: IO (RemoteBooth a)
newRemoteBooth = do
    bCounter <- newMVar [0..]
    bCoupons <- newMVar Map.empty
    return $ RemoteBooth {..}

-- | Take a coupon to the prize booth and maybe you'll get an item for it.
lookup :: Coupon -> RemoteBooth a -> IO (Maybe (Item a))
lookup coupon RemoteBooth{..} = do
    w <- Map.lookup coupon <$> readMVar bCoupons
    maybe (return Nothing) deRefWeak w

-- | Create a new item, which can be exchanged for coupons
-- at an associated prize booth.
--
-- The item can become unreachable,
-- at which point it will be garbage collected,
-- the finalizers will be run and any
-- coupon ceases to be valid.
-- 
-- The prize booth keeps track of coupons and items,
-- but does not keep the item alive.
newItem :: RemoteBooth a -> a -> IO (Item a)
newItem RemoteBooth{..} a = do
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
-- The coupon cannot be redeemed anymore when the finalizer runs.
addFinalizer :: Item a -> (Coupon -> a -> IO ()) -> IO ()
addFinalizer = undefined

-- | Perform an action with the item.
-- 
-- While the action is being performed, the item will not be garbage collected
-- and the coupon can be succesfully redeemed at the prize booth.
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

