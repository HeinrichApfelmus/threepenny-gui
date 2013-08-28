module Reactive.Threepenny (
    -- * Synopsis
    -- | Functional reactive programming.
    --
    -- Note: Basic functionality should work,
    -- but recursion does not work yet
    -- and there may be some unexpected surprises
    -- when attaching new behaviors amd events after some
    -- events have already occured
    -- ("dynamic event switching").
    
    -- * Types
    Handler, Event,
    newEvent, newEventsNamed, register,
    
    Behavior, currentValue,
    
    -- * Combinators
    never, filterJust, unionWith,
    accumE, accumB, stepper, apply,
    module Control.Applicative,
    
    -- * Internal
    onChange,
    ) where

import Control.Applicative
import Data.IORef
import qualified Data.Map as Map

import           Reactive.Threepenny.Memo       as Memo
import qualified Reactive.Threepenny.PulseLatch as Prim

type Pulse = Prim.Pulse
type Latch = Prim.Latch
type Map   = Map.Map

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
newtype Event    a = E { unE :: Memo (Pulse a)           }
newtype Behavior a = B { unB :: Memo (Latch a, Pulse ()) }

{-----------------------------------------------------------------------------
    IO
------------------------------------------------------------------------------}
-- | An /event handler/ is a function that takes an
-- /event value/ and performs some computation.
type Handler a = a -> IO ()

-- | Create a new event.
-- Also returns a function that triggers an event occurrence.
newEvent :: IO (Event a, Handler a)
newEvent = do
    (p, fire) <- Prim.newPulse
    return (E $ fromPure p, fire)


-- | Create a series of events with delayed initialization.
--
-- For each name, the initialization handler will be called
-- exactly once when the event is first "brought to life",
-- e.g. when an event handler is registered to it.
newEventsNamed :: Ord name
    => Handler (name, Event a, Handler a)   -- ^ Initialization procedure.
    -> IO (name -> Event a)                 -- ^ Series of events.
newEventsNamed init = do
    eventsRef <- newIORef Map.empty
    return $ \name -> E $ memoize $ do
        events <- readIORef eventsRef
        case Map.lookup name events of
            Just p  -> return p
            Nothing -> do
                (p, fire) <- Prim.newPulse
                writeIORef eventsRef $ Map.insert name p events
                init (name, E $ fromPure p, fire)
                return p


-- | Register an event 'Handler' for an 'Event'.
-- All registered handlers will be called whenever the event occurs.
-- 
-- When registering an event handler, you will also be given an action
-- that unregisters this handler again.
-- 
-- > do unregisterMyHandler <- register event myHandler
--
-- FIXME: This does not currently work.
register :: Event a -> Handler a -> IO (IO ())
register e h = do
    p <- at (unE e)     -- evaluate the memoized action
    Prim.addHandler p h
    return $ return ()  -- FIXME

-- | Register an event 'Handler' for a 'Behavior'.
-- All registered handlers will be called whenever the behavior changes.
-- 
-- However, note that this is only an approximation,
-- as behaviors may change continuously.
-- Consequently, handlers should be idempotent.
onChange :: Behavior a -> Handler a -> IO ()
onChange b h = do
    (l,p) <- at (unB b)
    -- This works because latches are updated before the handlers are being called.
    Prim.addHandler p (\_ -> h =<< Prim.readLatch l)

-- | Read the current value of a 'Behavior'.
currentValue :: Behavior a -> IO a
currentValue b = do
    (l, p) <- at (unB b)
    Prim.readLatch l


{-----------------------------------------------------------------------------
    Combinators
------------------------------------------------------------------------------}
instance Functor Event where
    fmap f e = E $ liftMemo1 (Prim.mapP f) (unE e)

never             = E $ fromPure Prim.neverP

-- | Keep only those event values that are of the form 'Just'.
filterJust e      = E $ liftMemo1 Prim.filterJustP    (unE e)
unionWith f e1 e2 = E $ liftMemo2 (Prim.unionWithP f) (unE e1) (unE e2)

apply  f x  = E $ liftMemo2 (\(l,_) p -> Prim.applyP l p) (unB f) (unE x)
accumB a e  = B $ liftMemo1 (accumL a) (unE e)
    where
    accumL a p1 = do
        (l,p2) <- Prim.accumL a p1
        p3     <- Prim.mapP (const ()) p2
        return (l,p3)

stepper :: a -> Event a -> Behavior a
stepper a e = accumB a (const <$> e)

accumE :: a -> Event (a -> a) -> Event a
accumE a e = E $ liftMemo1 (fmap snd . Prim.accumL a) (unE e)

instance Functor Behavior where
    fmap f b = B $ memoize $ do
        (l1,p1) <- at (unB b)
        l2      <- Prim.mapL f l1
        return (l2,p1)

instance Applicative Behavior where
    pure a  = B $ fromPure (Prim.pureL a,Prim.neverP)
    f <*> x = B $ liftMemo2 applyB (unB f) (unB x)
        where
        applyB (l1,p1) (l2,p2) = do
            p3 <- Prim.unionWithP const p1 p2
            l3 <- Prim.applyL l1 l2
            return (l3,p3)


{-----------------------------------------------------------------------------
    Test
------------------------------------------------------------------------------}
test :: IO (Int -> IO ())
test = do
    (e1,fire) <- newEvent
    let e2 = accumE 0 ((+) <$> e1)
    _ <- register e2 print
    
    return fire




