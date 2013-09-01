{-# LANGUAGE RecursiveDo  #-}
module Reactive.Threepenny (
    -- * Synopsis
    -- | Functional reactive programming.
    
    -- * Types
    Handler, Event,
    newEvent, newEventsNamed, register,
    
    Behavior, currentValue,
    
    -- * Combinators
    never, filterJust, unionWith,
    accumE, accumB, stepper, apply, (<@>), (<@),
    module Control.Applicative,

    -- * Additional Notes
    -- $recursion
    
    -- * Internal
    onChange,
    ) where

import Control.Applicative
import Control.Monad (void)
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
newtype Event    a = E { unE :: Memo (Pulse a) }
data    Behavior a = B { latch :: Latch a, changes :: Event () }

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
onChange (B l e) h = void $ do
    -- This works because latches are updated before the handlers are being called.
    register e (\_ -> h =<< Prim.readLatch l)

-- | Read the current value of a 'Behavior'.
currentValue :: Behavior a -> IO a
currentValue (B l _) = Prim.readLatch l


{-----------------------------------------------------------------------------
    Combinators
------------------------------------------------------------------------------}
instance Functor Event where
    fmap f e = E $ liftMemo1 (Prim.mapP f) (unE e)

never             = E $ fromPure Prim.neverP

-- | Keep only those event values that are of the form 'Just'.
filterJust e      = E $ liftMemo1 Prim.filterJustP    (unE e)
unionWith f e1 e2 = E $ liftMemo2 (Prim.unionWithP f) (unE e1) (unE e2)

{- $recursion
Recursion in the 'IO' monad is possible, but somewhat limited.
The main rule is that the sequence of IO actions must be known
in advance, only the values may be recursive.

Good:

> mdo
>     let e2 = apply (const <$> b) e1   -- applying a behavior is not an IO action
>     b <- accumB $ (+1) <$ e2

Bad:
 
> mdo
>     b <- accumB $ (+1) <$ e2          -- actions executed here could depend ...
>     let e2 = apply (const <$> b) e1   -- ... on this value
-}

-- | Apply the current value of the behavior whenever the event occurs.
--
-- Note that behaviors created with 'stepper' or 'accumB' are not updated
-- until shortly /after/ their creating event has occurred.
-- This allows for recursive use.
apply :: Behavior (a -> b) -> Event a -> Event b
apply  f x        = E $ liftMemo1 (\p -> Prim.applyP (latch f) p) (unE x)

infixl 4 <@>, <@

-- | Infix synonym for 'apply', similar to '<$>'.
(<@>) :: Behavior (a -> b) -> Event a -> Event b
(<@>) = apply

-- | Variant of 'apply' similar to '<$'
(<@) :: Behavior a -> Event b -> Event a
b <@ e = (const <$> b) <@> e

accumB :: a -> Event (a -> a) -> IO (Behavior a)
accumB a e = do
    (l1,p1) <- Prim.accumL a =<< at (unE e)
    p2      <- Prim.mapP (const ()) p1
    return $ B l1 (E $ fromPure p2)

stepper :: a -> Event a -> IO (Behavior a)
stepper a e = accumB a (const <$> e)

accumE :: a -> Event (a -> a) -> IO (Event a)
accumE a e = do
    p <- fmap snd . Prim.accumL a =<< at (unE e)
    return $ E $ fromPure p

instance Functor Behavior where
    fmap f ~(B l e) = B (Prim.mapL f l) e

instance Applicative Behavior where
    pure a  = B (Prim.pureL a) never
    ~(B lf ef) <*> ~(B lx ex) =
        B (Prim.applyL lf lx) (unionWith const ef ex)

{-----------------------------------------------------------------------------
    Test
------------------------------------------------------------------------------}
test :: IO (Int -> IO ())
test = do
    (e1,fire) <- newEvent
    e2 <- accumE 0 $ (+) <$> e1
    _  <- register e2 print
    
    return fire
    
test_recursion1 :: IO (IO ())
test_recursion1 = mdo
    (e1, fire) <- newEvent
    let e2 :: Event Int
        e2 = apply (const <$> b) e1
    b  <- accumB 0 $ (+1) <$ e2
    _  <- register e2 print
    
    return $ fire ()


