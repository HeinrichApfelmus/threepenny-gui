{-# LANGUAGE RecursiveDo  #-}
module Reactive.Threepenny (
    -- * Synopsis
    -- | Functional reactive programming.

    -- * Types
    -- $intro
    Event, Behavior,

    -- * IO
    -- | Functions to connect events to the outside world.
    Handler, newEvent, register,
    currentValue,

    -- * Core Combinators
    -- | Minimal set of combinators for programming with 'Event' and 'Behavior'.
    module Control.Applicative,
    never, filterJust, unionWith,
    accumE, apply, stepper,
    -- $classes

    -- * Derived Combinators
    -- | Additional combinators that make programming
    -- with 'Event' and 'Behavior' convenient.
    -- ** Application
    (<@>), (<@),
    -- ** Filtering
    filterE, filterApply, whenE, split,
    -- ** Union
    unions, concatenate,
    -- ** Accumulation
    -- $accumulation
    accumB, mapAccum,

    -- * Additional Notes
    -- $recursion

    -- * Tidings
    Tidings, tidings, facts, rumors,

    -- * Internal
    -- | Functions reserved for special circumstances.
    -- Do not use unless you know what you're doing.
    onChange, unsafeMapIO, newEventsNamed,
    ) where

import Control.Applicative
import Control.Monad (void)
import Control.Monad.IO.Class
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
{- $intro

At its core, Functional Reactive Programming (FRP) is about two
data types 'Event' and 'Behavior' and the various ways to combine them.

-}

{-| @Event a@ represents a stream of events as they occur in time.
Semantically, you can think of @Event a@ as an infinite list of values
that are tagged with their corresponding time of occurence,

> type Event a = [(Time,a)]
-}
newtype Event    a = E { unE :: Memo (Pulse a) }

{-| @Behavior a@ represents a value that varies in time. Think of it as

> type Behavior a = Time -> a
-}
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
-- FIXME: Unregistering event handlers does not work yet.
register :: Event a -> Handler a -> IO (IO ())
register e h = do
    p <- at (unE e)     -- evaluate the memoized action
    Prim.addHandler p h

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
currentValue :: MonadIO m => Behavior a -> m a
currentValue (B l _) = liftIO $ Prim.readLatch l


{-----------------------------------------------------------------------------
    Core Combinators
------------------------------------------------------------------------------}
instance Functor Event where
    fmap f e = E $ liftMemo1 (Prim.mapP f) (unE e)

unsafeMapIO :: (a -> IO b) -> Event a -> Event b
unsafeMapIO f e = E $ liftMemo1 (Prim.unsafeMapIOP f) (unE e)

-- | Event that never occurs.
-- Think of it as @never = []@.
never :: Event a
never = E $ fromPure Prim.neverP

-- | Return all event occurrences that are 'Just' values, discard the rest.
-- Think of it as
--
-- > filterJust es = [(time,a) | (time,Just a) <- es]
filterJust e = E $ liftMemo1 Prim.filterJustP (unE e)

-- | Merge two event streams of the same type.
-- In case of simultaneous occurrences, the event values are combined
-- with the binary function.
-- Think of it as
--
-- > unionWith f ((timex,x):xs) ((timey,y):ys)
-- >    | timex == timey = (timex,f x y) : unionWith f xs ys
-- >    | timex <  timey = (timex,x)     : unionWith f xs ((timey,y):ys)
-- >    | timex >  timey = (timey,y)     : unionWith f ((timex,x):xs) ys
unionWith :: (a -> a -> a) -> Event a -> Event a -> Event a
unionWith f e1 e2 = E $ liftMemo2 (Prim.unionWithP f) (unE e1) (unE e2)

-- | Apply a time-varying function to a stream of events.
-- Think of it as
--
-- > apply bf ex = [(time, bf time x) | (time, x) <- ex]
apply :: Behavior (a -> b) -> Event a -> Event b
apply  f x        = E $ liftMemo1 (\p -> Prim.applyP (latch f) p) (unE x)

infixl 4 <@>, <@

-- | Infix synonym for 'apply', similar to '<*>'.
(<@>) :: Behavior (a -> b) -> Event a -> Event b
(<@>) = apply

-- | Variant of 'apply' similar to '<*'
(<@) :: Behavior a -> Event b -> Event a
b <@ e = (const <$> b) <@> e

-- | The 'accumB' function is similar to a /strict/ left fold, 'foldl''.
-- It starts with an initial value and combines it with incoming events.
-- For example, think
--
-- > accumB "x" [(time1,(++"y")),(time2,(++"z"))]
-- >    = stepper "x" [(time1,"xy"),(time2,"xyz")]
--
-- Note that the value of the behavior changes \"slightly after\"
-- the events occur. This allows for recursive definitions.
accumB :: MonadIO m => a -> Event (a -> a) -> m (Behavior a)
accumB a e = liftIO $ do
    (l1,p1) <- Prim.accumL a =<< at (unE e)
    p2      <- Prim.mapP (const ()) p1
    return $ B l1 (E $ fromPure p2)


-- | Construct a time-varying function from an initial value and
-- a stream of new values. Think of it as
--
-- > stepper x0 ex = return $ \time ->
-- >     last (x0 : [x | (timex,x) <- ex, timex < time])
--
-- Note that the smaller-than-sign in the comparision @timex < time@ means
-- that the value of the behavior changes \"slightly after\"
-- the event occurrences. This allows for recursive definitions.
stepper :: MonadIO m => a -> Event a -> m (Behavior a)
stepper a e = accumB a (const <$> e)

-- | The 'accumE' function accumulates a stream of events.
-- Example:
--
-- > accumE "x" [(time1,(++"y")),(time2,(++"z"))]
-- >    = return [(time1,"xy"),(time2,"xyz")]
--
-- Note that the output events are simultaneous with the input events,
-- there is no \"delay\" like in the case of 'accumB'.
accumE :: MonadIO m =>  a -> Event (a -> a) -> m (Event a)
accumE a e = liftIO $ do
    p <- fmap snd . Prim.accumL a =<< at (unE e)
    return $ E $ fromPure p

instance Functor Behavior where
    fmap f ~(B l e) = B (Prim.mapL f l) e

instance Applicative Behavior where
    pure a  = B (Prim.pureL a) never
    ~(B lf ef) <*> ~(B lx ex) =
        B (Prim.applyL lf lx) (unionWith const ef ex)

{- $classes

/Further combinators that Haddock can't document properly./

> instance Applicative Behavior

'Behavior' is an applicative functor. In particular, we have the following functions.

> pure :: a -> Behavior a

The constant time-varying value. Think of it as @pure x = \\time -> x@.

> (<*>) :: Behavior (a -> b) -> Behavior a -> Behavior b

Combine behaviors in applicative style.
Think of it as @bf \<*\> bx = \\time -> bf time $ bx time@.

-}

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

{-----------------------------------------------------------------------------
    Derived Combinators
------------------------------------------------------------------------------}
-- | Return all event occurrences that fulfill the predicate, discard the rest.
filterE :: (a -> Bool) -> Event a -> Event a
filterE p = filterJust . fmap (\a -> if p a then Just a else Nothing)

-- | Return all event occurrences that fulfill the time-varying predicate,
-- discard the rest. Generalization of 'filterE'.
filterApply :: Behavior (a -> Bool) -> Event a -> Event a
filterApply bp = fmap snd . filterE fst . apply ((\p a -> (p a,a)) <$> bp)

-- | Return event occurrences only when the behavior is 'True'.
-- Variant of 'filterApply'.
whenE :: Behavior Bool -> Event a -> Event a
whenE bf = filterApply (const <$> bf)

-- | Split event occurrences according to a tag.
-- The 'Left' values go into the left component while the 'Right' values
-- go into the right component of the result.
split :: Event (Either a b) -> (Event a, Event b)
split e = (filterJust $ fromLeft <$> e, filterJust $ fromRight <$> e)
    where
    fromLeft  (Left  a) = Just a
    fromLeft  (Right b) = Nothing
    fromRight (Left  a) = Nothing
    fromRight (Right b) = Just b

-- | Collect simultaneous event occurrences in a list.
unions :: [Event a] -> Event [a]
unions = foldr (unionWith (++)) never . map (fmap (:[]))

-- | Apply a list of functions in succession.
-- Useful in conjunction with 'unions'.
--
-- > concatenate [f,g,h] = f . g . h
concatenate :: [a -> a] -> (a -> a)
concatenate = foldr (.) id

{- $accumulation

Note: All accumulation functions are strict in the accumulated value!
acc -> (x,acc) is the order used by 'unfoldr' and 'State'.

-}

-- | Efficient combination of 'accumE' and 'accumB'.
mapAccum :: MonadIO m => acc -> Event (acc -> (x,acc)) -> m (Event x, Behavior acc)
mapAccum acc ef = do
    e <- accumE (undefined,acc) ((. snd) <$> ef)
    b <- stepper acc (snd <$> e)
    return (fst <$> e, b)


{-----------------------------------------------------------------------------
    Tidings

    Data type for combining user events.
    See <http://apfelmus.nfshost.com/blog/2012/03/29-frp-three-principles-bidirectional-gui.html>
    for more information.
------------------------------------------------------------------------------}
-- | Data type representing a behavior ('facts')
-- and suggestions to change it ('rumors').
data Tidings a = T { facts :: Behavior a, rumors :: Event a }

-- | Smart constructor. Combine facts and rumors into 'Tidings'.
tidings :: Behavior a -> Event a -> Tidings a
tidings b e = T b e

instance Functor Tidings where
    fmap f (T b e) = T (fmap f b) (fmap f e)

-- | The applicative instance combines 'rumors'
-- and uses 'facts' when some of the 'rumors' are not available.
instance Applicative Tidings where
    pure x  = T (pure x) never
    f <*> x = uncurry ($) <$> pair f x

pair :: Tidings a -> Tidings b -> Tidings (a,b)
pair (T bx ex) (T by ey) = T b e
    where
    b = (,) <$> bx <*> by
    x = flip (,) <$> by <@> ex
    y = (,) <$> bx <@> ey
    e = unionWith (\(x,_) (_,y) -> (x,y)) x y


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


