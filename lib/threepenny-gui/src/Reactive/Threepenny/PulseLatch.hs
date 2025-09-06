{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RecursiveDo #-}
module Reactive.Threepenny.PulseLatch (
    Pulse, newPulse, addHandler,
    neverP, mapP, filterJustP, unionWithP, unsafeMapIOP,
    
    Latch,
    pureL, mapL, applyL, accumL, applyP,
    readLatch,
    
    -- * Internal
    test, test_recursion1
    ) where


import Control.Monad (join, void)
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS     as Monad

import Data.IORef

import qualified Data.Vault.Strict      as Vault
import           Data.Unique.Really
import qualified Data.Unique.Really.Map as Map

import Reactive.Threepenny.Monads
import Reactive.Threepenny.Types

#if defined(__MHS__)
modifyIORef' :: IORef a -> (a -> a) -> IO ()
modifyIORef' ref f = atomicModifyIORef ref (\a -> (f a, ()))
#endif

{-----------------------------------------------------------------------------
    Pulse
------------------------------------------------------------------------------}
-- Turn evaluation action into pulse that caches the value.
cacheEval :: EvalP (Maybe a) -> Build (Pulse a)
cacheEval e = do
    key <- Vault.newKey
    pure $ Pulse
        { addHandlerP = \_ -> pure (pure ())
        , evalP       = do
            vault <- Monad.get
            case Vault.lookup key vault of
                Just a  -> pure a
                Nothing -> do
                    a <- e
                    Monad.put $ Vault.insert key a vault
                    pure a
        }

-- Add a dependency to a pulse, for the sake of keeping track of dependencies.
dependOn :: Pulse a -> Pulse b -> Pulse a
dependOn p q = p { addHandlerP = \h -> (>>) <$> addHandlerP p h <*> addHandlerP q h }

-- Execute an action when the pulse occurs
whenPulse :: Pulse a -> (a -> IO ()) -> Handler
whenPulse p f = do
    ma <- evalP p
    pure $ case ma of
        Just a  -> f a
        Nothing -> pure ()

{-----------------------------------------------------------------------------
    Interface to the outside world.
------------------------------------------------------------------------------}
-- | Create a new pulse and a function to trigger it.
newPulse :: Build (Pulse a, a -> IO ())
newPulse = do
    key         <- Vault.newKey
    handlersRef <- newIORef Map.empty      -- map of handlers

    let
        -- add handler to map
        addHandlerP :: (Unique, Priority, Handler) -> Build (IO ())
        addHandlerP (uid,p,m) = do
            modifyIORef' handlersRef (Map.insert uid (p, m))
            pure $ modifyIORef' handlersRef (Map.delete uid)

        -- evaluate all handlers attached to this input pulse
        fireP a = do
            let pulses = Vault.insert key (Just a) $ Vault.empty
            handlers <- readIORef handlersRef
            (ms, _)  <- runEvalP pulses $ sequence $ 
                   [m | (DoLatch, m) <- Map.elems handlers]
                ++ [m | (DoIO   , m) <- Map.elems handlers]  
            sequence_ ms

        evalP = join . Vault.lookup key <$> Monad.get

    pure (Pulse {..}, fireP)

-- | Register a handler to be executed whenever a pulse occurs.
addHandler :: Pulse a -> (a -> IO ()) -> Build (IO ())
addHandler p f = do
    uid <- newUnique
    addHandlerP p (uid, DoIO, whenPulse p f)

-- | Read the value of a 'Latch' at a particular moment in Build.
readLatch :: Latch a -> Build a
readLatch = readL

{-----------------------------------------------------------------------------
    Pulse and Latch
    Public API
------------------------------------------------------------------------------}
-- | Create a new pulse that never occurs.
neverP :: Pulse a
neverP = Pulse
    { addHandlerP = const $ pure (pure ())
    , evalP       = pure Nothing
    }

-- | Map a function over pulses.
mapP :: (a -> b) -> Pulse a -> Build (Pulse b)
mapP f p = (`dependOn` p) <$> cacheEval (pure . fmap f =<< evalP p)

-- | Map an IO function over pulses. Is only executed once.
unsafeMapIOP :: (a -> IO b) -> Pulse a -> Build (Pulse b)
unsafeMapIOP f p = (`dependOn` p) <$> cacheEval (traverse' . fmap f =<< evalP p)
    where
    traverse' :: Maybe (IO a) -> EvalP (Maybe a)
    traverse' Nothing  = pure Nothing
    traverse' (Just m) = Just <$> lift m

-- | Filter occurrences. Only keep those of the form 'Just'.
filterJustP :: Pulse (Maybe a) -> Build (Pulse a)
filterJustP p = (`dependOn` p) <$> cacheEval (pure . join =<< evalP p)

-- | Pulse that occurs when either of the pulses occur.
-- Combines values with the indicated function when both occur.
unionWithP :: (a -> a -> a) -> Pulse a -> Pulse a -> Build (Pulse a)
unionWithP f p q = (`dependOn` q) . (`dependOn` p) <$> cacheEval eval
    where
    eval = do
        x <- evalP p
        y <- evalP q
        pure $ case (x,y) of
            (Nothing, Nothing) -> Nothing
            (Just a , Nothing) -> Just a
            (Nothing, Just a ) -> Just a
            (Just a1, Just a2) -> Just $ f a1 a2

-- | Apply the current latch value whenever the pulse occurs.
applyP :: Latch (a -> b) -> Pulse a -> Build (Pulse b)
applyP l p = (`dependOn` p) <$> cacheEval eval
    where
    eval = do
        f <- lift $ readL l
        a <- evalP p
        pure $ f <$> a

-- | Accumulate values in a latch.
accumL :: a -> Pulse (a -> a) -> Build (Latch a, Pulse a)
accumL a p1 = do
    -- IORef to hold the current latch value
    latch <- newIORef a
    let l1 = Latch { readL = readIORef latch }

    -- calculate new pulse from old value
    let l2 = mapL (flip ($)) l1
    p2 <- applyP l2 p1

    -- register handler to update latch
    uid <- newUnique
    let handler = whenPulse p2 $ (writeIORef latch $!)
    void $ addHandlerP p2 (uid, DoLatch, handler)
    
    pure (l1,p2)

-- | Latch whose value stays constant.
pureL :: a -> Latch a
pureL a = Latch { readL = pure a }

-- | Map a function over latches.
--
-- Evaluated only when needed, result is not cached.
mapL :: (a -> b) -> Latch a -> Latch b
mapL f l = Latch { readL = f <$> readL l } 

-- | Apply two current latch values
--
-- Evaluated only when needed, result is not cached.
applyL :: Latch (a -> b) -> Latch a -> Latch b
applyL l1 l2 = Latch { readL = readL l1 <*> readL l2 }

{-----------------------------------------------------------------------------
    Test
------------------------------------------------------------------------------}
test :: IO (Int -> IO ())
test = do
    (p1, fire) <- newPulse
    p2     <- mapP (+) p1
    (l1,_) <- accumL 0 p2
    let l2 =  mapL const l1
    p3     <- applyP l2 p1
    void $ addHandler p3 print
    pure fire

test_recursion1 :: IO (IO ())
test_recursion1 = do
    (p1, fire) <- newPulse
    rec {
        p2      <- applyP l2 p1;
        p3      <- mapP (const (+1)) p2;
        ~(l1,_) <- accumL (0::Int) p3;
        l2      <- pure (mapL const l1);
    }
    void $ addHandler p2 print
    pure $ fire ()
