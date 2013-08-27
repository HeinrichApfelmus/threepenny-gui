{-# LANGUAGE RecordWildCards #-}
module Reactive.Threepenny.PulseLatch (
    Pulse, newPulse, addHandler,
    neverP, mapP, filterJustP, unionWithP,
    
    Latch,
    pureL, mapL, applyL, accumL, applyP,
    readLatch,
    ) where


import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS     as Monad

import Data.Hashable
import Data.IORef
import Data.Monoid (Endo(..))

import Data.Unique.Really
import qualified Data.Vault.Lazy   as Vault
import qualified Data.HashMap.Lazy as Map

type Vault = Vault.Vault
type Map   = Map.HashMap


type Build = IO

{-----------------------------------------------------------------------------
    Pulse
------------------------------------------------------------------------------}
type EvalP = Monad.RWST () () Vault IO

runEvalP :: Vault -> EvalP a -> IO (a, Vault)
runEvalP pulses m = do
    (a, s, w) <- Monad.runRWST m () pulses
    return (a, s)


type Handler  = EvalP (IO ())
data Priority = DoLatch | DoIO deriving (Eq,Show,Ord,Enum)

instance Hashable Priority where hash = fromEnum

data Pulse a = Pulse
    { addHandlerP :: ((Unique, Priority), Handler) -> Build ()
    , evalP       :: EvalP (Maybe a)
    }

-- Turn evaluation action into pulse that caches the value.
cacheEval :: EvalP (Maybe a) -> Build (Pulse a)
cacheEval e = do
    key <- Vault.newKey
    return $ Pulse
        { addHandlerP = \_ -> return ()
        , evalP = do
            vault <- Monad.get
            case Vault.lookup key vault of
                Just a  -> return a
                Nothing -> do
                    a <- e
                    Monad.put $ Vault.insert key a vault
                    return a
        }

-- Add a dependency to a pulse, for the sake of keeping track of dependencies.
dependOn :: Pulse a -> Pulse b -> Pulse a
dependOn p q = p { addHandlerP = \h -> addHandlerP p h >> addHandlerP q h }

-- Execute an action when the pulse occurs
whenPulse :: Pulse a -> (a -> IO ()) -> Handler
whenPulse p f = do
    ma <- evalP p
    case ma of
        Just a  -> return (f a)
        Nothing -> return $ return ()

{-----------------------------------------------------------------------------
    Latch
------------------------------------------------------------------------------}
data Latch a = Latch { readL :: IO a }

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
        addHandlerP :: ((Unique, Priority), Handler) -> Build ()
        addHandlerP (uid,m) = do
            handlers <- readIORef handlersRef
            case Map.lookup uid handlers of
                Just _  -> return ()
                Nothing -> writeIORef handlersRef $ Map.insert uid m handlers
        
        -- evaluate all handlers attached to this input pulse
        fireP a = do
            let pulses = Vault.insert key (Just a) $ Vault.empty
            handlers <- readIORef handlersRef
            (ms, _)  <- runEvalP pulses $ sequence $ 
                   [m | ((_,DoLatch),m) <- Map.toList handlers]
                ++ [m | ((_,DoIO   ),m) <- Map.toList handlers]  
            sequence_ ms
        
        evalP = join . Vault.lookup key <$> Monad.get

    return (Pulse {..}, fireP)

-- | Register a handler to be executed whenever a pulse occurs.
--
-- FIXME: Cannot unregister a handler again.
addHandler :: Pulse a -> (a -> IO ()) -> Build ()
addHandler p f = do
    uid <- newUnique
    addHandlerP p ((uid, DoIO), whenPulse p f)

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
    { addHandlerP = const $ return ()
    , evalP       = return Nothing
    }

-- | Map a function over pulses.
mapP :: (a -> b) -> Pulse a -> Build (Pulse b)
mapP f p = (`dependOn` p) <$> cacheEval (return . fmap f =<< evalP p)

-- | Filter occurrences. Only keep those of the form 'Just'.
filterJustP :: Pulse (Maybe a) -> Build (Pulse a)
filterJustP p = (`dependOn` p) <$> cacheEval (return . join =<< evalP p)

-- | Pulse that occurs when either of the pulses occur.
-- Combines values with the indicated function when both occur.
unionWithP :: (a -> a -> a) -> Pulse a -> Pulse a -> Build (Pulse a)
unionWithP f p q = (`dependOn` q) . (`dependOn` p) <$> cacheEval eval
    where
    eval = do
        x <- evalP p
        y <- evalP q
        return $ case (x,y) of
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
        return $ f <$> a

-- | Accumulate values in a latch.
accumL :: a -> Pulse (a -> a) -> Build (Latch a, Pulse a)
accumL a p1 = do
    -- IORef to hold the current latch value
    latch <- newIORef a
    let l1 = Latch { readL = readIORef latch }

    -- calculate new pulse from old value
    l2 <- mapL (flip ($)) l1
    p2 <- applyP l2 p1

    -- register handler to update latch
    uid <- newUnique
    let handler = whenPulse p2 $ (writeIORef latch $!)
    addHandlerP p2 ((uid, DoLatch), handler)
    
    return (l1,p2)

-- | Latch whose value stays constant.
pureL :: a -> Latch a
pureL a = Latch { readL = return a }

-- | Map a function over latches.
--
-- Evaluated only when needed, result is not cached.
mapL :: (a -> b) -> Latch a -> Build (Latch b)
mapL f l = return $ Latch { readL = f <$> readL l } 

-- | Apply two current latch values
--
-- Evaluated only when needed, result is not cached.
applyL :: Latch (a -> b) -> Latch a -> Build (Latch b)
applyL l1 l2 = return $ Latch { readL = readL l1 <*> readL l2 }

{-----------------------------------------------------------------------------
    Test
------------------------------------------------------------------------------}
test :: IO (Int -> IO ())
test = do
    (p1, fire) <- newPulse
    p2 <- mapP (+) p1
    (l1,_) <- accumL 0 p2
    l2 <- mapL const l1
    p3 <- applyP l2 p1
    addHandler p3 print
    
    return fire

