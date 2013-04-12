module Control.Event (
    -- * Synopsis
    -- | Event-driven programming in the imperative style.
        
    -- * Documentation
    Handler, Event(..),
    mapIO, filterIO, filterJust,
    newEvent,
    ) where


import Data.IORef
import qualified Data.Unique -- ordinary uniques here, because they are Ord

import qualified Data.Map as Map

type Map = Map.Map

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
-- | An /event handler/is a function that takes an
-- /event value/ and performs some computation.
type Handler a = a -> IO ()


-- | An /event/ is a facility for registering
-- event handlers. These will be called whenever the event occurs.
-- 
-- When registering an event handler, you will also be given an action
-- that unregisters this handler again.
-- 
-- > do unregisterMyHandler <- register event myHandler
--
newtype Event a = Event { register :: Handler a -> IO (IO ()) }

{-----------------------------------------------------------------------------
    Combinators
------------------------------------------------------------------------------}
instance Functor Event where
    fmap f = mapIO (return . f)

-- | Map the event value with an 'IO' action.
mapIO :: (a -> IO b) -> Event a -> Event b
mapIO f e = Event $ \h -> register e $ \x -> f x >>= h 

-- | Filter event values that don't return 'True'.
filterIO :: (a -> IO Bool) -> Event a -> Event a
filterIO f e = Event $ \h ->
    register e $ \x -> f x >>= \b -> if b then h x else return ()

-- | Keep only those event values that are of the form 'Just'.
filterJust :: Event (Maybe a) -> Event a
filterJust e = Event $ \g -> register e (maybe (return ()) g)


{-----------------------------------------------------------------------------
    Construction
------------------------------------------------------------------------------}
-- | Build a facility to register and unregister event handlers.
-- Also yields a function that takes an event handler and runs all the registered
-- handlers.
--
-- Example:
--
-- > do
-- >     (event, fire) <- newEvent
-- >     register event (putStrLn)
-- >     fire "Hello!"
newEvent :: IO (Event a, a -> IO ())
newEvent = do
    handlers <- newIORef Map.empty
    let register k = do
            key <- Data.Unique.newUnique
            modifyIORef handlers $ Map.insert key k
            return $ modifyIORef handlers $ Map.delete key
        runHandlers x =
            mapM_ ($ x) . map snd . Map.toList =<< readIORef handlers
    return (Event register, runHandlers)
