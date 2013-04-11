module Control.AddHandler (
    -- * Synopsis
    -- | Event-driven programming, imperative style.
        
    -- * Documentation
    Handler, AddHandler(..), newAddHandler,
    mapIO, filterIO, filterJust,
    ) where


import Data.IORef
import qualified Data.Unique -- ordinary uniques here, because they are Ord

import qualified Data.Map as Map

type Map = Map.Map

{-----------------------------------------------------------------------------
    Types
------------------------------------------------------------------------------}
-- | An /event handler/ of type @Handler a@ is a function that takes an
-- /event value/ of type @a@ and performs some computation.
type Handler a = a -> IO ()


-- | A value of type @AddHandler a@ is a facility for registering
-- event handlers.
-- Put differently, an 'AddHandler' represents an /event/.
-- Registering an event handler means that the handler will be called
-- whenever the event occurs.
-- 
-- When registering an event handler, you will also be given an action
-- that unregisters this handler again.
-- 
-- > do unregisterMyHandler <- register addHandler myHandler
--
newtype AddHandler a = AddHandler { register :: Handler a -> IO (IO ()) }

{-----------------------------------------------------------------------------
    Combinators
------------------------------------------------------------------------------}
instance Functor AddHandler where
    fmap f = mapIO (return . f)

-- | Map the event value with an 'IO' action.
mapIO :: (a -> IO b) -> AddHandler a -> AddHandler b
mapIO f addHandler = AddHandler $ \h -> register addHandler $ \x -> f x >>= h 

-- | Filter event values that don't return 'True'.
filterIO :: (a -> IO Bool) -> AddHandler a -> AddHandler a
filterIO f addHandler = AddHandler $ \h ->
    register addHandler $ \x -> f x >>= \b -> if b then h x else return ()

-- | Keep only those event values that are of the form 'Just'.
filterJust :: AddHandler (Maybe a) -> AddHandler a
filterJust addHandler = AddHandler $ \g ->
    register addHandler (maybe (return ()) g)


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
-- >     (addHandler, fire) <- newAddHandler
-- >     register addHandler (putStrLn)
-- >     fire "Hello AddHandler!"
newAddHandler :: IO (AddHandler a, a -> IO ())
newAddHandler = do
    handlers <- newIORef Map.empty
    let register k = do
            key <- Data.Unique.newUnique
            modifyIORef handlers $ Map.insert key k
            return $ modifyIORef handlers $ Map.delete key
        runHandlers x =
            mapM_ ($ x) . map snd . Map.toList =<< readIORef handlers
    return (AddHandler register, runHandlers)
