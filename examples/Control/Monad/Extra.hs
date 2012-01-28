-- | Extra utilities for monads.

module Control.Monad.Extra where

import Data.Maybe

-- | Ignore the given action's return.
ig :: (Monad m) => m a -> m ()
ig m = m >> return ()

-- | A non-operator version of (=<<).
bind :: (Monad m) => (a -> m b) -> m a -> m b
bind = flip (>>=)

-- | When the value is Just, run the action.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust (Just a) m = m a
whenJust Nothing  _ = return ()

-- | Wrap up a functor in a Maybe.
just :: Functor m => m a -> m (Maybe a)
just = fmap Just

-- | Flip mapMaybe.
forMaybe :: [a] -> (a -> Maybe b) -> [b]
forMaybe = flip mapMaybe

-- | Monadic version of maybe.
maybeM :: (Monad m) => a -> (a1 -> m a) -> Maybe a1 -> m a
maybeM nil cons a = maybe (return nil) cons a
