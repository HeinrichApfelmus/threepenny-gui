module Reactive.Threepenny.Memo (
    Memo, fromPure, memoize, at, liftMemo1, liftMemo2,
    ) where

import Control.Monad.Fix (mfix)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

{-----------------------------------------------------------------------------
    Memoize time-varying values / computations
------------------------------------------------------------------------------}
data Memo a
    = Const a
    | Memoized (IORef (MemoD a))

type MemoD a = Either (IO a) a

fromPure :: a -> Memo a
fromPure = Const

at :: Memo a -> IO a
at (Const a)    = pure a
at (Memoized r) = do
    memo <- readIORef r
    case memo of
        Right a -> pure a
        Left ma -> mfix $ \a' -> do
            writeIORef r $ Right a'
            a <- ma    -- allow some recursion
            pure a

memoize :: IO a -> Memo a
memoize m = unsafePerformIO $ Memoized <$> newIORef (Left m)

liftMemo1 :: (a -> IO b) -> Memo a -> Memo b
liftMemo1 f ma = memoize $ f =<< at ma

liftMemo2 :: (a -> b -> IO c) -> Memo a -> Memo b -> Memo c
liftMemo2 f ma mb = memoize $ do
    a <- at ma
    b <- at mb
    f a b
