{-# LANGUAGE RecursiveDo #-}
module Reactive.Threepenny.Memo (
    Memo, fromPure, memoize, at, liftMemo1, liftMemo2,
    ) where

import Control.Monad
import Data.Functor
import Data.IORef
import System.IO.Unsafe

{-----------------------------------------------------------------------------
    Memoize time-varying values / computations
------------------------------------------------------------------------------}
data Memo a
    = Const a
    | Memoized (IORef (MemoD a))

type MemoD a = Either (IO a) a

fromPure = Const

at :: Memo a -> IO a
at (Const a)    = return a
at (Memoized r) = do
    memo <- readIORef r
    case memo of
        Right a -> return a
        Left ma -> mdo
            writeIORef r $ Right a
            a <- ma    -- allow some recursion
            return a

memoize :: IO a -> Memo a
memoize m = unsafePerformIO $ Memoized <$> newIORef (Left m)

liftMemo1 :: (a -> IO b) -> Memo a -> Memo b
liftMemo1 f ma = memoize $ f =<< at ma

liftMemo2 :: (a -> b -> IO c) -> Memo a -> Memo b -> Memo c
liftMemo2 f ma mb = memoize $ do
    a <- at ma
    b <- at mb
    f a b
