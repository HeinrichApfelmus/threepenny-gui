{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | API for weak pointers that fixes a few issues over "System.Mem.Weak".
module Foreign.RemotePtr.Weak
    ( Weak
    , mkWeakIORef
    , mkWeakIORefValue
    , deRefWeak
    , finalize
    ) where

import Prelude

#if defined(__MHS__)
{-----------------------------------------------------------------------------
    MicroHs
------------------------------------------------------------------------------}
import Data.IORef hiding (mkWeakIORef)

type Weak = IORef

mkWeakIORef :: IORef a -> IO () -> IO (Weak (IORef a))
mkWeakIORef ref _ = newIORef ref

mkWeakIORefValue :: IORef a -> value -> IO () -> IO (Weak value)
mkWeakIORefValue _ v _ = newIORef v

deRefWeak :: Weak v -> IO (Maybe v)
deRefWeak ref = Just <$> readIORef ref

finalize :: Weak v -> IO ()
finalize _ = pure ()

#else
{-----------------------------------------------------------------------------
    GHC
------------------------------------------------------------------------------}
import Data.IORef
    ( IORef, mkWeakIORef )
import System.Mem.Weak
    ( Weak, deRefWeak, finalize )

import qualified GHC.Base  as GHC
import qualified GHC.Weak  as GHC
import qualified GHC.IORef as GHC
import qualified GHC.STRef as GHC

-- | Make 'Weak' pointer whose key is an 'IORef'.
mkWeakIORefValue :: IORef a -> value -> IO () -> IO (Weak value)
mkWeakIORefValue (GHC.IORef (GHC.STRef r#)) v (GHC.IO f) = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)

#endif
