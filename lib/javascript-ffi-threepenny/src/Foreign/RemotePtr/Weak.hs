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
import Data.IORef
    ( IORef, mkWeakIORef )
import System.Mem.Weak
    ( Weak, deRefWeak, finalize )

import qualified GHC.Base  as GHC
import qualified GHC.Weak  as GHC
import qualified GHC.IORef as GHC
import qualified GHC.STRef as GHC

{-----------------------------------------------------------------------------
    Operations
------------------------------------------------------------------------------}
-- | Make 'Weak' pointer whose key is an 'IORef'.
mkWeakIORefValue :: IORef a -> value -> IO () -> IO (Weak value)
#if CABAL
#if MIN_VERSION_base(4,9,0)
mkWeakIORefValue (GHC.IORef (GHC.STRef r#)) v (GHC.IO f) = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)
#else
mkWeakIORefValue (GHC.IORef (GHC.STRef r#)) v f = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)
#endif
#else
mkWeakIORefValue (GHC.IORef (GHC.STRef r#)) v (GHC.IO f) = GHC.IO $ \s ->
  case GHC.mkWeak# r# v f s of (# s1, w #) -> (# s1, GHC.Weak w #)
#endif
