module Reactive.Threepenny.Types where

import Control.Monad.Trans.RWS.Lazy
import Data.Functor.Identity ()

import           Data.Hashable
import qualified Data.Vault.Strict   as Vault.Strict
import           Data.Unique.Really

{-----------------------------------------------------------------------------
    Pulse and Latch
------------------------------------------------------------------------------}
type Values = Vault.Strict.Vault

type Handler  = EvalP (IO ())
data Priority = DoLatch | DoIO deriving (Eq,Show,Ord,Enum)

data Pulse a = Pulse
    { addHandlerP :: ((Unique, Priority), Handler) -> Build (IO ())
    , evalP       :: EvalP (Maybe a)
    }

instance Hashable Priority where hashWithSalt _ = fromEnum

data Latch a = Latch { readL :: EvalL a }

{-----------------------------------------------------------------------------
    Monads
------------------------------------------------------------------------------}
-- | The 'EvalP' monad is used to evaluate pulses.
type EvalP = RWST () () Values BuildIO
    -- state: current pulse values

-- | The 'EvalL' monad is used to evaluate latches.
type EvalL = IO

-- | The 'Build' monad is used to add pulses and latches to the graph.
type Build   = IO
type BuildIO = Build
