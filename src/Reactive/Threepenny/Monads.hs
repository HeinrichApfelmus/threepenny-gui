module Reactive.Threepenny.Monads where

import Control.Monad.Trans.RWS.Lazy
import Reactive.Threepenny.Types

{-----------------------------------------------------------------------------
    EvalP - evaluate pulses
------------------------------------------------------------------------------}
runEvalP :: Values -> EvalP a -> IO (a, Values)
runEvalP pulses m = do
    (a, s, _) <- runRWST m () pulses
    return (a, s)
