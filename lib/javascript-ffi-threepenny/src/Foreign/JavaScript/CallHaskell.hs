{-# LANGUAGE NamedFieldPuns #-}

-- | Call Haskell from JavaScript.
--
-- In the web browser implementation, we do not support
-- arbitrary nesting of JavaScript and Haskell code.
-- Instead, Haskell code must be called as \"events\",
-- which do not return results back to JavaScript code.
module Foreign.JavaScript.CallHaskell
    ( newHandler
    ) where

import Foreign.JavaScript.Types
    ( HsEvent, Window(Window,wEventHandlers) )

import qualified Data.Aeson         as JSON
import qualified Foreign.RemotePtr  as RemotePtr

{-----------------------------------------------------------------------------
    Exports, Imports and garbage collection
------------------------------------------------------------------------------}
-- | Turn a Haskell function into an event handler.
newHandler :: Window -> ([JSON.Value] -> IO ()) -> IO HsEvent
newHandler Window{wEventHandlers} handler = do
    coupon <- RemotePtr.newCoupon wEventHandlers
    RemotePtr.newRemotePtr coupon (handler . parseArgs) wEventHandlers
  where
    fromSuccess (JSON.Success x) = x
    -- parse a genuine JavaScript array
    parseArgs x = fromSuccess (JSON.fromJSON x) :: [JSON.Value]
    -- parse a JavaScript arguments object
    -- parseArgs x = Map.elems (fromSuccess (JSON.fromJSON x) :: Map.Map String JSON.Value)
