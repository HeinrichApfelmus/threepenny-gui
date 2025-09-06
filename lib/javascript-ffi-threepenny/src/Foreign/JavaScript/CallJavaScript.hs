{-# LANGUAGE NamedFieldPuns #-}

-- | Call JavaScript from Haskell.
module Foreign.JavaScript.CallJavaScript
    ( fromJSStablePtr, newJSObjectFromCoupon
    ) where

import Foreign.JavaScript.CallBuffer
    ( bufferRunEval )
import Foreign.JavaScript.Types
    ( JSObject, JSPtr (..), Window(Window,wJSObjects) )

import qualified Data.Text                  as T
import qualified Foreign.JavaScript.JSON    as JSON
import qualified Foreign.RemotePtr          as RemotePtr

{-----------------------------------------------------------------------------
    Create JSObject
------------------------------------------------------------------------------}
-- | Retrieve 'JSObject' associated with a JavaScript stable pointer.
fromJSStablePtr :: JSON.Value -> Window -> IO JSObject
fromJSStablePtr js w@(Window{wJSObjects}) = do
    let JSON.Success coupon = JSON.fromJSON js
    mhs <- RemotePtr.lookup coupon wJSObjects
    case mhs of
        Just hs -> pure hs
        Nothing -> newJSObjectFromCoupon w coupon

-- | Create a new JSObject by registering a new coupon.
newJSObjectFromCoupon :: Window -> RemotePtr.Coupon -> IO JSObject
newJSObjectFromCoupon w@(Window{wJSObjects}) coupon = do
    ptr <- RemotePtr.newRemotePtr coupon (JSPtr coupon) wJSObjects
    RemotePtr.addFinalizer ptr $
        bufferRunEval w ("Haskell.freeStablePtr('" ++ T.unpack coupon ++ "')")
    pure ptr
