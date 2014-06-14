BarTab tutorial
===============

> {-# OPTIONS_GHC -Wall #-}
> {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

TODO What do these pragmas mean?

> import Control.Applicative
> import Control.Monad
> import Data.IORef
> import Data.Maybe

TODO Maybe I should import qualified as FOO everything

> import qualified Graphics.UI.Threepenny as UI
> import Graphics.UI.Threepenny.Core

TODO What's in each of these modules?

> -- | Main entry point.
> main :: IO ()
> main = startGUI defaultConfig setup

> setup :: Window -> UI ()
> setup w = do
>     -- active elements
>     _ <- return w # set title "BarTab"

The `#` function is defined as:

```haskell
(#) :: a -> (a -> b) -> b
a # f = f $ a
```

Each `set` operation here will return a new value of the window,
but we still discard of the final value. Why?

My guess is that the `set` operation isn't really pure? I'm a bit confused by this.

>     elAdd    <- UI.button # set UI.text "Add"

Here we take the default button value provided by threepenny,
and with the set functionality get back a new button value with its
`text` attribute set to `"Add"`.

>     elRemove <- UI.button # set UI.text "Remove"
>     elResult <- UI.span

>     inputs   <- liftIO $ newIORef []



>     -- functionality
>     let
>         displayTotal = void $ do
>             xs <- getValuesList =<< liftIO (readIORef inputs)
>             element elResult # set text (showNumber . sum $ map readNumber xs)



>         redoLayout :: UI ()
>         redoLayout = void $ do
>             layout <- mkLayout =<< liftIO (readIORef inputs)
>             _ <- getBody w # set children [layout]
>             displayTotal



>         mkLayout :: [Element] -> UI Element
>         mkLayout xs = column $
>             [row [element elAdd, element elRemove]
>             ,UI.hr]
>             ++ map element xs ++
>             [UI.hr
>             ,row [UI.span # set text "Sum: ", element elResult]
>             ]



>         addInput :: UI ()
>         addInput = do
>             elInput <- UI.input # set value "0"
>             on (domEvent "livechange") elInput $ \_ -> displayTotal
>             liftIO $ modifyIORef inputs (elInput:)



>         removeInput :: UI ()
>         removeInput = liftIO $ modifyIORef inputs (drop 1)



>     on UI.click elAdd    $ \_ -> addInput    >> redoLayout
>     on UI.click elRemove $ \_ -> removeInput >> redoLayout
>     addInput >> redoLayout



> {-----------------------------------------------------------------------------
>     Functionality
> ------------------------------------------------------------------------------}
> type Number = Maybe Double



> instance Num Number where
>     (+) = liftA2 (+)
>     (-) = liftA2 (-)
>     (*) = liftA2 (*)
>     abs = fmap abs
>     signum = fmap signum
>     fromInteger = pure . fromInteger



> readNumber :: String -> Number
> readNumber s = listToMaybe [x | (x,"") <- reads s]
> showNumber :: Number -> String
> showNumber   = maybe "--" show



