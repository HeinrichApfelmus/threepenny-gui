import Control.Monad (void)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Main
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = do
    return w # set title "Mouse"
  -- ^ Let's place brackets around the expressions and see which functions
  -- are called using what variables [TODO: items objects variables?]:
  -- (return (w # (set title "Mouse")))
  -- ^ let's look at the types of the items [TODO: Items? Objects?
  -- Variables?]:
  -- # :: a -> (a->b) -> b
  -- `#` is exactly the same as `$`:
  -- (a # f) == (f $ a)
  --
  -- title :: WriteAttr Window String
  --
  -- set :: ReadWriteAttr x i o -- This is used to read (get) and write (set) 
  --     -> i -> UI x -> UI x
  -- set takes exactly three expressions. The first one will be
  -- a `ReadWriteAttr x i o`
  --
  -- w :: Window
  -- As defined in the function's definition.
    
    out  <- UI.span # set text "Coordinates: "
    wrap <- UI.div #. "wrap"
        # set style [("width","300px"),("height","300px"),("border","solid black 1px")]
        # set (attr "tabindex") "1" -- allow key presses
        #+ [element out]
    getBody w #+ [element wrap]
    
    on UI.mousemove wrap $ \xy ->
        element out # set text ("Coordinates: " ++ show xy)
    on UI.keydown   wrap $ \c ->
        element out # set text ("Keycode: " ++ show c)
