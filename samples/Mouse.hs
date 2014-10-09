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
  --
  -- set :: ReadWriteAttr x i o -- The attribute we want to set. The most
  -- general attribute is of the `ReadWriteAttr x i o`, where  We
  --                               `title` in our case.
  --     -> i                   -- The value we want to set the attribute to.
  --                               `"Mouse"` in our case.
  --     -> UI x                -- the object we want to set its attribute.
  --                               `w` in our case.
  --     -> UI x                -- We get back an object with its attribute
  --                               changed.
  --
  -- # :: a -> (a->b) -> b
  -- `#` is exactly the same as `$`:
  -- (a # f) == (f $ a)
  -- This function cleans up the code when you change a bunch of
  -- attributes. Compare this with:
  -- (set attr5 "value 5" w) .
  -- (set attr4 "value 4" w) .
  -- (set attr3 "value 3" w) .
  -- (set attr2 "value 2" w) .
  -- (set attr1 "value 1") $ w
  --
  -- with this:
  -- w # set attr5 "value 5"
  --   # set attr4 "value 4"
  --   # set attr3 "value 3"
  --   # set attr2 "value 2"
  --   # set attr1 "value 1"
  --
  --
  -- title :: WriteAttr Window String
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
