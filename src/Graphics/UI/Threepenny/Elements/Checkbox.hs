
module Graphics.UI.Threepenny.Elements.Checkbox (
    -- * Checkbox
    Checkbox
    -- * Construction
  , checkbox
    -- * Attributes
  , isChecked
  , checkboxText
  ) where

import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Elements.Class
--
import Control.Applicative
import Control.Monad (void)
import Data.IORef

data Checkbox =
  Checkbox { checkboxElement :: Element
           , checkboxName :: Element
           , checkboxNameVar :: IORef String
             }

instance Widget Checkbox where
  toElement cb = row [ element $ checkboxElement cb
                     , element $ checkboxName    cb
                       ]

checkbox :: String -> IO Checkbox
checkbox str = Checkbox <$> e
                        <*> (a #+ [string str])
                        <*> newIORef str
  where
    e = input # set type_ "checkbox" # set checked False

isChecked :: Attr Checkbox Bool
isChecked = mkReadWriteAttr
  (get checked . checkboxElement)
  (\b cb -> void $ element (checkboxElement cb) # set checked b)

checkboxText :: Attr Checkbox String
checkboxText = mkReadWriteAttr
  (readIORef . checkboxNameVar)
  (\str cb -> do
     e <- string str
     element (checkboxName cb) # set children [e]
     writeIORef (checkboxNameVar cb) str
   )

{- Widgets proposal

The intention of this module goes beyond implementing a feature. It is
actually an API proposal.

The idea is to organize elements as /widgets/. Instead of having every element
with the same type, each kind of widget has a type of its own. In the example
above, a simple checkbox widget is implemented. It consists in the following
parts:

1) Widget Type. It contains all the necessary elements that the given widget
   needs to work properly. It may contain several elements, or even other
   widgets. The type must be kept abstract.

2) Widget Class Instance. To fit in the picture, a widget must be convertad
   to a single element, so it can be included in the layout of the program and
   interact with other elements.

3) Widget Attributes. Attribute setters and/or getters of the widget. In the
   example above, checkboxes have the 'checked' and 'text' attributes.

About attributes, I think plenty of widgets are going to share some of them.
In the example above, properties like 'checked' and 'text' may apply to other
widgets as well. Type classes can solve this problem in an elegant way. A great
consequence of implementing it this way is that users will be only allowed
to apply certain attributes only to those type that makes sense to apply them to.
Currently, attributes can be applied freely to any element, which might make the user
wonder in each execution if the attribute will work or not, and then go back to code
and try another one.

Note that elements will still be used, but certain tasks (like using lists, checkboxes,
buttons, etc) can be achieved in a better way.

-}
