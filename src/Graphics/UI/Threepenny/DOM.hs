module Graphics.UI.Threepenny.DOM where

import Graphics.UI.Threepenny

-- | A prettier bind.
(#) :: (Monad m) => m a -> (a -> m b) -> m b
(#) = (>>=)
infixl 1 #

-- | Append the element to a parent.
(#+) :: IO Element -> Element -> IO Element
m #+ parent = m # addTo parent
infixl 1 #+

-- | Set the class of an element.
(#.) :: IO Element -> String -> IO Element
m #. cls = m # setClass cls
infixl 1 #.

-- | Set the id of an element.
(##) :: IO Element -> String -> IO Element
m ## id = m # setId id
infixl 1 ##

-- | Set the text of an element.
(#=) :: IO Element -> String -> IO Element
m #= txt = m # setText txt
infixl 1 #=

-- | To this element, add this child.
addTo :: Element -> Element -> IO Element
addTo = appendTo

-- | Add this child, to that element.
add :: Element -> Element -> IO Element
add child parent = do
  appendTo parent child # unit
  return parent

-- | Set an attribute.
set :: String -> String -> Element -> IO Element
set = setAttr

-- | Discard the element chain.
unit :: Monad m => a -> m ()
unit =  \_ -> return ()

-- | Set the class of an element.
setClass :: String -> Element -> IO Element
setClass = set "class"

-- | Set the id of an element.
setId :: String -> Element -> IO Element
setId = set "id"


  
-- | Make a new div.
new :: Window -> IO Element
new w = newElement w "div"

-- | Remove the element.
remove :: Element -> IO ()
remove = delete

-- | Hide an element.
hide :: Element -> IO ()
hide el = setClass "hidden" el # unit
