module Graphics.UI.Threepenny.DOM where

import Graphics.UI.Threepenny

-- | A prettier bind.
(#) :: (Monad m) => m a -> (a -> m b) -> m b
(#) = (>>=)
infixl 1 #

-- | Append the element to a parent.
(#+) :: (MonadTP m) => m Element -> Element -> m Element
m #+ parent = m # addTo parent
infixl 1 #+

-- | Set the class of an element.
(#.) :: (MonadTP m) => m Element -> String -> m Element
m #. cls = m # setClass cls
infixl 1 #.

-- | Set the id of an element.
(##) :: (MonadTP m) => m Element -> String -> m Element
m ## id = m # setId id
infixl 1 ##

-- | Set the text of an element.
(#=) :: (MonadTP m) => m Element -> String -> m Element
m #= txt = m # setText txt
infixl 1 #=

-- | To this element, add this child.
addTo :: MonadTP m => Element -> Element -> m Element
addTo = appendTo

-- | Add this child, to that element.
add :: MonadTP m => Element -> Element -> m Element
add child parent = do
  appendTo parent child # unit
  return parent

-- | Set an attribute.
set :: (MonadTP m) => String -> String -> Element -> m Element
set = setAttr

-- | Discard the element chain.
unit :: Monad m => a -> m ()
unit =  \_ -> return ()

-- | Set the class of an element.
setClass :: (MonadTP m) => String -> Element -> m Element
setClass = set "class"

-- | Set the id of an element.
setId :: (MonadTP m) => String -> Element -> m Element
setId = set "id"


  
-- | Make a new div.
new :: MonadTP m => m Element
new = newElement "div"

-- | Remove the element.
remove :: MonadTP m => Element -> m ()
remove = delete

-- | Hide an element.
hide :: MonadTP m => Element -> m ()
hide el = setClass "hidden" el # unit
