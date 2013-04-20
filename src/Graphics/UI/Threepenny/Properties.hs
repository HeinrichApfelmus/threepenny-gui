module Graphics.UI.Threepenny.Properties (
    -- * Synopsis
    -- | Element properties, for convenience.
    
    -- * Documentation
    style, cssClass,
    
    -- * Drag and Drop
    draggable, droppable, dragData,
    ) where

import Control.Monad
import Graphics.UI.Threepenny.Core
import Graphics.UI.Threepenny.Internal.Core

{-----------------------------------------------------------------------------
    Properties
------------------------------------------------------------------------------}
style :: WriteOnlyProperty Element [(String,String)]
style = mkProperty set undefined
    where
    set vs a = setStyle vs a >> return ()

cssClass :: WriteOnlyProperty Element String
cssClass = mkProperty (set' (attr "class")) undefined

{-----------------------------------------------------------------------------
    Drag and Drop
------------------------------------------------------------------------------}
-- | Enable or disable dragging an element.
draggable :: WriteOnlyProperty Element Bool
draggable = mkProperty set undefined
    where
    set v = set' (attr "draggable") $ if v then "true" else "false"

-- | Set the data that is transferred when dragging.
dragData :: WriteOnlyProperty Element String
dragData = mkProperty set undefined
    where
    set v = set' (attr "ondragstart") $
        "event.dataTransfer.setData('dragData', '" ++ v ++ "')"

-- | Enable or disable whether the element accepts drops.
droppable :: WriteOnlyProperty Element Bool
droppable = mkProperty enable undefined
    where
    enable v = void . if v then allowDrop else blockDrop
    allowDrop el =
        element el
            # set (attr "ondragover") "event.preventDefault()"
            # set (attr "ondrop"    ) "event.preventDefault()"
    blockDrop el =
        element el
            # set (attr "ondragover") ""
            # set (attr "ondrop"    ) ""
