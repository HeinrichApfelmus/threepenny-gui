module Graphics.UI.Threepenny.Attributes (
    -- * Synopsis
    -- | Element attributes, for convenience.
    
    -- * Documentation
    style, class_, id_, href,
    ) where

import Graphics.UI.Threepenny.Core

{-----------------------------------------------------------------------------
    Attributes
------------------------------------------------------------------------------}
mkElementAttr name = mkWriteAttr (set' (attr name))

-- | CSS class.
class_ :: WriteAttr Element String
class_ = mkElementAttr "class"

href :: WriteAttr Element String
href = mkElementAttr "href" 

-- | @id@ attribute.
id_ :: WriteAttr Element String
id_ = mkElementAttr "id"

