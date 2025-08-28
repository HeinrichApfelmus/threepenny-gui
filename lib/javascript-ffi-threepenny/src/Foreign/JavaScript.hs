{-# LANGUAGE CPP #-}

{- | A Foreign Function Interface (FFI) for JavaScript.

This module allows you to call JavaScript from Haskell, and vice-versa.
-}
module Foreign.JavaScript
    ( withBrowserWindow, Window
#if !defined(__MHS__)
    , module Foreign.JavaScript.Server
#endif
    , module Foreign.JavaScript.Functions
    , module Foreign.JavaScript.JSON
    ) where

import Foreign.JavaScript.Functions
import Foreign.JavaScript.JSON

{-----------------------------------------------------------------------------
    Server
------------------------------------------------------------------------------}
import Foreign.JavaScript.Types (Window)

#if defined(__MHS__)
import qualified Foreign.JavaScript.MicroHs
#else
import Foreign.JavaScript.Server
#endif

{- | Execute Haskell code when the browser window is first loaded.
The 'Window' argument allows you to call JavaScript functions.

This functions behaves differently for different compilers:

* When compiled with GHC, this function starts a web server on port @8023@.
  Whenever a web browser loads the main page, the argument is executed
  with 'Window' representing the browser that has connected.
  For custom configuration of the server, use "Foreign.JavaScript.Server".

* When compiled with MicroHs, this function is embeded into the web page
  and executes its argument when the web page is loaded.
  In this case, the "Foreign.JavaScript.Server" module is not available.
-}
withBrowserWindow :: (Window -> IO ()) -> IO ()
#if defined(__MHS__)
withBrowserWindow = Foreign.JavaScript.MicroHs.withBrowserWindow
#else
withBrowserWindow = serve defaultConfig
#endif
