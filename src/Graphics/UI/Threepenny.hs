module Graphics.UI.Threepenny (
    -- * Introduction
    -- $intro

    -- * Example
    -- $example

    -- * Modules
    module Graphics.UI.Threepenny.Attributes,
    module Graphics.UI.Threepenny.Core,
    module Graphics.UI.Threepenny.Canvas,
    module Graphics.UI.Threepenny.DragNDrop,
    module Graphics.UI.Threepenny.Elements,
    module Graphics.UI.Threepenny.Events,
    module Graphics.UI.Threepenny.JQuery,
    module Graphics.UI.Threepenny.Timer,
    module Graphics.UI.Threepenny.Widgets,
    ) where

import           Graphics.UI.Threepenny.Attributes
import           Graphics.UI.Threepenny.Canvas
import           Graphics.UI.Threepenny.Core
import           Graphics.UI.Threepenny.DragNDrop
import           Graphics.UI.Threepenny.Elements
import           Graphics.UI.Threepenny.Events
import           Graphics.UI.Threepenny.JQuery
import           Graphics.UI.Threepenny.Timer
import           Graphics.UI.Threepenny.Widgets

{- $intro

Welcome to the Threepenny library for graphical user interfaces.

A program written with Threepenny is essentially a small web server
that displays the user interface as a web page to any browser that connects to it.

For an introduction, see the example below.
The module "Graphics.UI.Threepenny.Core" contains the main functions.

-}


{- $example

The following example should help to get you started with Threepenny.
(The lines of code below are meant to be concatenated into a single file.)

> module Main where

First, we have to import the library.
It is a good idea to import the core module verbatim
and import all other functions with a mandatory @UI@ prefix.

> import qualified Graphics.UI.Threepenny       as UI
> import           Graphics.UI.Threepenny.Core

We begin by starting a server on port @8023@ using the 'startGUI' function.
Additional static content is served from the @../wwwroot@ directory.

> main :: IO ()
> main = do
>     startGUI defaultConfig
>         { jsPort       = Just 8023
>         , jsStatic     = Just "../wwwroot"
>         } setup

Whenever a browser connects to the server,
the following function will be executed to start the GUI interaction.
It builds the initial HTML page.

> setup :: Window -> UI ()
> setup window = do

First, set the title of the HTML document

>     return window # set UI.title "Hello World!"

Then create a button element

>     button <- UI.button # set UI.text "Click me!"

DOM elements can be accessed much in the same way they are
accessed from JavaScript; they can be searched, updated, moved and
inspected. In the line above, we set the 'text' contents.

To actually display the button, we have to attach it to the body of the HTML element.
The '#+' combinator allows you to nest elements quickly
in the style of a HTML combinator library.

>     getBody window #+ [element button]

Finally, we register an event handler for the 'click' event,
which occurs whenever the user clicks on the button.
When that happens, we change the text of the button.

>     on UI.click button $ const $ do
>         element button # set UI.text "I have been clicked!"

That's it! Now, run the program and visit the URL <http://localhost:8023/>
in your browser to interact with the user interface.

The library comes with a
<https://github.com/HeinrichApfelmus/threepenny-gui/tree/master/samples#readme plethora of additional example code>.


-}

