## Examples

This folder contains the code examples for the Threepenny GUI library.

To run the examples, you can (but don't have to) use the provided `runhaskell` and `ghci` scripts:

    cd samples
    ./runhaskell Chat.hs

These scripts check whether you have set up a [cabal sandbox][] or, alternatively, the [stack utility][stack] for use with this library, and uses these instead of the global package database; this is very useful for me as a library author.

(To set up the examples with `stack`, do the following

    git clone https://github.com/HeinrichApfelmus/threepenny-gui.git
    cd threepenny-gui/
    stack init
    stack setup
    stack build
    cd samples
    ./runhaskell Mouse.hs

)

After you have started an example, open your web browser and point it to the address [http://localhost:8023](http://localhost:8023). Enjoy!

  [stack]: http://haskellstack.org
  [cabal sandbox]: http://coldwa.st/e/blog/2013-08-20-Cabal-sandbox.html

Here a description of the currently maintained examples. The others are probably experimental or bitrotted.

* [BarTab.hs](BarTab.hs) — Dynamic creation of widgets.
* [Buttons.hs](Buttons.hs) — Simple buttons to click on.
* [Canvas.hs](Canvas.hs) — Graphics using the HTML 5 canvas element.
* [Chat.hs](Chat.hs) — Multi-user chat.
* [CRUD.hs](CRUD.hs) — A CRUD example showcasing functional reactice programming (FRP).
* [CurrencyConverter.hs](CurrencyConverter.hs) — Simple demonstration of two reactive input elements.
* [DragNDropExample.hs](DragNDropExample.hs) — Simple drag'N'drop demo.
* [DrumMachine.hs](DrumMachine.hs) — Specify rhythm by activating and deactivating checkboxes.
* [FadeInFadeOut.hs](FadeInFadeOut.hs) — Calling Haskell functions from the browser.
* [GetElementsBy.hs](GetElementsBy.hs) — Get elements by tag name or ID.
* [Mouse.hs](Mouse.hs) — Test of the `mousemove` event.
* [Svg.hs](Svg.hs) — Test of the SVG elements.
