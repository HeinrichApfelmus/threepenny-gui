## Examples

This folder contains the code examples for the Threepenny-GUI library.

Run run the examples, use the provided `runhaskell` and `ghci` scripts:

    cd samples
    ./runhaskell Chat.hs

The scripts take care of setting up the necessary paths. However, they assume that you have set up a *[cabal sandbox][] in the project root folder*.

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
