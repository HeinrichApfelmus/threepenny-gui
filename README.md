[![Build Status](https://travis-ci.org/HeinrichApfelmus/threepenny-gui.png)](https://travis-ci.org/HeinrichApfelmus/threepenny-gui)

Threepenny-gui is a GUI framework that uses the web browser as a display.

* [Project homepage](http://www.haskell.org/haskellwiki/Threepenny-gui)

## Introduction

Threepenny is a GUI library for Haskell designed for easy setup across
all major OS variants.  It leverages the web browser as the foundation on 
which to build a desktop GUI.  Web-centric tasks such as message passing, 
routing, and serving files are handled automatically with little set up 
from the user.  As such, users unfamiliar with web technologies will likely
be able to pick up Threepenny quickly, although familiarity with HTML helps.
What is more, Threepenny is controlled entirely from within Haskell code, 
relieving the user of writing client-side Javascript by hand.

Threepenny comes with a simple web server that is preconfigured to host a 
client-side JS file called `threepenny-gui.js`. The Threepenny API communicates
with this JS to create new elements, respond to events, and more. This frequent
communication precludes Threepenny from use in high-latency environments.

Users can, however, write their own Javascript if they wish, and invoke that
JS from Threepenny.  This is useful for cases in which keeping the server in
the loop contributes too much delay.

This project was originally called Ji in its earliest iterations.

## Examples

The library comes with many examples, which can be found in the [samples](samples/) directory. To run them, use the provided `runhaskell` and `ghci` scripts:

    cd samples
    ./runhaskell Chat.hs

These scripts set up the correct paths.

* [BarTab.hs](https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/samples/BarTab.hs) - Dynamic creation of widgets.
* [Buttons.hs](https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/samples/Buttons.hs) - Simple buttons to click on.
* [Chat.hs](https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/samples/Chat.hs) - Multi-user chat.
* [CurrencyConverter.hs](https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/samples/CurrencyConverter.hs) - Simple demonstration of two reactive input elements.
* [DragNDropExample.hs](https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/samples/DragNDropExample.hs) - Simple drag'N'drop demo.
* [DrumMachine.hs](https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/samples/DrumMachine.hs) - Specify rhythm by activating and deactivating checkboxes.
* [MissingDollars.hs](https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/samples/MissingDollars.hs) - Text with configurable values.
* [UseWords.hs](https://github.com/HeinrichApfelmus/threepenny-gui/blob/master/samples/UseWords.hs)  - Text with configurable words. (Apologies for the male-centric story.)



## Challenges

### Latency

As indicated, the frequent communication between browser and server
means that Threepenny is best used as a GUI server running on localhost.

The communication is done over a persistent connection using WebSockets.

If you want to reduce latency, the best option is to generate JavaScript
code and run it on the client. Consider this approach similar to [a
shading language.](http://en.wikipedia.org/wiki/Shading_language)

Some means of producing JavaScript from Haskell might be:

* Fay
* HJScript
* GHCJS
* UHC

Alternatively, the JS can be written by the user directly and invoked from 
Threepenny.

### Garbage collection

Every DOM element referenced from the server needs a number generated
for it per session in a map, and once the session timesout it's
deleted.

We need more fine-grained garbage collection, though.
In particular, elements that are no longer referenced on the server
and elements that are no longer used in client window
should be removed. This needs some support from the JS garbage collector.


## Other ideas

Switch to HTML rendering mode — it might be nice in the case of search
engines to merely generate a DOM and render it, so that search engines
can read the pages.

## Other helpful libraries of interest

* [qooxdoo](http://qooxdoo.org/demo) — provides a feature-complete
  widget set. One could wrap this in a type-safe API from Threepenny and get a
  complete, stable UI framework for free. Most of the "immediate
  feedback" like dragging things here, switching tabs there, are taken
  care of by the framework. All that would be left would be to provide
  the domain configuration and business/presentation logic.

There are plenty more like this, but this is the first that springs to
mind that is good.

## Contributors

Many thanks to everyone who contributed, provided feedback or simply wrote an application using Threepenny!

* *Heinrich Apfelmus*
* *Daniel Austin*
* Daniel Díaz
* Yuval Langer
* *Daniel Mlot*
* JP Moresmau
* Luke Palmer
* Jens Petersen
* rnons
* Michael Snoyman
* tailcalled

Special thanks to *Chris Done* for starting the precursor project Ji.
