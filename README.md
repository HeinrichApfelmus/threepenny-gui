[![Build Status](https://travis-ci.org/HeinrichApfelmus/threepenny-gui.png)](https://travis-ci.org/HeinrichApfelmus/threepenny-gui)

Threepenny-gui is a GUI framework written in Haskell that uses the web browser as a display. It's very easy to install. See the

  [**Project homepage**](http://wiki.haskell.org/Threepenny-gui)

for more information on what it does and can do for you as a library user.

## Examples

The library comes with many examples, which can be found in the [samples](https://github.com/HeinrichApfelmus/threepenny-gui/tree/master/samples#readme) folder. Follow the link for more information on how to run them.

## Technical overview

A program written with Threepenny is essentially a small web server that displays the user interface as a web page to any browser that connects to it.

### Latency

The frequent communication between browser and server
means that Threepenny is best used as a GUI server running on localhost.

The communication is done over a persistent connection using WebSockets.

If you want to reduce latency, the best option is to generate JavaScript
code and run it on the client. Consider this approach similar to [a
shading language](http://en.wikipedia.org/wiki/Shading_language). Some means of producing JavaScript from Haskell might be:

* Fay
* HJScript
* GHCJS
* UHC

Alternatively, the JS can be written by the user directly and invoked via the JavaScript FFI from Threepenny.

## Future ideas

### HTML rendering mode

It might be nice in the case of search engines to merely generate a DOM and render it, so that search engines can read the pages.

### UI libraries

[qooxdoo](http://qooxdoo.org/demo) — provides a feature-complete widget set. One could wrap this in a type-safe API from Threepenny and get a complete, stable UI framework for free. Most of the "immediate feedback" like dragging things here, switching tabs there, are taken care of by the framework. All that would be left would be to provide the domain configuration and business/presentation logic.

There are plenty more like this, but this is the first that springs to
mind that is good.

## Contributors

Many thanks to everyone who contributed, provided feedback or simply wrote an application using Threepenny! In particular, many thanks to:

Heinrich Apfelmus, Daniel Austin, Steve Bigham, Ken Friis Larsen, Daniel Mlot, Tim C. Schröder *and also* Daniel Diaz, Alex Greif, Yuval Langer, JP Moresmau, Luke Palmer, Jens Peterson, Jaap van der Plas, rnons, Michael Snoyman, tailcalled

Special thanks to *Chris Done* for starting the precursor project Ji.
