[![Travis Build Status](https://travis-ci.org/HeinrichApfelmus/threepenny-gui.svg)](https://travis-ci.org/HeinrichApfelmus/threepenny-gui)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/HeinrichApfelmus/threepenny-gui?svg=true)](https://ci.appveyor.com/project/HeinrichApfelmus/threepenny-gui)
[![Hackage](https://img.shields.io/hackage/v/threepenny-gui.svg)](https://hackage.haskell.org/package/threepenny-gui)
[![Stackage LTS](http://stackage.org/package/threepenny-gui/badge/lts)](http://stackage.org/lts/package/threepenny-gui)
[![Stackage Nightly](http://stackage.org/package/threepenny-gui/badge/nightly)](http://stackage.org/nightly/package/threepenny-gui)

# Threepenny-GUI

### What's this?

Threepenny is a GUI framework written in Haskell that uses the web browser as a display. It's very easy to install. See the

  [**Project homepage**](http://wiki.haskell.org/Threepenny-gui)

for more information on what it does and can do for you as a library user.

### Examples

The library comes with many examples, which can be found in the [samples](samples#readme) folder. Follow the link for more information on how to run them.

### Desktop Apps

Threepenny is mainly intended for writing GUI applications that run on the local network, and it relies on a web browser being installed. You can drop the latter requirement and integrate it a bit more tightly with you desktop environment by using the Electron framework. There is no fully automated support for this yet, but the documentation includes a [tutorial on how to use Electron with Threepenny](doc/electron.md).

# Technical overview

### JavaScript FFI

A program written with Threepenny is essentially a small web server that displays the user interface as a web page to any browser that connects to it.

The web server displays a HTML page, which in turn establishes WebSocket connection with the server. The server uses this connection to send JavaScript code that is executed in the client. In the library, this appears as a JavaScript Foreign Function Interface (FFI). The documentation includes [more information on the design of the JavaScript FFI](doc/javascript-ffi.md).

### Latency

The frequent communication between browser and server
means that Threepenny is best used as a GUI server running on localhost. You can use it on your local network as well.

If you want to reduce latency, the best option is to generate larger blocks of JavaScript
code and run them on the client. Consider this approach similar to [a
shading language](http://en.wikipedia.org/wiki/Shading_language).
You can import any JavaScript library and use it from the JavaScript FFI.

If you don't want to write JavaScript, then you could choose a Haskell-like language like [PureScript](http://www.purescript.org), [Fay](https://github.com/faylang/fay/wiki). You can also directly compile Haskell to JavaScript with [Haste](https://github.com/valderman/haste-compiler) or [GHCJS](https://github.com/ghcjs/ghcjs).

# Future ideas

### HTML rendering mode

It might be nice in the case of search engines to merely generate a DOM and render it, so that search engines can read the pages.

### UI libraries

[qooxdoo](http://qooxdoo.org/demo) — provides a feature-complete widget set. One could wrap this in a type-safe API from Threepenny and get a complete, stable UI framework for free. Most of the "immediate feedback" like dragging things here, switching tabs there, are taken care of by the framework. All that would be left would be to provide the domain configuration and business/presentation logic.

There are plenty more like this, but this is the first that springs to
mind that is good.

# Contributors

Many thanks to everyone who contributed, provided feedback or simply wrote an application using Threepenny! In particular, many thanks to:

Heinrich Apfelmus, Daniel Austin, Jeremy Barisch-Rooney, Steve Bigham, Simon Jakobi, Ken Friis Larsen, Daniel Mlot, Tim C. Schröder [*and many others*](CONTRIBUTORS)

Special thanks to *Simon Jakobi* for co-maintaining this project.

Special thanks to *Chris Done* for starting the precursor project Ji.
