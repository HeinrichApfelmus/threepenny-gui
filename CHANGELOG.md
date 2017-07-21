## Changelog for the `threepenny-gui` package

**Unreleased**

* Bump dependencies for compatibility with GHC-8.2.1
* Bump dependencies to allow `websockets` 0.12

**0.8.1.0** — Snapshot release

* Expose JavaScript FFI functions `toJSObject` and `liftJSWindow` in `Graphics.UI.Threepenny`. This is useful for linking the lifetime of JavaScript objects to the lifetime of `Element`. [#181][]
* Use `jsLog` parameter to log exceptions. [#185][]

  [#181]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/181
  [#185]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/185

**0.8.0.0** — Snapshot release

Graphics.UI.Threepenny

* Fix `getElementById` to properly return `Nothing` when no element with the given `id` attribute is present. [#129][].
* Bring back `loadFile` and `loadDirectory`. [#110][]
* Add `MonadUI` typeclass for easier lifting in custom monad stacks. [#173][]

JavaScript FFI

* Implement batching / buffering for FFI calls. [#131][]. Several calls to the `runFunction` function may now buffer the JavaScript code and send it to the browser window in a single message at a later time. This improves performance considerably.
* Clarify semantics for exceptions. See the file [`doc/javascript-ffi.md`](doc/javascript-ffi.md) for more.
    * The `UI` monad now supports exception handling by being an instance of the type classes `MonadThrow` and `MonadCatch`.
    * The function `callFunction` can now throw a `JavaScriptException` exception to the Haskell side.
    * The function `runFunction` now terminates the connection to the browser window whenever the JavaScript code within throws an exception.
* Exceptions in the `UI` monad that are not handled are now printed properly for better debugging. [#145][]
* Clarify semantics of the `disconnect` event. It is now triggered more reliably. [#133][].
* Remove unnecessary client response when exporting event handlers. [#131][].
* Add option `jsWindowReloadOnDisconnect` to reload the browser window [#130][] whenever the WebSocket connection is broken. This is useful for e.g. mobile devices, which tend to disconnect WebSocket connections very often.

  [#110]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/110
  [#129]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/129
  [#130]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/130
  [#133]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/133
  [#145]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/145
  [#173]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/173

Dependencies

* Add dependency on `exceptions`

**0.7.0.2** — Maintenance release

* Bump dependencies to allow `aeson` 1.2
* Bump dependencies to allow `websockets` 0.11

**0.7.0.1** — Maintenance release

* Bump dependencies to allow `aeson` 1.1
* Bump dependencies to allow `vector` 0.12
* Bump dependencies to allow `websockets` 0.10

**0.7.0.0** — Maintenance and snapshot release

* JavaScript FFI: Reduce communication from browser to server when creating `Element`s.
    New function `unsafeCreateJSObject` to create JavaScript objects without
    waiting for a client response. [#131][]
* JavaScript FFI: Implement escape sequence '%%' when calling JavaScript functions. [#132][].
* Change type of `onEvent` function to allow unregistering events.
* Add function `timestamp` for simple performance measurements.
* Update JavaScript dependencies to jQuery 2.2.3
* Adapt to GHC 8.0.1. [#138][]
* Bump dependencies to allow `aeson` 1.0
* Bump dependencies to allow `data-default` 0.7
* Bump dependencies to allow `snap-core` 1.0 and `snap-server` 1.0
* Bump dependencies to allow `template-haskell` 2.11
* Bump dependencies to allow `websockets-snap` 0.10

  [#131]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/131
  [#132]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/132
  [#138]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/138

**0.6.0.6** — Maintenance release

* Bump dependencies to allow `base` 4.9
* Bump dependencies to allow `aeson` 0.11

**0.6.0.5** — Maintenance release

* Bump dependencies to allow `async` 2.1
* Bump dependencies to allow `transformers` 0.5

**0.6.0.4** — Maintenance release.

* Elements that have become unreachable, for instance because they have been removed from the DOM and are no longer reachable in the Haskell code, will be garbage collected again. Fix [#109][], [#113][].
* Adjust dependencies.
* Add `<meta>` tag to indicate UTF8 encoding in html file. [#116][]

  [#113]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/113
  [#109]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/109
  [#116]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/116

**0.6.0.3** — Maintenance release.

* Temporary fix for #109, which was causing event handlers to be discarded. Unfortunately, this means that elements are currently not garbage collected after they have been removed from the DOM tree.

**0.6.0.2** — Maintenance release.

* Remove unused direct dependencies, in particular
** attoparsec-enumerator
** utf8-string
** MonadCatchIO-transformers
** time

**0.6.0.1** — Maintenance release.

* The `ADDR` environment variable is now parsed correctly.
* Now builds on GHC 7.8 and GHC 7.10
* The example source code in the `samples` folder has been reorganized and consolidated.

**0.6.0.0** — Snapshot release.

* The internals of the JavaScript FFI has been reimplemented completely. A new module `Foreign.JavaScript` exports a bare JavaScript FFI in case you want to write a custom GUI framework. However, the module `Graphics.UI.Threepenny` is *not* compatible with it, even though it builds on top of it.
* The fields of `Config` type for server configuration are now prefixed with `js` instead of `tp`. Example: `jsPort`, `jsStatic`.
* The functions `loadFile` and `loadDirectory` have been *removed*, as I felt that the `jsStatic` option is sufficient for most use cases.

**0.5.0.0** — Snapshot release.

* Possibility to specify IP address to bind the server to.
* FFI now supports callbacks into Haskell. Remove `callDeferredFunction` function.
* `Graphics.UI.Threepenny.Canvas.SVG` for creating SVG elements and attributes.
* 2D graphics API in `Graphics.UI.Threepenny.Canvas` is beginning to grow.
* `Bool` is now correctly marshalled to JavaScript.
* `Text` can now be marshalled to JavaScrtip.

**0.4.2.0** — Maintenance release.

* Dependency `bytestring >=0.9.2` is now implemented correctly.
* Allow newer versions of `aeson` dependency.
* Allow newer versions of `network`, `transformers` and `template-haskell` dependencies.
* Helper scripts in the `samples` directory now assume that you use a cabal sandbox for development.
* The `UI` monad is now an instance of the `Applicative` class.

**0.4.1.0** — Maintenance release.

* Dependency on `text` package now from version 0.11 to 1.1.*.
* Dependency on `aeson` package replaces the former dependency on the `json` package.
* Unicode characters are now transmitted correctly to the browser. #75, #62.
* Change default port number to 8023. #64

**0.4.0.2** — Bugfix release.

* Fix CSS bug for `grid` function.

**0.4.0.1** — Maintenance release.

* Adjust package dependencies.

**0.4.0.0** — Snapshot release.

* New `UI` monad for easier JavaScript FFI and recursion in FRP.
* Garbage collection for DOM elements. (Unfortunately, this doesn't support using custom HTML files anymore, see [issue #60][#60].)
* First stab at widgets.
* Bump dependencies to allow `websockets` 0.8

  [#60]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/60

**0.3.0.0** — Snapshot release.

* Browser communication with WebSockets.
* First stab at FRP integration.

**0.2.0.0** — Snapshot release.

* First stab at easy JavaScript FFI.

**0.1.0.0**

* Initial release.


