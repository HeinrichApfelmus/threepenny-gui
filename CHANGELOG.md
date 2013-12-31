## Changelog for the `threepenny-gui` package

**0.5.0.0** -- Snapshot release.

* FFI now supports callbacks into Haskell. Remove `callDeferredFunction` function.
* Remove dependency on `json` library.

**0.4.0.2** -- Bugfix release.

* Fix CSS bug for `grid` function.

**0.4.0.1** -- Maintenance release.

* Adjust package dependencies.

**0.4.0.0** -- Snapshot release.

* New `UI` monad for easier JavaScript FFI and recursion in FRP.
* Garbage collection for DOM elements. (Unfortunately, this doesn't support using custom HTML files anymore, see [issue #60][60].)
* First stab at widgets.
* Update dependency to `websockets-0.8`.

**0.3.0.0** -- Snapshot release.

* Browser communication with WebSockets.
* First stab at FRP integration.

**0.2.0.0** -- Snapshot release.

* First stab at easy JavaScript FFI.

**0.1.0.0**

* Initial release.



  [60]: https://github.com/HeinrichApfelmus/threepenny-gui/issues/60