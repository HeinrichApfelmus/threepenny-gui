## Changelog for the `threepenny-gui` package

**0.5.0.0** -- Snapshot release.

* Possibility to specify IP address to bind the server to.
* `Bool` is now correctly marshalled to JavaScript.
* `Text` can now be marshalled to JavaScrtip.

**0.4.2.0** -- Maintenance release.

* Dependency `bytestring >=0.9.2` is now implemented correctly.
* Allow newer versions of `aeson` dependency.
* Allow newer versions of `network`, `transformers` and `template-haskell` dependencies.
* Helper scripts in the `samples` directory now assume that you use a cabal sandbox for development.
* The `UI` monad is now an instance of the `Applicative` class.

**0.4.1.0** -- Maintenance release.

* Dependency on `text` package now from version 0.11 to 1.1.*.
* Dependency on `aeson` package replaces the former dependency on the `json` package.
* Unicode characters are now transmitted correctly to the browser. #75, #62.
* Change default port number to 8023. #64

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