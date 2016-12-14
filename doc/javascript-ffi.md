JavaScript FFI documentation
============================
*Heinrich Apfelmus* -- *December 2016*

<a rel="license" href="https://creativecommons.org/licenses/by-sa/4.0/"><img alt="Creative Commons License" style="border-width:0" src="http://i.creativecommons.org/l/by-sa/4.0/88x31.png" /></a> This text is licensed under an <a rel="license" href="http://creativecommons.org/licenses/by-sa/4.0/">Attribution-ShareAlike 4.0 International License</a>.

TODO: Most of this information belongs into the Haddock documentation of the corresponding modules.

Overview
--------

This library implements a JavaScript foreign function interface (FFI) in Haskell. This means that you can call JavaScript functions from Haskell and Haskell functions from JavaScripts.

Here, we want to execute the JavaScript code in the context of a web browser window, while the Haskell code is part of a compiled executable. In fact, the executable acts a web server, to which a web browser can connect. The server instructs the browser to execute the desired JavaScript functions.

A JavaScript FFI needs to offer ways to

* Manage the connection between browser and server
* Manage data and data types, e.g.
    * Manage object lifetime (garbage collection)
    * Marshal JavaScript and Haskell data types
* Execute code, e.g.
    * Allow the Haskell side to call JavaScript functions
    * Allow the JavaScript side to call Haskell functions

In the following, we will describe how the various parts are designed.

Calling functions
-----------------

The JavaScript FFI allows Haskell functions to call JavaScript functions, and vice versa. There are two sides to this story: The Haskell side and the JavaScript side.

We begin with the Haskell side. To the Haskell, it looks as if we can execute arbitrary JavaScript code. There are two ways to execute JavaScript code: `runFunction` and `callFunction`.

The function `callFunction` executes JavaScript code in a *synchronous* way. The server sends a message to the browser, which executes the code, and returns a result to the server. However, returning results is not the only possible control flow: the code can also throw an exception. If that happens, then `callFunction` will rethrow any exception that has occurred. An additional problem is that the connection between the browser may break down and no result can be obtained. In this case, the function `callFunction` will also throw an exception.

The function `runFunction` executes JavaScript code in an *asynchronous* way. The server sends a message to the browser to execute the code, but does not wait for any result, and simply resumes the execution of subsequent Haskell code. If `runFunction` is called again, then the additional piece of JavaScript code is guaranteed to be executed after the previous one. What happens if the JavaScript code throws an exception? Then, the connection between browser and server will actually be *terminated*, as this is the only way to communicate to the server that something went wrong (You can always wrap your JavaScript code in a `try { .. } catch (err) {}` block if you don't want to terminate the connection). When the connection between browser and server has been broken, any attempt at calling `runFunction` will throw an exception, though this may happen belatedly.

To the JavaScript side, it will look as if we have suddenly gained the ability to call Haskell functions.

However, there is a difficulty: What if we call a Haskell function, which in turn calls a JavaScript function, which in turn calls a Haskell function? Managing this kind of control flow is rather daunting, at least if we want to offer synchronous calls. We take the easy way out, and only allow calling Haskell functions in an *asynchronous* way.

Before the JavaScript side is able to call a Haskell function, the latter has to be *exported* by the Haskell side. This is done with the `exportHandler` function, like this:

    jsfun <- exportHandler window hsfun

The result `jsfun` corresponds to a JavaScript `Function` object, which represents an exported Haskell function, and which can be called at will on the JavaScript side. Doing so will send a message to the server to execute the IO action that was supplied in the `hsfun` argument. Since the call is *asynchronous*, no result will be returned to the JavaScript side. Again, any exception will *terminate* the connection to the browser. If several exported Haskell are called, the Haskell side will execute them sequentially, in the order that they were called, but not necessarily immediately.


Connecting server and browser
-----------------------------

…

*Initialization* —
Just after the browser establishes a connection to the server, an initial Haskell function will be called, namely the last argument to `serve`. From the JavaScript side, this behaves like an ordinary call to an exported Haskell function. In particular, if the function throws an exception, the connection will be terminated again.

*Disconnection* —
If, for some reason, the connection between browser and server is broken,
another Haskell function will be called, the *disconnect* handler. The rules are the same as for any ordinary "exported" Haskell function. In particular, the disconnect handler will be called sequentially after any previous event handler. Disconnecting the browser window is *not* an asynchronous exception. That said, any attempt at executing JavaScript code with e.g. `runFunction` will throw a (synchronous) exception after the browser has disconnected. The disconnect handler will be trigger only once (in case the disconnect handler itself tries to call JavaScript code, which will throw an exception).


Garbage collection
------------------

TODO. StablePtr. RemotePtr.
