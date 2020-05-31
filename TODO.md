Threepenny TODO list
====================

* Implement a function to get the request URL (for the examples!)
* Implement a function to get the cookies while we are at it.
* Host threepenny examples with NFSN.

Now
---

* Use Reactive.Banana for the reactive subsystem.


* UI layout and styling
  * "Foundation" is a nice layout framework. Try it for "Dobble on screen"

  * Approach: The default is for these things to be "ON",
    but offer the possibility to switch them "OFF" again.
    * Defaults for Fonts, colors and UI elements (buttons)
    * Simple grid layouts using Flexbox (warn in documentation)
  * I think it's better to make an extra module that introduces these things?

  * Example program that displays all UI elements -> styling


* Ability to call threepenny-gui from within GHCi,
  see how the QuickPlot package does it!
  This not an interactive REPL yet, but comes closer than what we have now.

* Custom JavaScript layout engine?
  * quooxdoo -- seems to be a very nice framework for this task
  * Grid that does not change when the text in a field changes
  * Graphical UI designer?


Sometime
--------

* Replace `eval` with a handcrafted interpreter of a restricted JS-language?
  -> Performance measurements "JavaScript eval" indicate
     that eval is 10x slower than an a stack-based interpreter
  -> However, in a real world project (Canvas),
     the performance benefits seem to be negligible.
  * Bugs:
        1. Numbers are parsed incorrectly
        2. Bug in string conversion / Unicode / special symbols
        3. Capturing the `this` argument
           requires creating a closure.
           -> eliminate this by incorporating into PushScript language?

* Implement a useful application: Small Heap viewer!
  -> actually, ezyang is implementing something like that right now,
     so don't do that.


Later
-----

* Technical documentation for the design decisions behind the `Foreign.JavaScript` module:
  calling functions, handling events, garbage collection

* The `Foreign.JavaScript` module has a nice `IsHandler` class that
  can be used to marshal event handlers.
  It would be very nice if this could also be used for the `domEvent` function
  on the UI side, so that we don't have to marshal functions by hand anymore.


* Do not try to delete elements from the table when the connection has died
