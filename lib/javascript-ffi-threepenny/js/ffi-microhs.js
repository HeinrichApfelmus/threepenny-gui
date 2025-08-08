/* *********************************************************************
  Foreign Function Interface (FFI)
  JavaScript <-> Haskell

  This module implements everything necessary to call JS functions
  from Haskell and vice versa.

    * Listen to server and call JS functions on request.
    * Present Haskell functions as objects that can be called from JS.
    * StablePtr for JS objects.

********************************************************************* */

// Connect to the Haskell server and initialize the FFI.
// An optional string argument can be used to specify the server URL.
Haskell.initFFI = function () {

  /////////////////////////////////////////////////////////////////////
  // Calling Haskell functions

  // An event is a function on the server side that can be called anytime,
  // but whose execution will be queued.
  Haskell.newEvent = function (name, args) {
    var that = function () {
      var theargs = [];
      if (args) {
        // 'args' is a string that contains the name 'arguments'
        // Evaluating it will perform appropriate marshalling.
        theargs = eval(args);
      } else {
        for (var i=0; i<arguments.length; i++) {
          theargs[i] = arguments[i];
        }
      }

      Module._apply_sp(
        _haskellCallback,
        stringToNewUTF8(JSON.stringify({"name":name, "args":theargs}))
      );
    };
    return that;
  };

  /////////////////////////////////////////////////////////////////////
  // Stable Pointers on JavaScript objects
  //
  // Warning: We assume that each object can have at most one StablePtr
  // associated to it. We have to pay attention that we don't
  // free a stable pointer twice.
  var stablePtrs = {};
  var counter    = 0;

  var newStablePtr = function (object) {
    if (object.stablePtr === undefined) {
      object.stablePtr = 'js-' + counter.toString();
      stablePtrs[object.stablePtr] = object;
      counter++;
    }
    return object.stablePtr.toString();
  };

  Haskell.imposeStablePtr = function (object, ptr) {
    object.stablePtr = ptr;
    stablePtrs[object.stablePtr] = object;
  };

  Haskell.getStablePtr = function (object) {
    return (object.stablePtr || newStablePtr(object));
  };

  Haskell.deRefStablePtr = function (ptr) {
    return stablePtrs[ptr];
  };

  Haskell.freeStablePtr = function (ptr) {
    delete stablePtrs[ptr].stablePtr;
    delete stablePtrs[ptr];
  };
};
