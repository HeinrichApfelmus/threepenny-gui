/* *********************************************************************
  Logging utilities
********************************************************************* */

// simple logging
Haskell.log = function () {
  if ($.cookie('Haskell.log.enabled') === "true") {
    window.console.log.apply(window.console, arguments);
  }
};

Haskell.log.enable = function (bool) {
  $.cookie('Haskell.log.enabled', bool.toString());
};


// very simple performance counter
Haskell.performance = function () {
  var t1,t2 = 0.0;
  var that  = {}

  that.now = function () {
    t1 = t2;
    t2 = window.performance.now();
    return t2;
  }
  that.diff = function () {
    return t2 - t1;
  }

  return that;
}();
