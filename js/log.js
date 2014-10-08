/* *********************************************************************
  Logging utilities
********************************************************************* */
Haskell.log = function () {
  if ($.cookie('Haskell.log.enabled') === "true") {
    window.console.log.apply(window.console, arguments);
  }
};

Haskell.log.enable = function (bool) {
  $.cookie('Haskell.log.enabled', bool.toString());
};
