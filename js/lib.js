/* *********************************************************************
  Threepenny - JavaScript library
  
  Additional JavaScript functions that are available
  for Threepenny.
********************************************************************* */
window.jquery_scrollToBottom = function(el){
  $(el).scrollTop(el.scrollHeight);
};

Haskell.map = function (fun, array) {
  var result = [];
  for (i=0; i<array.length; i++) {
    result[i] = fun(array[i]);
  }
  return result;
};

/////////////////////////////////////////////////////////////////////
// Binding to events
Haskell.bind = function (el, eventType, fun) {
  if(eventType === 'livechange') {
    $(el).livechange(300,function(e){
      fun([ $(el).val() ]);
      return true;
    });
  } else if(eventType === 'sendvalue') {
    $(el).sendvalue(function(x){
      fun(x.toString());
    });
  } else if (eventType.match('dragstart|dragenter|dragover|dragleave|drag|drop|dragend')) {
    $(el).bind(eventType, function(e) {
      fun( e.originalEvent.dataTransfer
            ? [e.originalEvent.dataTransfer.getData("dragData")]
            : [] );
    });
  } else if(eventType.match('mousemove|mousedown|mouseup')) {
    $(el).bind(eventType, function(e) {
      var offset = $(this).offset();
      var x      = e.pageX - offset.left;
      var y      = e.pageY - offset.top;
      fun([x, y]);
    });
  } else if(eventType.match('keydown|keyup')) {
    $(el).bind(eventType, function(e) {
      fun(e.keyCode);
      return true;
    });
  } else {
    $(el).bind(eventType, function(e) {
      fun(e.which ? [e.which.toString()] : []);
      return true;
    });
  }
};


/////////////////////////////////////////////////////////////////////
// Canvas API additions.
// See http://stackoverflow.com/a/9722502/403805 .
CanvasRenderingContext2D.prototype.clear = 
  CanvasRenderingContext2D.prototype.clear || function (preserveTransform) {
    if (preserveTransform) {
      this.save();
      this.setTransform(1, 0, 0, 1, 0, 0);
    }
    this.clearRect(0, 0, this.canvas.width, this.canvas.height);
    if (preserveTransform) {
      this.restore();
    }           
};


// Sendvalue
$.fn.sendvalue = function(trigger){
  $(this).each(function(){
    var self = this;
    var el = $(self);
    el.keydown(function(e){
      if(e.which == 13) {
        trigger.call(self,el.val());
        return false;
      }
      else
        return true;
    });
  });
};

// Livechange
$.fn.livechange = function(ms,trigger){
  $(this).each(function(){
    var self = this;
    var el = $(self);
    var last_val;
    var check = function(){
      var val = el.val();
      if(val != last_val)
        trigger.call(self);
      last_val = val;
    };
    var checker;
    var restart = function(){
      clearTimeout(checker);
      checker = setInterval(check,ms);
    };
    restart();
    el.keypress(restart).change(restart);
  });
};

