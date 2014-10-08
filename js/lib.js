/* *********************************************************************
  Threepenny - JavaScript library
  
  Additional JavaScript functions that are available
  for Threepenny.
  
********************************************************************* */
window.jquery_scrollToBottom = function(el){
  $(el).scrollTop(el.scrollHeight);
};

function prim_audio_stop(audio){
  audio.pause();
  audio.currentTime = 0;
}

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

