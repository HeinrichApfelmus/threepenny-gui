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

(function(){

  ////////////////////////////////////////////////////////////////////////////////
  // State
  var sessionToken = null;
  var el_table = {};
  var tp_enable_log = $.cookie('tp_log') == "true";
  var signal_count = 0;

  document.head = document.head || document.getElementsByTagName('head')[0];

  ////////////////////////////////////////////////////////////////////////////////
  // Logging
  window.do_logging = function(x){
    $.cookie('tp_log',x.toString());
  };

  function console_log(){
    if (tp_enable_log) { window.console.log.apply(window.console,arguments); }
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Main entry point
  $(document).ready(function(){
    // initCommunicationHTTP();
    initCommunicationWebSockets();
  });

  ////////////////////////////////////////////////////////////////////////////////
  // Client-server communication
  // - GET and POST requests
  
  // Initialize communication via HTTP requests.
  function initCommunicationHTTP() {
    setTimeout(function(){
      waitForEvents();
    })
  }
  
  // Poll instruction from the server.
  function waitForEvents(){
    console_log("Polling… (%d signals so far)",signal_count);
    var data = { token: sessionToken };
    var cmd = sessionToken != null? 'poll' : 'init';
    if(cmd == 'init')
      data.info = window.location.href;
    var req = $.ajax({
      dataType: 'json',
      url: cmd,
      data: data,
      success: function(events){
        if(sessionToken == null) {
          sessionToken = req.getResponseHeader('Set-Token').match(/[0-9]+/)*1;
        }
        console_log("Running event" +(events.length>1?'s':'') +"…")
        if(events.length){
          console_log('Event list:');
          runMultipleEvents(events);
        } else {
          runEvent(events, signalEvent, function(response){
            maybeReply(response, waitForEvents);
          });
        }
      },
      error: function(reply){
        console_log("Error, waiting…");
        setTimeout(function(){
          waitForEvents();
        },5000);
      }
    });
  }

  function runMultipleEvents(events){
    if(events.length == 0) {
      return waitForEvents();
    }
    runEvent(events.shift(), signalEvent, function(response){
      maybeReply(response, function(){
        runMultipleEvents(events);
      });
    });
  }
  
  // Send an event to the server.
  function signalEvent(value) {
    signal({ Event: value}, function (){});
  }
  
  // Send a reply to the server if necessary.
  function maybeReply(response, continuation) {
    if (response != undefined) { signal(response, continuation); }
    else { continuation(); }
  }
  
  // Send response back to the server.
  function signal(signal,continuation){
    signal_count++;
    console_log('Signal: %s',JSON.stringify(signal));
    $.ajax({
      dataType: 'json',
      url:'signal',
      data: { token: sessionToken, signal: JSON.stringify(signal) },
      success: function(){
        continuation();
      },
      error: function(reply){
        console_log("Error: %o",reply);
      }
    });
  }
  
  ////////////////////////////////////////////////////////////////////////////////
  // Client-server communication
  // - WebSockets
  
  // Initialize client-server communication via WebSockets.
  function initCommunicationWebSockets() {
    var url = 'ws:' + window.location.href.toString().slice(5) + 'websocket';
    var ws  = new WebSocket(url);
    
    $(window).unload( function () {
      // Make sure that the WebSocket is closed when the browser window is closed.
      ws.close();
    });
    
    var sendEvent = function (elid, key, params) {
      ws.send(JSON.stringify({ Event : 
          { Element : { Element : elid }
          , EventId : key
          , Params  : params
          }
        }));
    }
    var reply     = function (response) {
      if (response != undefined)
        ws.send(JSON.stringify(response));
    }
    // Send ping message in regular intervals.
    // We expect pong messages in return to keep the connection alive.
    function ping(){
      ws.send("ping");
      window.setTimeout(ping,2000);
    }
    
    ws.onopen = function (e){
      ping();
      ws.onmessage = function (msg) {
        // console_log("WebSocket message: %o",msg);
        if (msg.data != "pong")
          { runEvent(JSON.parse(msg.data), sendEvent, reply); }
      }
      ws.onclose = function (e) {
        console_log("WebSocket closed: %o", e);
      }
      ws.onerror = function (e) {
        console_log("WebSocket error: %o", e);
      }
    }
  }


  ////////////////////////////////////////////////////////////////////////////////
  // FFI - Execute and reply to commands from the server
  
  function runEvent(event,sendEvent,reply){
    // reply();      -- Continue without replying to the server.
    // reply(value); -- Send  value  back to the server.
    // sendEvent     -- Function that sends a message { Event : value } to the server.
  
    console_log("Event: %s",JSON.stringify(event));
    switch(event.tag){

    case "CallDeferredFunction": {
      // FIXME: CallDeferredFunction probably doesn't work right now.
      var call        = event.contents;
      var closure     = call[0];
      var theFunction = eval(call[1]);
      var params      = call[2];
      theFunction.apply(window, params.concat(function(){
        console_log(this);
        var args = Array.prototype.slice.call(arguments,0);
        sendEvent(closure[0],closure[1],args);
      }));
      reply();
      break;
    }
    case "RunJSFunction": {
        eval(event.contents);
        reply();
        break;
    }
    case "CallJSFunction": {
        var result = eval(event.contents);
        reply({FunctionResult : result});
        break;
    }
    case "Delete": {
        deleteElid(event.contents);
        reply();
        break;
    }
    case "Debug": {
        if(window.console)
          console.log("Server debug: %o",event.contents);
        reply();
        break;
    }
    case "GetValues": {
        var ids = event.contents;
        var len = ids.length;
        var values = [];
        for(var i = 0; i < len; i++) {
          values.push($(elidToElement(ids[i])).val());
        }
        reply({ Values: values });
        break;
    }
    case "Bind": {
        var bind        = event.contents;
        var eventType   = bind[0];
        var elid        = bind[1];
        var el          = elidToElement(elid);
        console_log('event type: ' + eventType);
        if(eventType == 'livechange') {
          $(el).livechange(300,function(e){
            sendEvent(elid,eventType, [$(el).val()]);
            return true;
          });
        } else if(eventType == 'sendvalue') {
          $(el).sendvalue(function(x){
            sendEvent(elid,eventType, [x]);
          });
        } else if(eventType.match('dragstart|dragenter|dragover|dragleave|drag|drop|dragend')) {
          $(el).bind(eventType,function(e){
            sendEvent(elid,eventType,
                e.originalEvent.dataTransfer
                    ? [e.originalEvent.dataTransfer.getData("dragData")]
                    : []
              );
            return true;
          });
        } else if(eventType.match('mousemove|mousedown|mouseup')) {
          $(el).bind(eventType,function(e){
            var offset = $(this).offset();
            var x      = e.pageX - offset.left;
            var y      = e.pageY - offset.top;         
            sendEvent(elid,eventType, [x.toString(), y.toString()]);
            return true;
          });
        } else if(eventType.match('keydown|keyup')) {
          $(el).bind(eventType,function(e){
            sendEvent(elid,eventType, [e.keyCode.toString()]);
            return true;
          });
        } else {
          $(el).bind(eventType,function(e){
            sendEvent(elid,eventType, e.which ? [e.which.toString()] : []);
            return true;
          });
        }
        reply();
        break;
    }
    default: reply();
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  // FFI - marshaling

  // When the server creates elements, it assigns them a string "elid".  
  // This elidToElement function is used to sync the elids on the server with the 
  // elids on this client code.  Lookups on elids that do not already exist in the client
  // table are created and added automatically.
  function elidToElement(elid){
    if(elid == 'body')
      return document.body;
    else if(elid == 'head')
      return document.head;
    else if(elid[0] == '*')
      return newElement(elid);
    else
      throw "Unknown element: " + elid;
  }

  // Function to leverage the DOM2 createElementNS() function that creates DOM
  // elements based on tags defined in HTML 5 namespaces. Elements/elids are cached
  // and returned if subsequently requested. Current implementation supports only 
  // HTML and SVG namespaces.
  // TODO: Opportunity to support all HTML 5 namespaces, 
  //         http://www.w3.org/TR/2011/WD-html5-20110113/namespaces.html
  function newElement(elid) {
    var create = elid.split(':');
    if( create[1] == "svg"){
      ns    = "http://www.w3.org/2000/svg";
      tag   = create[2];
      elid_ = create[0] + ":" + create[2];
    }
    else {
      ns    = "http://www.w3.org/1999/xhtml";
      tag   = create[1];
      elid_ = elid;
    }
    if(el_table[elid_]) return el_table[elid_];
    element         = document.createElementNS(ns, tag);
    element.elid    = elid_;
    el_table[elid_] = element;
    return element;
  }
 
  // Get/generate a elid for an element.  This function is used for cases in which the
  // element is accessed without knowing an elid from the server, such as when the 
  // element is retrieved by type or html ID attribute.  The element is then added to 
  // elid lookup table using the new elid.
  // Note: The mapping between  elids  and  DOM elements  must be bijective.
  function elementToElid(element){
    if(element.elid)
      return element.elid;
	  else if (element === document.body)
      return "body";
	  else if (element === document.head)
      return "head";
    else {
      throw "Element requested, but does not have elid: " + element;
    }
  }
  
  // plural of the mapping from elements to elids
  function elementsToElids(elements){
    var els = [], match;
    for(var i = 0; i < elements.length; i++) {
      match = elements[i];
      if (match != null) {
        els.push({ Element: elementToElid(match) });
      }
    }
    return els;
  }
  
  // Delete element from the table
  function deleteElid(elid){
    var el = el_table[elid];
    if (el) {
      $(el).detach(); // Should be detached already, but make sure
      delete el_table[elid];
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  // FFI - additional primitive functions
  
  window.jquery_animate = function(el_id,props,duration,easing,complete){
    var el = elidToElement(JSON.parse(el_id));
    $(el).animate(JSON.parse(props),
                  duration * 1,
                  easing * 1,
                  complete);
  }

  window.jquery_scrollToBottom = function(el){
    $(el).scrollTop(el.scrollHeight);
  };

  function prim_audio_stop(audio){
    audio.pause();
    audio.currentTime = 0;
  }

  // see http://stackoverflow.com/a/9722502/403805
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

})();
