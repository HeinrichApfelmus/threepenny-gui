/* *********************************************************************
  Client-server communication
  using WebSockets
  
  The function  createWebSocket(receive)  creates a client-server
  connection. The argument function `receive` is called whenever the client
  receives a message. The connection object has a single member `send`
  which can be used to send messages to the server.
  
  In Haskell types:
  
  createWebSocket :: (JSON -> IO ()) -> IO { send :: JSON -> IO () }
  
********************************************************************* */
Haskell.createWebSocket = function (url0, receive) {
  var that = {};
  var url  = 'ws:' + url0.slice(5) + 'websocket/';
  var ws   = new WebSocket(url);
  
  // Close WebSocket when the browser window is closed.
  $(window).unload( function () {
    ws.close();
  });

  // Send ping message in regular intervals.
  // We expect pong messages in return to keep the connection alive.
  var ping = function () {
    ws.send("ping");
    window.setTimeout(ping,2000);
  };
  
  // Start communication when the WebSocket is opened.
  ws.onopen = function (e) {
    ping();
    ws.onmessage = function (msg) {
      // Haskell.log("WebSocket message: %o",msg);
      if (msg.data !== "pong") {
        receive(JSON.parse(msg.data));
      }
    };
    ws.onclose = function (e) {
      Haskell.log("WebSocket closed: %o", e);
    };
    ws.onerror = function (e) {
      Haskell.log("WebSocket error: %o", e);
    };
  };
  
  // Send a JSON message to the server
  that.send = function (json) {
    ws.send(JSON.stringify(json));
  };
  
  return that;
};


/* *********************************************************************
  Client-server communication
  using HTTP
  
  WARNING: The following code is untested legacy code.
  It's only there because there may be a chance we want to resurrect it.
  
********************************************************************* */

/* *********************************************************************
Threepenny.createHTTP = function (receive) {
  var that;
  var signal_count = 0;
  var sessionToken = {};
  
  window.setTimeout(function () {
    waitForEvents();
  });
    
  // Poll instruction from the server.
  var waitForEvents = function () {
    console.log("Polling… (%d signals so far)",signal_count);
    var data = { token: sessionToken };
    var cmd  = sessionToken != null? 'poll' : 'init';
    if (cmd === 'init') {
      data.info = window.location.href;
    }
    var req = $.ajax({
      dataType: 'json',
      url : cmd,
      data: data,
      success: function (events) {
        if (sessionToken == null) {
          sessionToken = req.getResponseHeader('Set-Token').match(/[0-9]+/)*1;
        }
        console.log("Running event" +(events.length>1?'s':'') +"…")
        if (events.length) {
          console.log('Event list:');
          runMultipleEvents(events);
        } else {
          runEvent(events, signalEvent, function(response){
            maybeReply(response, waitForEvents);
          });
        }
      },
      error: function(reply) {
        console.log("Error, waiting...");
        window.setTimeout(function(){
          waitForEvents();
        },5000);
      }
    });
  }

  var runMultipleEvents = function (events){
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
  var signalEvent = function (value) {
    signal({ Event: value}, function (){});
  }
  
  // Send a reply to the server if necessary.
  var maybeReply = function (response, continuation) {
    if (response != undefined) { signal(response, continuation); }
    else { continuation(); }
  }
  
  // Send response back to the server.
  var signal = function signal(signal,continuation) {
    signal_count++;
    console.log('Signal: %s',JSON.stringify(signal));
    $.ajax({
      dataType: 'json',
      url:'signal',
      data: { token: sessionToken, signal: JSON.stringify(signal) },
      success: function(){
        continuation();
      },
      error: function(reply){
        console.log("Error: %o",reply);
      }
    });
  }
  
  return that;
};
********************************************************************* */
