////////////////////////////////////////////////////////////////////////////////
// State
var sessionToken = null;
var global_ids = [], element_count = 0, el_table = [];
var transaction = false;
//console.log = function(){};

////////////////////////////////////////////////////////////////////////////////
// Main entry point
$(document).ready(function(){
  setTimeout(function(){
    waitForEvents();
  })
});

////////////////////////////////////////////////////////////////////////////////
// Running instructions

function waitForEvents(){
  console.log("Polling…");
  $.ajax({
    dataType: 'json',
    url:'/' + (sessionToken? 'poll' : 'init'),
    data: { token: sessionToken },
    success: function(events){
      console.log("Running event" +(events.length>1?'s':'') +"…")
      if(events.length){
        console.log('Event list:');
        runMultipleEvents(events);
      } else {
        runEvent(events,function(){
          waitForEvents();
        });
      }
    },
    error: function(reply){
      console.log("Error, waiting…");
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
  runEvent(events.shift(),function(){
    runMultipleEvents(events);
  });
}

function runEvent(event,continuation){
  console.log("Event: %s",JSON.stringify(event));
  for(var key in event){
    switch(key){
    case "Begin": {
      transaction = true;
      continuation();
      break;
    }
    case "End": {
      transaction = false;
      continuation();
      break;
    }
    case "SetToken": {
      sessionToken = event.SetToken;
      signal({
        Init: []
      },function(){
        continuation();
      });
      break;
    }
    case "Debug": {
      console.log("Server debug: %o",event.Debug);
      continuation();
      break;
    }
    case "Clear": {
      $('body').empty();
      continuation();
      break;
    }
    case "GetElementsByTagName": {
      var elements = document.getElementsByTagName(event.GetElementsByTagName);
      var els = [];
      var len = elements.length;
      for(var i = 0; i < len; i++) {
        els.push({
          Element: getElementGuid(elements[i])
        });
      }
      signal({
        Elements: els
      },function(){
        continuation();
      });
      break;
    }
    case "SetStyle": {
      var set = event.SetStyle;
      var id = set[0];
      var style = set[1];
      var el = lookupElementTable(id);
      var len = style.length;
      for(var i = 0; i < len; i++){
        el.style[style[i][0]] = style[i][1];
      }
      continuation();
      break;
    }
    case "SetAttr": {
      var set = event.SetAttr;
      var id = set[0];
      var key = set[1];
      var value = set[2];
      var el = lookupElementTable(id);
      $(el).attr(key,value);
      continuation();
      break;
    }
    case "GetValue": {
      var id = event.GetValue;
      var el = lookupElementTable(id);
      var value = $(el).val();
      signal({
        Value: value
      },function(){
        continuation();
      });
      break;
    }
    case "NewElement": {
      var el = document.createElement(event.NewElement);
      signal({
        SingleEl1ement: { Element: getElementGuid(el) }
      },function(){
        continuation();
      });
      break;
    }
    case "Append": {
      var append = event.Append;
      $(lookupElementTable(append[0])).append($(lookupElementTable(append[1])));
      continuation();
      break;
    }
    case "SetText": {
      var set = event.SetText;
      $(lookupElementTable(set[0])).text(set[1]);
      continuation();
      break;
    }
    case "SetHtml": {
      var set = event.SetHtml;
      $(lookupElementTable(set[0])).html(set[1]);
      continuation();
      break;
    }
    case "Bind": {
      var bind = event.Bind;
      var eventType = bind[0];
      var handlerGuid = bind[2];
      var el = lookupElementTable(bind[1]);
      console.log('event type: ' + eventType);
      $(el).bind(eventType,function(){
        if(!transaction) {
          signal({
            Event: handlerGuid
          },function(){
            // no action
          });
        }
        return false;
      });
      continuation();
      break;
    }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////
// Signalling events

function signal(signal,continuation){
  console.log('Signal: %s',JSON.stringify(signal));
  $.ajax({
    dataType: 'json',
    url:'/signal',
    data: { token: sessionToken, signal: JSON.stringify(signal) },
    success: function(){
      continuation();
    },
    error: function(reply){
      console.log("Error: %o",reply);
    }
  });
}

function lookupElementTable(elid){
  if(el_table[elid]){
    return el_table[elid];
  } else {
    if(elid[0] == '*'){
      var create = elid.split(':');
      var el = document.createElement(create[1]);
      el_table[elid] = el;
      return el;
    } else {
      throw "Unknown element: " + elid;
    }
  }
}

// Get/generate a guid for an element

function getElementGuid(element){
  if(element.id) return element.id;
  else {
    if(global_ids.length > 0) {
      var id = global_ids.pop();
      element.id = id;
      el_table[id] = element;
      return id;
    } else {
      var id = element_count.toString();
      element.id = id;
      el_table[id] = element;
      element_count++;
      return id;
    }
  }
}
