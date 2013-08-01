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
  var element_count = 0, el_table = {};
  var tp_enable_log = $.cookie('tp_log') == "true";
  var signal_count = 0;

  document.head = document.head || document.getElementsByTagName('head')[0];

  ////////////////////////////////////////////////////////////////////////////////
  // Main entry point
  $(document).ready(function(){
    setTimeout(function(){
      waitForEvents();
    })
  });

  ////////////////////////////////////////////////////////////////////////////////
  // Running instructions

  window.do_logging = function(x){
    $.cookie('tp_log',x.toString());
  };
  
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
          runEvent(events,function(){
            waitForEvents();
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
    runEvent(events.shift(),function(){
      runMultipleEvents(events);
    });
  }

  function runEvent(event,continuation){
    console_log("Event: %s",JSON.stringify(event));
    for(var key in event){
      switch(key){
                
      case "EmptyEl": {
        var id = event.EmptyEl;
        var el = elidToElement(id);
        // Detach child elements without deleting associated event handlers and data.
        // It is not correct to remove the child elements from the el_table
        // because they may still be present on the server side.
        $(el).contents().detach();
        continuation();
        break;
      }
      case "CallDeferredFunction": {
        var call = event.CallDeferredFunction;
        var closure = call[0];
        var theFunction = eval(call[1]);
        var params = call[2];
        theFunction.apply(window, params.concat(function(){
          console_log(this);
          var args = Array.prototype.slice.call(arguments,0);
          signal({
            Event: closure.concat([args])
          },function(){
            // No action.
          });
        }));
        continuation();
        break;
      }
      case "RunJSFunction": {
        eval(event.RunJSFunction);
        continuation();
        break;
      }
      case "CallJSFunction": {
        var result = eval(event.CallJSFunction);
        signal({FunctionResult : result}, continuation);
        break;
      }
      case "Delete": {
        event_delete(event);
        continuation();
        break;
      }
      case "Debug": {
        if(window.console)
          console.log("Server debug: %o",event.Debug);
        continuation();
        break;
      }
      case "Clear": {
        $('body').contents().detach();
        continuation();
        break;
      }
      case "GetElementsByTagName": {
        var elements = document.getElementsByTagName(event.GetElementsByTagName);
        var els = [];
        var len = elements.length;
        for(var i = 0; i < len; i++) {
          els.push({
            Element: elementToElid(elements[i])
          });
        }
        signal({
          Elements: els
        },function(){
          continuation();
        });
        break;
      }
      case "GetElementsById": {
        // Note that this is the html ID, not the elid that is the key of the el_table.
        var ids = event.GetElementsById;
        var els = [];
        for(var i = 0; i < ids.length; i++) {
            var match = document.getElementById(ids[i]);
            if (match != null) {
                els.push({
                  Element: elementToElid(match)
                });
            }
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
        var el = elidToElement(id);
        var len = style.length;
        for(var i = 0; i < len; i++){
          el.style[style[i][0]] = style[i][1];
        }
        continuation();
        break;
      }
      case "SetAttr": {
        var set   = event.SetAttr;
        var id    = set[0];
        var key   = set[1];
        var value = set[2];
        var el    = elidToElement(id);
        $(el).attr(key,value);
        continuation();
        break;
      }
      case "GetValue": {
        var id = event.GetValue;
        var el = elidToElement(id);
        var value = $(el).val();
        signal({
          Value: value
        },function(){
          continuation();
        });
        break;
      }
      case "GetLocation": {
        signal({
          Location: window.location.href
        },function(){
          continuation();
        });
        break;
      }
      case "GetValues": {
        var ids = event.GetValues;
        var len = ids.length;
        var values = [];
        for(var i = 0; i < len; i++) {
          values.push($(elidToElement(ids[i])).val());
        }
        signal({
          Values: values
        },function(){
          continuation();
        });
        break;
      }
      case "Append": {
        var append = event.Append;
        $(elidToElement(append[0])).append($(elidToElement(append[1])));
        continuation();
        break;
      }
      case "SetText": {
        var set = event.SetText;
        $(elidToElement(set[0])).text(set[1]);
        continuation();
        break;
      }
      case "SetTitle": {
        document.title = event.SetTitle;
        continuation();
        break;
      }
      case "SetHtml": {
        var set = event.SetHtml;
        $(elidToElement(set[0])).html(set[1]);
        continuation();
        break;
      }
      case "Bind": {
        var bind        = event.Bind;
        var eventType   = bind[0];
        var handlerGuid = bind[2];
        var el = elidToElement(bind[1]);
        console_log('event type: ' + eventType);
        if(eventType == 'livechange') {
          $(el).livechange(300,function(e){
            signal({
              Event: handlerGuid.concat([[$(el).val()]])
            },function(){
              // no action
            });
            return true;
          });
        } else if(eventType == 'sendvalue') {
          $(el).sendvalue(function(x){
            signal({
              Event: handlerGuid.concat([[x]])
            },function(){});
          });
        } else if(eventType.match('dragstart|dragenter|dragover|dragleave|drag|drop|dragend')) {
          $(el).bind(eventType,function(e){
            signal({
              Event: handlerGuid.concat([
                e.originalEvent.dataTransfer
                    ?[e.originalEvent.dataTransfer.getData("dragData")]
                    :[]])
            },function(){
              // no action
            });
            return true;
          });
        } else if(eventType.match('mousemove')) {
          $(el).bind(eventType,function(e){
            signal({
              Event: handlerGuid.concat([[e.pageX.toString(), e.pageY.toString()]])
            },function(){
              // no action
            });
            return true;
          });
        } else {
          $(el).bind(eventType,function(e){
            signal({
              Event: handlerGuid.concat([e.which?[e.which.toString()]:[]])
            },function(){
              // no action
            });
            return true;
          });
        }
        continuation();
        break;
      }
      default: continuation();
      }
    }
  }

  function event_delete(event){
    var id = event.Delete;
    var el = elidToElement(id);
    // TODO: Think whether it is correct to remove element ids
    $(el).detach();
    deleteElementTable(id);
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Signalling events

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

  // When the server creates elements, it assigns them a string "elid".  
  // This elidToElement function is used to sync the elids on the server with the 
  // elids on this client code.  Lookups on elids that do not already exist in the client
  // table are created and added automatically.
  function elidToElement(elid){
    if(elid == 'body')
      return document.body;
    else if(elid == 'head')
      return document.head;
    else if(el_table[elid]){
      return el_table[elid];
    } else {
      if(elid[0] == '*'){
        var create = elid.split(':');
        var element = document.createElement(create[1]);
        element.elid   = elid;
        el_table[elid] = element;
        return element;
      } else {
        throw "Unknown element: " + elid;
      }
    }
  }
  function deleteElementTable(elid){
    delete el_table[elid];
  }
  
  // Get/generate a elid for an element.  This function is used for cases in which the
  // element is accessed without knowing an elid from the server, such as when the 
  // element is retrieved by type or html ID attribute.  The element is then added to 
  // elid lookup table using the new elid.
  // Note: The mapping between  elids  and  DOM elements  must be bijective.
  function elementToElid(element){
    if(element.elid) {
        return element.elid;
	}
	else if (element === document.body) {
        return "body";
    }
	else if (element === document.head) {
        return "head";
    }
    else {
        var elid = "!" + element_count.toString();
        element_count++;
        element.elid   = elid;
        el_table[elid] = element;
        return elid;
    }
  }

  // A log
  function console_log(){
    if (tp_enable_log) {
      window.console.log.apply(window.console,arguments);
    }
  };

})();
