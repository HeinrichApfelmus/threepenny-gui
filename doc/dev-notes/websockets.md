Development Notes on Websockets
===============================

Turns out that WebSocket implementations in browsers are horrible.

Issues:

1. WebSocket objects get closed unexpectedly on the client.

    To the server, it looks like the connection is still alive,
    it does not throw a  ConnectionClosed  exception.
    In fact, the connection may still be alive, see 3.

    The JS object on the client does receive an `onclose` message.
    
    The JS object is closed even when the client is sending data
    periodically!
    
    Fortunately, the JS object is *not* closed when the server
    sends data periodically.

2. WebSocket connections can persist over a browser reload. WTF?

    Opening a new WebSocket will instead reuse the old socket,
    and the server will send data to both connections (which are just one).
    
    In fact, if the read end on the client closes and the server
    throws a `ConnectionClosed` exception, the write end may still be open.


Facts:

* When the  websocket.close();  function is called,
  the server will throw a proper `ConnectionClosed` exception.

* When the browser window is closed or reloaded,
  the server will also throw a `ConncetionClosed` exception.


Support:

* Safari `5.1.9` only supports the `Hybi00` protocol.

