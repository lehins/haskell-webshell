
function initWesh() {
  const encoder = new TextEncoder();
  const ws = new WebSocket("ws://localhost:3000");

  const t = new hterm.Terminal("light");

  t.onTerminalReady = function() {
    // Create a new terminal IO object and give it the foreground.
    // (The default IO object just prints warning messages about unhandled
    // things to the the JS console.)
    const io = t.io.push();

    io.onVTKeystroke = (str) => {
      ws.send(encoder.encode(str));
    };

    io.sendString = (str) => {
      // Just like a keystroke, except str was generated by the terminal itself.
      ws.send(encoder.encode(str));
    };

  // You can call io.push() to foreground a fresh io context, which can
  // be uses to give control of the terminal to something else.  When that
  // thing is complete, should call io.pop() to restore control to the
  // previous io object.
  };

  ws.onopen = function(e) {
    console.log("Connected: ");
    console.log(e);
  }
  // Listen for the close connection event
  ws.onclose = function(e) {
    console.log("Disconnected: ");
    console.log(e);
  }

  // Listen for connection errors
  ws.onerror = function(e) {
    console.log("Error: ");
    console.log(e);
  }

  // Listen for new messages arriving at the client
  ws.onmessage = function(e) {
    console.log("Recieved: ");
    console.log(e);
    t.io.print(e.data);
  }

  t.decorate(document.querySelector('#terminal'));
  t.installKeyboard();

}

window.onload = function() {
  hterm.defaultStorage = new lib.Storage.Memory();
  lib.init(initWesh);
};
