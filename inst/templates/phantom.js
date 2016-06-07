// Do NOT have empty line at start of template, or whisker::whisker.render()
// will just return "" (!)
// Log function
log = function(msg) {
    console.log("R package DOM: " + msg);
}
// Must load page from R server before trying to create websocket
// (otherwise violate the "Same Origin Policy")
var page = require('webpage').create();
page.onConsoleMessage = function(msg) {
    console.log(msg);
}
// NOTE that phantomjs wants 127.0.0.1 not 'localhost'
// ALSO phantomjs (2) wants http: protocol explicit 
// (otherwise it appears to assume file: protocol)
page.open("{{{url}}}", 
          function(status) {
             if (status === "success") {
                 // Load css-selector-generator library
                 page.injectJs("{{{CSGjs}}}");
                 log("opening socket")
                  ws = new WebSocket("ws://127.0.0.1:{{{port}}}");
                  log("socket: " + ws.toString());
                  initSocket(ws, "{{{tag}}}");
                  log("socket initialised");
              } else {
                  log("Failed to load page");
              }
          });
