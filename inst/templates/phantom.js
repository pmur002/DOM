// Do NOT have empty line at start of template, or whisker::whisker.render()
// will just return "" (!)
// Log function
dblog = function(msg) {
    console.log("++ RDOM JS: " + msg);
}
// Must load page from R server before trying to create websocket
// (otherwise violate the "Same Origin Policy")
var page = require('webpage').create();
page.onConsoleMessage = function(msg) {
    console.log(msg);
}
// Set page size
page.viewportSize = {
    width: {{{width}}},
    height: {{{height}}}
};
// NOTE that phantomjs wants 127.0.0.1 not 'localhost'
// ALSO phantomjs (2) wants http: protocol explicit 
// (otherwise it appears to assume file: protocol)
page.open("{{{url}}}", 
          function(status) {
              if (status === "success") {
                  // Load css-selector-generator library
                  page.injectJs("{{{CSGjs}}}");
                  page.injectJs("{{{bowserjs}}}");
                  page.injectJs("{{{RDOMjs}}}");
                  // dblog("opening socket")
                  page.evaluate(function(port, tag) { RDOM.init(port, tag) }, 
                                "{{{port}}}", "{{{tag}}}");
                  // dblog("socket initialised");
                  // Kill PhantomJS when websocket closes
                  page.onCallback = function(data) { 
                      if (data.type === "RENDER") {
                          // RDOM.js has been asked to render page
                          // Set default font to match visual browsers
                          page.evaluate(function() {
                              document.body.style.fontFamily = 'sans-serif';
                          });
                          page.render(data.outfile, 
                                      { format: 'png', quality: '100' });
                      } else if (data.type === "EXIT") {
                          // RDOM.js has closed the socket connection
                          phantom.exit() 
                      }
                  };
              } else {
                  // dblog("Failed to load page");
              }
          });
