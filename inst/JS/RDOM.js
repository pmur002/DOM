// Single global 'RDOM' object
RDOM = (function(){

    // INTERNALS
    
    // Log for debugging
    var debug;
    var dblog = function(msg) {
        if (debug)
            console.log("++ RDOM JS: " + msg);
    }

    // The websocket connection to R
    var ws;
    
    // CSS selector generator object
    var CSG = new CssSelectorGenerator();

    // Utils for tracking requests from JS to R
    var requestID = 0;
    var getRequestID = function() {
        var result = requestID;
        requestID = requestID + 1;
        return result;
    }
    var requests = [];
    var addRequest = function(id, callback) {
        requests[id] = { callback: callback };
    }
    var removeRequest = function(id) {
        requests[id] = null;
    }
    var getRequest = function(id) {
        return requests[id];
    }

    // Utils for handling requests from R to JS
    var resolveTarget = function(target, css) {
        if (css) {
            return document.querySelector(target);
        } else {
            return document.evaluate(target, document, 
                                     null, XPathResult.ANY_TYPE, null);
        }
    }

    // Utils for generating requests from JS to R
    var requestValue = function(fn, element, selector, tag) {
        return { type: "REQUEST",
                 tag: tag,
                 body: {
                     fn: fn,
                     target: element.outerHTML,
                     targetRef: selector
                 }
               }
    }
    var returnValue = function(tag, value) {
        return { type: "RESPONSE",
                 tag: tag,
                 body: value
               }    
    }   
    var errorValue = function(tag, err) {
        return { type: "ERROR",
                 tag: tag,
                 body: err
               }    
    }

    // Main function for handling requests from R to JS
    var handleMessage = function(msg) {
        dblog("RECEIVING " + msg.data);
        var msgJSON = JSON.parse(msg.data);

        handleRequest = function() {
            var result = "";
            var msgBody = msgJSON.body;
            switch(msgBody.fun[0]) {
            case "appendChild": // parent, child, css
                var child;
                if (msgBody.byRef[0]) {
                    child = resolveTarget(msgBody.child[0], msgBody.css[0]);
                } else {
                    var container = document.createElement("div");
                    container.innerHTML = msgBody.child[0];
                    child = container.firstChild;
                }
                var parent = resolveTarget(msgBody.parent[0], msgBody.css[0]);
                dblog("ADDING " + child.toString() + 
                      " TO " + parent.toString());
                parent.appendChild(child);
                if (msgBody.returnRef[0]) {
                    var selector = CSG.getSelector(child);
                    result = returnValue(msgJSON.tag, selector);
                } else {
                    result = returnValue(msgJSON.tag, child.outerHTML);
                }
                break;
            case "removeChild": // child, parent, css
                var error = false;
                var child = resolveTarget(msgBody.child[0], msgBody.css[0]);
                var parent;
                if (msgBody.parent === null) { 
                    parent = child.parentElement;
                } else {
                    parent = resolveTarget(msgBody.parent[0], msgBody.css[0]);
                }
                dblog("REMOVING " + child.toString() + 
                      " FROM " + parent.toString());
                if (msgBody.returnRef[0]) {
                    var selector = CSG.getSelector(child);
                    result = returnValue(msgJSON.tag, selector);
                } else {
                    result = returnValue(msgJSON.tag, child.outerHTML);
                }
                // Remove child AFTER determining its CSS selector !
                parent.removeChild(child);
                break;
            case "replaceChild": // newchild, oldchild, parent, css
                var newChild;
                if (msgBody.byRef[0]) {
                    newChild = resolveTarget(msgBody.newChild[0], 
                                             msgBody.css[0]);
                } else {
                    var container = document.createElement("div");
                    container.innerHTML = msgBody.newChild[0];
                    newChild = container.firstChild;
                }
                var oldChild = resolveTarget(msgBody.oldChild[0], 
                                             msgBody.css[0]);
                var parent;
                if (msgBody.parent === null) { 
                    parent = oldChild.parentElement;
                } else {
                    parent = resolveTarget(msgBody.parent[0], msgBody.css[0]);
                }
                dblog("REPLACING " + oldChild.toString() + 
                      " WITH " + newChild.toString());
                if (msgBody.returnRef[0]) {
                    var selector = CSG.getSelector(oldChild);
                    result = returnValue(msgJSON.tag, selector);
                } else {
                    result = returnValue(msgJSON.tag, oldChild.outerHTML);
                }
                // Replace oldChild AFTER determining its CSS selector !
                parent.replaceChild(newChild, oldChild);
                break;
            case "setAttribute": // elt, attr, value, css
                var element = resolveTarget(msgBody.elt[0], msgBody.css[0]);
                element.setAttribute(msgBody.attr[0], msgBody.value[0]);
                result = returnValue(msgJSON.tag, "");
                break;
            case "appendScript": // script, css
                // Distinct from "appendChild" because of the way we need
                // to build a script so that it not only gets ADDED, but
                // also gets RUN
                var script = document.createElement("script");
                script.innerHTML = msgBody.script[0];
                var parent = resolveTarget(msgBody.parent[0], msgBody.css[0]);
                dblog("ADDING " + script.toString() + 
                      " TO " + parent.toString());
                parent.appendChild(script);
                result = returnValue(msgJSON.tag, "");
                break;
            case "click":
                var element = resolveTarget(msgBody.elt[0], msgBody.css[0]);
                var event = document.createEvent( 'MouseEvents' );
                event.initMouseEvent( 'click', true, true, window, 1, 0, 0 );
                element.dispatchEvent( event );
                result = returnValue(msgJSON.tag, "");
                break;
            default:
                throw new Error("Unsupported DOM request");
                break;
            }

            return result;
        }
        
        if (msgJSON.type[0] === "DEBUG") {
            debug = true;

        } else if (msgJSON.type[0] === "PREPARETODIE") {
	    var msg = JSON.stringify({ type: "DEAD",
                                       tag: msgJSON.tag[0],
                                       body: document.documentElement.outerHTML
                                     });
            ws.send(msg);
	    
        } else if (msgJSON.type[0] === "DIE") {
	    ws.close();
	    
        } else if (msgJSON.type[0] === "REQUEST") {
            var result = "";
            try {
                result = handleRequest();
            } catch(err) {
                result = errorValue(msgJSON.tag, err);
            }

            dblog("SENDING " + JSON.stringify(result));
            ws.send(JSON.stringify(result)); 

        } else if (msgJSON.type[0] === "RESPONSE") {
            // Match response tag to request tag and eval callback (if any)
            var request = getRequest(msgJSON.tag[0]);
            if (request.callback != null) {
                request.callback(msgJSON.body);
            }

        } else {
            throw new Error("Unknown message type");
        }
    }

    // EXPORTED functions

    // 'onclose' allows PhantomJS to supply phantom.exit (?)
    var init = function(port, tag) {
	// Use 127.0.0.1 instead of 'localhost' to keep PhantomJS happy (?)
	ws = new WebSocket("ws://127.0.0.1:" + port);
	// Set up websocket methods
	ws.onopen = function() {
            ws.send(JSON.stringify({ type: "ALIVE", 
                                     tag: tag }));
            dblog("Connection opened");
	}
	ws.onerror = function(evt) { 
            msg = "An error occurred with the WebSocket. " +
		"Has the R server been started?";
            dblog(msg);
	};
	ws.onclose = function(evt) {
            dblog("Connection closed");
	    // If we are running PhantomJS, exit when websocket closed
	    // (encompasses DIE request AND errors that close websocket)
	    if (typeof window.callPhantom === 'function') {
		window.callPhantom({ type: "EXIT" });
	    }
	}
	ws.onmessage = function(evt) {
	    handleMessage(evt);
	};	
        // Turn debugging on/off
        debug = debug;
    }
    
    // 'fn' is name of R function (string)
    // 'args' is JSON object
    Rcall = function(fn, element, callback) {
        var tag = getRequestID();
        addRequest(tag, callback);
        var selector = CSG.getSelector(element);
        var request = requestValue(fn, element, selector, tag);
        var msgJSON = JSON.stringify(request);
        dblog("SENDING " + msgJSON);
        ws.send(msgJSON); 
    }

    return {
	init: init,
	Rcall: Rcall
    }
}());
