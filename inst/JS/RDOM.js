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

    var parser = new DOMParser();
    var serializer = new XMLSerializer;

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
            return document.evaluate(target, 
                                     document, 
                                     null, 
                                     XPathResult.FIRST_ORDERED_NODE_TYPE,
                                     null).singleNodeValue;
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
    var returnValue = function(tag, fn, value) {
        return { type: "RESPONSE",
                 tag: tag,
                 body: {
                     fn: fn,
                     value: value
                 }
               }    
    }   
    var errorValue = function(tag, err) {
        return { type: "ERROR",
                 tag: tag,
                 body: err
               }    
    }
    var pageContent = function() {
        return document.documentElement.outerHTML
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
                var container;
                switch (msgBody.childType[0]) {
                case "DOM_node_HTML":
                    if (msgBody.ns[0]) {
                        // "text/xml" rather than "text/html" 
                        // to satisfy PhantomJS
                        container = parser.parseFromString(msgBody.child[0],
                                                           "text/xml");
                    } else {
                        container = document.createElement("div");
                        container.innerHTML = msgBody.child[0];
                    }        
                    child = container.firstChild;
                    break;
                case "DOM_node_SVG":
                    container = parser.parseFromString(msgBody.child[0],
                                                       "image/svg+xml");
                    child = container.firstChild;
                    break;
                case "DOM_node_CSS":
                    child = resolveTarget(msgBody.child[0], true);
                    break;
                case "DOM_node_XPath":
                    child = resolveTarget(msgBody.child[0], false);
                case "DOM_node_ptr":
                    throw new Error("DOM_node_ptr support not yet implemented");
                    break;
                }
                var parent;
                switch (msgBody.parentType[0]) {
                case "DOM_node_CSS":
                    parent = resolveTarget(msgBody.parent[0], true);
                    break;
                case "DOM_node_XPath":
                    parent = resolveTarget(msgBody.parent[0], false);
                    break;
                case "DOM_node_ptr":
                    throw new Error("DOM_node_ptr support not yet implemented");
                    break;
                }
                dblog("ADDING " + child.toString() + 
                      " TO " + parent.toString());

                parent.appendChild(child);

                switch (msgBody.responseType[0]) {
                case "DOM_node_HTML":
                case "DOM_node_SVG":
                    if (msgBody.ns[0]) {
                        result = returnValue(msgJSON.tag, msgBody.fun[0],
                                             serializer.serializeToString(child));
                    } else {
                        result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                             child.outerHTML);
                    }                    
                    break;
                case "DOM_node_CSS":
                    var selector = CSG.getSelector(child);
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         selector);
                    break;
                case "DOM_node_XPath":
                    throw new Error("DOM_node_XPath support not yet implemented");
                    break;
                case "DOM_node_ptr":
                    throw new Error("DOM_node_ptr support not yet implemented");
                    break;
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
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         selector);
                } else {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         child.outerHTML);
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
                    var container;
                    if (msgBody.ns != null) {
                        if (msgBody.ns[0] === "HTML") {
                            container = document.createElementNS("http://www.w3.org/1999/xhtml", "div");
                        } else if (msgBody.ns[0] === "SVG") {
                            container = document.createElementNS("http://www.w3.org/2000/svg", "svg");                            
                        } else {
                            throw new Error("Unsupported namespace");
                        }
                    } else {
                        container = document.createElement("div");
                    }        
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
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         selector);
                } else {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         oldChild.outerHTML);
                }
                // Replace oldChild AFTER determining its CSS selector !
                parent.replaceChild(newChild, oldChild);
                break;
            case "setAttribute": // elt, attr, value, css
                var element = resolveTarget(msgBody.elt[0], msgBody.css[0]);
                element.setAttribute(msgBody.attr[0], msgBody.value[0]);
                result = returnValue(msgJSON.tag, msgBody.fun[0], null);
                break;
            case "getElementById": // id
                var element = document.getElementById(msgBody.id[0]);
                if (element === null) {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], null);
                } else if (msgBody.returnRef[0]) {
                    var selector = CSG.getSelector(element);
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         selector);
                } else {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         element.outerHTML);
                }
                break;
            case "getElementsByTagName": // name ('*' is special)
                var elements = document.getElementsByTagName(msgBody.name[0]);
                if (elements.length === 0) {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], null);
                } else if (msgBody.returnRef[0]) {
                    var css = [];
                    for (i = 0; i < elements.length; i++) {
                        css.push(CSG.getSelector(elements[i]));
                    }
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         css);
                } else {                
                    var html = [];
                    for (i = 0; i < elements.length; i++) {
                        html.push(elements[i].outerHTML);
                    }
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         html);
                }
                break;
            case "getElementsByClassName": // name, rootRef
                var elements;
                if (msgBody.root === null) {
                    elements = document.getElementsByClassName(msgBody.name[0]);
                } else {
                    var parent = resolveTarget(msgBody.root[0], msgBody.css[0]);
                    elements = parent.getElementsByClassName(msgBody.name[0]);
                }
                if (elements.length === 0) {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], null);
                } else if (msgBody.returnRef[0]) {
                    var css = [];
                    for (i = 0; i < elements.length; i++) {
                        css.push(CSG.getSelector(elements[i]));
                    }
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         css);
                } else {                
                    var html = [];
                    for (i = 0; i < elements.length; i++) {
                        html.push(elements[i].outerHTML);
                    }
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         html);
                }
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
                result = returnValue(msgJSON.tag, msgBody.fun[0], null);
                break;
            case "click":
                var element = resolveTarget(msgBody.elt[0], msgBody.css[0]);
                var event;
                if (bowser.phantom) {
                    event = document.createEvent( 'MouseEvents' );
                    event.initMouseEvent('click', true, true, window, 1, 0, 0);
                } else {
                    event = new MouseEvent('click');
                }
                element.dispatchEvent(event);
                result = returnValue(msgJSON.tag, msgBody.fun[0], null);
                break;
            default:
                throw new Error("Unsupported DOM request");
                break;
            }

            return result;
        }
        
        if (msgJSON.type[0] === "DEBUG") {
            debug = true;

        } else if (msgJSON.type[0] === "GETPAGE") {
	    var msg = JSON.stringify({ type: "PAGECONTENT",
                                       tag: msgJSON.tag[0],
                                       body: pageContent()
                                     });
            ws.send(msg);

        } else if (msgJSON.type[0] === "PREPARETODIE") {
	    var msg = JSON.stringify({ type: "DEAD",
                                       tag: msgJSON.tag[0],
                                       body: pageContent()
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
