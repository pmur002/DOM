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

    // The page number that R knows this connection by
    var pageID;

    var parser = new DOMParser();
    var serializer = new XMLSerializer;

    // CSS selector generator object
    var CSG = new CssSelectorGenerator();

    // Generate XPath for element
    // http://stackoverflow.com/questions/2661818/javascript-get-xpath-of-a-node
    function createXPathFromElement(elm) { 
        var allNodes = document.getElementsByTagName('*'); 
        for (var segs = []; elm && elm.nodeType == 1; elm = elm.parentNode) 
        { 
            if (elm.hasAttribute('id')) { 
                var uniqueIdCount = 0; 
                for (var n=0;n < allNodes.length;n++) { 
                    if (allNodes[n].hasAttribute('id') && 
                        allNodes[n].id == elm.id) 
                        uniqueIdCount++; 
                    if (uniqueIdCount > 1) 
                        break; 
                }; 
                if ( uniqueIdCount == 1) { 
                    segs.unshift('id("' + elm.getAttribute('id') + '")'); 
                    return segs.join('/'); 
                } else { 
                    segs.unshift(elm.localName.toLowerCase() + 
                                 '[@id="' + elm.getAttribute('id') + '"]'); 
                } 
            } else if (elm.hasAttribute('class')) { 
                segs.unshift(elm.localName.toLowerCase() + 
                             '[@class="' + elm.getAttribute('class') + '"]'); 
            } else { 
                for (i = 1, sib = elm.previousSibling; sib; 
                     sib = sib.previousSibling) { 
                    if (sib.localName == elm.localName)  i++; }; 
                segs.unshift(elm.localName.toLowerCase() + '[' + i + ']'); 
            }; 
        }; 
        return segs.length ? '/' + segs.join('/') : null; 
    }; 

    // Utils for tracking DOM_node_ptr objects
    var DOMnodes = [];
    var DOMindex = 0;
    var setDOMnode = function(node) {
        // Check that this node has not already been recorded
        var index = -1;
        var i = 0;
        while (index < 0 && i < DOMnodes.length) {
            if (node === DOMnodes[i]) {
                index = i;
            }
            i++;
        }
        // If the node is new, record it
        if (index < 0) {
            DOMnodes[DOMindex] = node;
            index = DOMindex++;
        }
        return index;
    }
    var getDOMnode = function(index) {
        return DOMnodes[index];
    }

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
    var requestValue = function(tag, fn, args, argsType, pageID) {
        return { type: "REQUEST",
                 tag: tag,
                 body: {
                     fn: fn,
                     args: args,
                     argsType: argsType
                 },
                 pageID: pageID
               }
    }
    var returnValue = function(tag, fn, value, type) {
        return { type: "RESPONSE",
                 tag: tag,
                 body: {
                     fn: fn,
                     value: value,
                     type: type
                 }
               }    
    }   
    var errorValue = function(tag, err) {
        return { type: "ERROR",
                 tag: tag,
                 body: err.message
               }    
    }
    var pageContent = function() {
        return document.documentElement.outerHTML
    }

    // http://stackoverflow.com/questions/8511281/check-if-a-value-is-an-object-in-javascript
    function isObject(obj) {
        return obj === Object(obj);
    }

    var DOMnode = function(spec, type, ns) {
        var node;
        var container;
        switch (type) {
        case "DOM_node_HTML":
            if (ns) {
                // "text/xml" rather than "text/html" 
                // to satisfy PhantomJS
                container = parser.parseFromString(spec, "text/xml");
            } else {
                container = document.createElement("div");
                container.innerHTML = spec;
            }        
            node = container.firstChild;
            break;
        case "DOM_node_SVG":
            container = parser.parseFromString(spec, "image/svg+xml");
            node = container.firstChild;
            break;
        case "DOM_node_JS":
            var script = document.createElement("script");
            script.innerHTML = spec;
            node = script;
            break;
        case "DOM_node_CSS":
            node = resolveTarget(spec, true);
            break;
        case "DOM_node_XPath":
            node = resolveTarget(spec, false);
            break;
        case "DOM_obj_ptr":
        case "DOM_node_ptr":
        case "DOM_CSSStyleSheet_ptr":
        case "DOM_CSSRule_ptr":
            node = getDOMnode(spec);
            break;
        // Variations on DOM_value
        case "number":
            node = Number(spec);
            break;
        case "character":
            node = String(spec);
            break;
        case "logical":
            node = Boolean(spec);
            break;
        }
        return node;
    }

    // Try to determine a 'DOM' package S4 class for a response value
    var DOMclasses = [ "StyleSheetList", "CSSStyleSheet", 
                       "CSSRuleList", "CSSRule" ];

    var DOMresponseType = function(object) {
        var responseType;
        switch (typeof object) {
        case "number":
            responseType = "DOM_number";
            break;
        case "string":
            responseType = "DOM_string";
            break;
        case "boolean":
            responseType = "DOM_boolean";
            break;
        case "object":
            // NOTE that PhantomJS does not support object.constructor.name
            // Fallback
            responseType = "DOM_obj_ptr";
            var i = 0;
            while (i < DOMclasses.length && responseType === "DOM_obj_ptr") {
                var className = DOMclasses[i];
                if (object instanceof window[className]) {
                    responseType = "DOM_" + className + "_ptr";
                }
                i++;
            }
            break;
        default:
            responseType = "DOM_obj_ptr";
        }        
        return responseType;
    }

    // Returns both a response value AND a response type
    // (and the latter may differ from the responseType passed in)
    // (the latter is used in getProperty() [at least] to consolidate
    //  "array-of-class" and "class" to just "class")
    var DOMresponse = function(node, responseType, ns) {
        var i;
        var result = [];
        switch (responseType) {
        case "DOM_node_HTML":
        case "DOM_node_SVG":
            // Treat single nodes and collections of nodes the same way
            // NodeList to accommodate phantomJS
            if (!(node.constructor.name === "HTMLCollection" ||
                  node instanceof NodeList)) {
                node = [ node ];
            }
            if (ns) {
                for (i = 0; i < node.length; i++) {
                    result.push(serializer.serializeToString(node[i]));
                }
            } else {
                for (i = 0; i < node.length; i++) {
                    result.push(node[i].outerHTML);
                }               
            }     
            break;
        case "DOM_node_CSS":
            if (!(node.constructor.name === "HTMLCollection" ||
                  node instanceof NodeList)) {
                node = [ node ];
            }
            for (i = 0; i < node.length; i++) {
                if (node[i].parentNode === null) {
                    result.push(null);
                } else {
                    result.push(CSG.getSelector(node[i]));
                }
            }
            break;
        case "DOM_node_XPath":
            if (!(node.constructor.name === "HTMLCollection" ||
                  node instanceof NodeList)) {
                node = [ node ];
            }
            for (i = 0; i < node.length; i++) {
                if (node[i].parentNode === null) {
                    result.push(null);
                } else {
                    result.push(createXPathFromElement(node[i]));
                }
            }
            break;
        case "DOM_obj_ptr":
        case "DOM_node_ptr":
            if (!(node.constructor.name === "HTMLCollection" ||
                  node instanceof NodeList)) {
                node = [ node ];
            }
            for (i = 0; i < node.length; i++) {
                result.push(setDOMnode(node[i]));
            }
            break;
        case "DOM_CSSStyleSheet_ptr":
        case "DOM_CSSRule_ptr":
            result.push(setDOMnode(node));
            break;
        case "DOM_StyleSheetList_ptr":
            for (i = 0; i < node.length; i++) {
                result.push(setDOMnode(node[i]));
            }
            responseType = "DOM_CSSStyleSheet_ptr";
            break;
        case "DOM_CSSRuleList_ptr":
            for (i = 0; i < node.length; i++) {
                result.push(setDOMnode(node[i]));
            }
            responseType = "DOM_CSSRule_ptr";
            break;
        case "DOM_number":
        case "DOM_string":
        case "DOM_boolean":
            if (typeof node != responseType.substr(4)) {
                throw new Error("Invalid DOM response");
            }
            result.push(node);
            break;
        }
        return { response: result, responseType: responseType };
    }
    
    // Main function for handling requests from R to JS
    var handleMessage = function(msg) {
        dblog("RECEIVING " + msg.data);
        var msgJSON = JSON.parse(msg.data);
        
        handleRequest = function() {
            var result = "";
            var msgBody = msgJSON.body;
            switch(msgBody.fun[0]) {
            case "createElement":
                var node = document.createElement(msgBody.tagName[0]);
                result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                     DOMresponse(node, 
                                                 msgBody.responseType[0], 
                                                 false).response,
                                     msgBody.responseType[0]);
                break;
            case "createElementNS":
                var node = document.createElementNS(msgBody.namespace[0],
                                                    msgBody.tagName[0]);
                node.setAttribute("xmlns", msgBody.namespace[0]);
                result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                     DOMresponse(node, 
                                                 msgBody.responseType[0], 
                                                 true).response,
                                     msgBody.responseType[0]);
                break;
            case "appendChild": // parent, child
                var child = DOMnode(msgBody.child[0], msgBody.childType[0],
                                    msgBody.ns[0]);
                var parent;
                if (msgBody.parentType[0] === "NULL") {
                    parent = child.parentElement;
                } else {
                    parent = DOMnode(msgBody.parent[0], msgBody.parentType[0],
                                     false);
                }
                
                dblog("ADDING " + child.toString() + 
                      " TO " + parent.toString());
                parent.appendChild(child);
                
                result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                     DOMresponse(child, 
                                                 msgBody.responseType[0], 
                                                 msgBody.ns[0]).response,
                                     msgBody.responseType[0]);
                break;
            case "removeChild": // child, parent
                var error = false;
                var child = DOMnode(msgBody.child[0], msgBody.childType[0],
                                    false);
                var parent;
                if (msgBody.parentType[0] === "NULL") {
                    parent = child.parentElement;
                } else {
                    parent = DOMnode(msgBody.parent[0], msgBody.parentType[0],
                                     false);
                }

                result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                     DOMresponse(child, 
                                                 msgBody.responseType[0], 
                                                 false).response,
                                     msgBody.responseType[0]);

                // Remove child AFTER determining its CSS selector !
                dblog("REMOVING " + child.toString() + 
                      " FROM " + parent.toString());
                parent.removeChild(child);
                break;
            case "replaceChild": // newchild, oldchild, parent
                var newChild = DOMnode(msgBody.newChild[0], 
                                       msgBody.newChildType[0],
                                       msgBody.ns[0]);
                var oldChild = DOMnode(msgBody.oldChild[0], 
                                       msgBody.oldChildType[0],
                                       false);
                var parent;
                if (msgBody.parentType[0] === "NULL") {
                    parent = oldChild.parentElement;
                } else {
                    parent = DOMnode(msgBody.parent[0], msgBody.parentType[0],
                                     false);
                }

                result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                     DOMresponse(oldChild,
                                                 msgBody.responseType[0], 
                                                 msgBody.ns[0]).response,
                                     msgBody.responseType[0]);
                
                // Replace oldChild AFTER determining its CSS selector !
                dblog("REPLACING " + oldChild.toString() + 
                      " WITH " + newChild.toString());
                parent.replaceChild(newChild, oldChild);
                break;
            case "setAttribute": // elt, attr, value
                var element = DOMnode(msgBody.elt[0], msgBody.eltType[0], 
                                      false);
                element.setAttribute(msgBody.attrName[0], msgBody.attrValue[0]);
                result = returnValue(msgJSON.tag, msgBody.fun[0], null, "NULL");
                break;
            case "getElementById": // id
                var element = document.getElementById(msgBody.id[0]);
                if (element === null) {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         null, msgBody.responseType[0]);
                } else {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         DOMresponse(element,
                                                     msgBody.responseType[0], 
                                                     false).response,
                                         msgBody.responseType[0]);
                }
                break;
            case "getElementsByTagName": // name ('*' is special)
                var elements = document.getElementsByTagName(msgBody.name[0]);
                if (elements.length === 0) {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         null, msgBody.responseType[0]);
                } else {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         DOMresponse(elements,
                                                     msgBody.responseType[0], 
                                                     false).response,
                                         msgBody.responseType[0]);
                }
                break;
            case "getElementsByClassName": // name, root
                var elements;
                // root not specified
                if (msgBody.rootType[0] === "NULL") {
                    elements = document.getElementsByClassName(msgBody.name[0]);
                } else {
                    var parent = DOMnode(msgBody.root[0], msgBody.rootType[0],
                                         false);
                    elements = parent.getElementsByClassName(msgBody.name[0]);
                }
                if (elements.length === 0) {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         null, msgBody.responseType[0]);
                } else {
                    result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                         DOMresponse(elements,
                                                     msgBody.responseType[0], 
                                                     false).response,
                                         msgBody.responseType[0]);
                }
                break;
            case "getProperty":
                var element = DOMnode(msgBody.object[0], msgBody.objectType[0],
                                      false);
                var name = msgBody.propName[0];
                var value = element[name];
                // May have asked for property that does not exist
                if (typeof value === "undefined") {
                    throw new Error("Property " + name + " does not exist");
                }
                var responseType = msgBody.responseType[0];
                if (responseType === "NULL") {
                    responseType = DOMresponseType(value);
                } 
                var response = DOMresponse(value, responseType, false);
                result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                     response.response,
                                     response.responseType);
                break;
            case "setProperty":
                var element = DOMnode(msgBody.object[0], msgBody.objectType[0],
                                      false);
                var name = msgBody.propName[0];
                var value = DOMnode(msgBody.value[0], msgBody.valueType[0]);
                element[name] = value;
                result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                     null, "NULL");
                break;
            case "styleSheets":
                // Incoming responseType is DOM_StyleSheetList_ptr
                // Outgoing responseType is DOM_CSSStyleSheet_ptr
                var value = document.styleSheets;
                var responseType = msgBody.responseType[0];
                var response = DOMresponse(value, responseType, false);
                result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                     response.response,
                                     response.responseType);
                break;
            case "insertRule":
                var styleSheet = DOMnode(msgBody.styleSheet[0], 
                                         msgBody.styleSheetType[0],
                                         false);
                var index = styleSheet.insertRule(msgBody.rule[0], 
                                                  msgBody.index[0]);
                result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                     DOMresponse(index,
                                                 msgBody.responseType[0], 
                                                 false).response,
                                     msgBody.responseType[0]);
                break;
            case "deleteRule":
                var styleSheet = DOMnode(msgBody.styleSheet[0], 
                                         msgBody.styleSheetType[0],
                                         false);
                styleSheet.deleteRule(msgBody.index[0]);
                result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                     null, "NULL");
                break;
            case "click":
                var element = DOMnode(msgBody.elt[0], msgBody.eltType[0],
                                      false);
                var event;
                if (bowser.phantom) {
                    event = document.createEvent( 'MouseEvents' );
                    event.initMouseEvent('click', true, true, window, 1, 0, 0);
                } else {
                    event = new MouseEvent('click');
                }
                element.dispatchEvent(event);
                result = returnValue(msgJSON.tag, msgBody.fun[0], 
                                     null, "NULL");
                break;
            default:
                throw new Error("Unsupported DOM request");
                break;
            }

            return result;
        }
        
        if (msgJSON.type[0] === "DEBUG") {
            debug = true;
            
        } else if (msgJSON.type[0] === "PAGEID") {
            pageID = msgJSON.body[0];
	    var msg = JSON.stringify({ type: "ALIVE",
                                       tag: msgJSON.tag[0]
                                     });
            ws.send(msg);

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
    // 'target' is a DOM node or an array of DOM nodes
    // 'targetType' is an array containing one or more of:
    //     "HTML", "SVG", "CSS", "XPath"
    // 'callback' is a JS function
    Rcall = function(fn, target, targetType, callback) {
        var i;
        var tag = getRequestID();
        addRequest(tag, callback);
        // Turn target into array even if length 1
        if (typeof target.length === "undefined") {
            target = [ target ];
        }
        var args = [];
        var argsType = [];
        if (targetType != null) {
            for (i = 0; i < target.length; i++) {
                for (j = 0; j < targetType.length; j++) {
                    args.push(DOMresponse(target[i], 
                                          'DOM_node_' + targetType[j],
                                          false).response[0]);
                    argsType.push('DOM_node_' + targetType[j]);
                }
            }
        }
        var request = requestValue(tag, fn, args, argsType, pageID);
        var msgJSON = JSON.stringify(request);
        dblog("SENDING " + msgJSON);
        ws.send(msgJSON); 
    }

    return {
	init: init,
	Rcall: Rcall
    }
}());
