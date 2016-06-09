// handleMessage() must ONLY accept JSON string and ONLY return JSON string
// so that PhantomJS can call page.evaluate(handleMessage)
// (it can only accept and receive simple objects)
// A closure to encapsulate request info
handleMessage = function() {

    var id = 0;
    getRequestID = function() {
        var result = id;
        id = id + 1;
        return result;
    }

    var requests = [];
    addRequest = function(id, callback) {
        requests[id] = { callback: callback };
    }
    removeRequest = function(id) {
        requests[id] = null;
    }
    getRequest = function(id) {
        return requests[id];
    }

    return function(msg) {

        // Make log() and resolveTarget() part of handleMessage() closure
        // so that PhantomJS page.evaluate(handleMessage) can see them
        // (should have no negative consequences for other browsers?)

        log = function(msg) {
            console.log("R package DOM: " + msg);
        }
        
        resolveTarget = function(target, css) {
            if (css) {
                return document.querySelector(target);
            } else {
                return document.evaluate(target, document, 
                                         null, XPathResult.ANY_TYPE, null);
            }
        }

        returnValue = function(tag, value) {
            return { type: "RESPONSE",
                     tag: tag,
                     body: value
                   }    
        }
        
        errorValue = function(tag, err) {
            return { type: "ERROR",
                     tag: tag,
                     body: err
                   }    
        }
        
        log("RECEIVING " + msg.data);
        var msgJSON = JSON.parse(msg.data);

        var CSG = new CssSelectorGenerator();

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
                log("ADDING " + child.toString() + 
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
                log("REMOVING " + child.toString() + 
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
                var container = document.createElement("div");
                container.innerHTML = msgBody.newnode[0];
                var oldnode = resolveTarget(msgBody.oldnode[0], msgBody.css[0]);
                var parent = oldnode.parentElement;
                log("REPLACING " + oldnode.toString() + 
                    " WITH " + container.firstChild.toString());
                parent.replaceChild(container.firstChild, oldnode);
                result = returnValue(msgJSON.tag, oldnode.outerHTML);
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
                log("ADDING " + script.toString() + 
                    " TO " + parent.toString());
                parent.appendChild(script);
                result = returnValue(msgJSON.tag, "");
                break;
            default:
                throw new Error("Unsupported DOM request");
                break;
            }

            return result;
        }
        

        if (msgJSON.type[0] === "REQUEST") {
            var result = "";
            try {
                result = handleRequest();
            } catch(err) {
                result = errorValue(msgJSON.tag, err);
            }

            log("RETURNING " + JSON.stringify(result));
            return JSON.stringify(result);

        } else if (msgJSON.type[0] === "RESPONSE") {
            // Match response tag to request tag and eval callback (if any)
            var request = getRequest(msgJSON.tag[0]);
            if (request.callback != null) {
                request.callback(msgJSON.body);
            }
            return null;

        } else {
            throw new Error("Unknown message type");
        }
    }
}();

