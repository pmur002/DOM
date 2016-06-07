// handleMessage() must ONLY accept JSON string and ONLY return JSON string
// so that PhantomJS can call page.evaluate(handleMessage)
// (it can only accept and receive simple objects)
handleMessage = function(msg) {

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

    returnValue = function(tag, fun, value) {
        return { type: "RESPONSE",
                 tag: tag,
                 body: { 
                     fun: fun,
                     value: value
                 }
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
    var result = "";

    var CSG = new CssSelectorGenerator();

    handleMsg = function() {
        if (msgJSON.type[0] === "REQUEST") {
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
                    result = returnValue(msgJSON.tag, msgBody.fun, 
                                         selector);
                } else {
                    result = returnValue(msgJSON.tag, msgBody.fun, 
                                         child.outerHTML);
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
                    result = returnValue(msgJSON.tag, msgBody.fun, 
                                         selector);
                } else {
                    result = returnValue(msgJSON.tag, msgBody.fun, 
                                         child.outerHTML);
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
                result = returnValue(msgJSON.tag, 
                                     msgBody.fun, oldnode.outerHTML);
                break;
            default:
                throw new Error("Unsupported DOM request");
                break;
            }
        } else {
            throw new Error("Cannot handle RESPONSEs yet");
        }
        
        return result;
    }
    
    try {
        result = handleMsg();
    } catch(err) {
        result = errorValue(msgJSON.tag, err);
    }

    log("RETURNING " + JSON.stringify(result));
    return JSON.stringify(result);
}

