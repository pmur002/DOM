handleMessage = function(ws, msg) {

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
    
    var msgJSON = JSON.parse(msg.data);
    if (msgJSON.type[0] === "REQUEST") {
        var msgBody = msgJSON.body;
        switch(msgBody.fun[0]) {
        case "appendChild": // parent, child, css
            var container = document.createElement("div");
            container.innerHTML = msgBody.node[0];
            var parent = resolveTarget(msgBody.parent[0], msgBody.css[0]);
            log("ADDING " + container.firstChild.toString() + 
                " TO " + parent.toString());
            parent.appendChild(container.firstChild);
            var response = returnValue(msgJSON.tag, msgBody.fun, msgBody.node);
            ws.send(JSON.stringify(response)); 
            break;
        case "replaceChild": // newchild, oldchild, css
            var container = document.createElement("div");
            container.innerHTML = msgBody.newnode[0];
            var oldnode = resolveTarget(msgBody.oldnode[0], msgBody.css[0]);
            var parent = oldnode.parentElement;
            log("REPLACING " + oldnode.toString() + 
                " WITH " + container.firstChild.toString());
            parent.replaceChild(container.firstChild, oldnode);
            var response = returnValue(msgJSON.tag, 
                                       msgBody.fun, oldnode.outerHTML);
            ws.send(JSON.stringify(response)); 
            break;
        case "removeChild": // child, css
            var child = resolveTarget(msgBody.child[0], msgBody.css[0]);
            var parent = child.parentElement;
            log("REMOVING " + child.toString() + " FROM " + parent.toString());
            parent.removeChild(child);
            var response = returnValue(msgJSON.tag, 
                                       msgBody.fun, child.outerHTML);
            ws.send(JSON.stringify(response)); 
            break;
        default:
            throw new Error("Unsupported DOM request");
            break;
        }
    } else {
        throw new Error("Cannot handle RESPONSEs yet");
    }
}

