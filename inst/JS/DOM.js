
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

handleMessage = function(msg) {
    var msgJSON = JSON.parse(msg.data);
    switch(msgJSON.fun[0]) {
    case "replaceChild": // newchild, oldchild, css
        var container = document.createElement("div");
        container.innerHTML = msgJSON.newnode[0];
        var oldnode = resolveTarget(msgJSON.oldnode[0], msgJSON.css[0]);
        var parent = oldnode.parentElement;
        log("REPLACING " + oldnode.toString() + 
            " WITH " + container.firstChild.toString());
        parent.replaceChild(container.firstChild, oldnode);
        break;
    case "appendChild": // parent, child, css
        var container = document.createElement("div");
        container.innerHTML = msgJSON.node[0];
        var parent = resolveTarget(msgJSON.parent[0], msgJSON.css[0]);
        log("ADDING " + container.firstChild.toString() + 
            " TO " + parent.toString());
        parent.appendChild(container.firstChild);
        break;
    case "removeChild": // child, css
        var child = resolveTarget(msgJSON.child[0], msgJSON.css[0]);
        var parent = child.parentElement;
        log("REMOVING " + child.toString() + " FROM " + parent.toString());
        parent.removeChild(child);
        break;
    default:
        throw new Error("Unsupported DOM operation");
        break;
    }
}

