
// A closure to capture the websocket object
requestGenerator = function(ws) {

    log = function(msg) {
        console.log("R package DOM: " + msg);
    }
    
    requestValue = function(fn, element, selector, tag) {
        return { type: "REQUEST",
                 tag: tag,
                 body: {
                     fn: fn,
                     target: element.outerHTML,
                     targetRef: selector
                 }    
               }
    }
    
    var CSG = new CssSelectorGenerator();

    // 'fn' is name of R function (string)
    // 'args' is JSON object
    return function(fn, element, callback) {
        var tag = getRequestID();
        addRequest(tag, callback);
        var selector = CSG.getSelector(element);
        var request = requestValue(fn, element, selector, tag);
        var msgJSON = JSON.stringify(request);
        log("SENDING " + msgJSON);
        ws.send(msgJSON); 
    }
}

    
    
