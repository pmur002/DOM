
// A closure to capture the websocket object
requestGenerator = function(ws) {

    log = function(msg) {
        console.log("R package DOM: " + msg);
    }
    
    requestValue = function(fn, args, tag) {
        return { type: "REQUEST",
                 tag: tag,
                 body: {
                     fn: fn,
                     args: args
                 }    
               }
    }
    
    // 'fn' is name of R function (string)
    // 'args' is JSON object
    return function(fn, args, callback) {
        var tag = getRequestID();
        addRequest(tag, callback);
        var request = requestValue(fn, args, tag);
        var msgJSON = JSON.stringify(request);
        log("SENDING " + msgJSON);
        ws.send(msgJSON); 
    }
}

    
    
