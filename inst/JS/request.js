
// A closure to capture the websocket object
requestGenerator = function(ws) {

    requestValue = function(fn, args) {
        return { type: "REQUEST",
                 body: {
                     fn: fn,
                     args: args
                 }    
               }
    }
    
    // 'fn' is name of R function (string)
    // 'args' is JSON object
    return function(fn, args) {
        var request = requestValue(fn, args);
        ws.send(JSON.stringify(request)); 
    }
}

    
    
