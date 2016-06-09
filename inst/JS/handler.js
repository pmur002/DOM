// A closure to capture the websocket object
messageHandler = function(ws) {
    return function (evt) {
        log = function(msg) {
            console.log("R package DOM: " + msg);
        }
        
        var response = handleMessage(evt);
        if (response === null) {
            ; // Handled a RESPONSE not a REQUEST
        } else {
            log("SENDING " + response);
            ws.send(response); 
        }
    }
}
