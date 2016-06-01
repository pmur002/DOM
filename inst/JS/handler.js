// A closure to capture the websocket object
messageHandler = function(ws) {
    return function (evt) {
        log = function(msg) {
            console.log("R package DOM: " + msg);
        }
        
        var response = handleMessage(evt);
        log("SENDING " + response);
        ws.send(response); 
    }
}
