initSocket = function(ws) {
    log = function(msg) {
        console.log("R package DOM: " + msg);
    }
    
    ws.onopen = function() {
        log("Connection opened");
    }
    ws.onerror = function(evt) { 
        msg = "An error occurred with the WebSocket. " +
            "Has the R server been started?";
        log(msg);
    };
    ws.onclose = function(evt) {
        log("Connection closed");
    }
    ws.onmessage = function(evt) {
        handleMessage(ws, evt);
    }
}
