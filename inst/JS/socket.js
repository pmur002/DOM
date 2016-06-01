initSocket = function(ws, tag) {
    log = function(msg) {
        console.log("R package DOM: " + msg);
    }
    
    ws.onopen = function() {
        ws.send(JSON.stringify({ type: "ALIVE", tag: tag }));
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
        var response = handleMessage(evt);
        ws.send(response); 
    }
}
