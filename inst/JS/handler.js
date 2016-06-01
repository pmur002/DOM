messageHandler = function(evt) {
    var response = handleMessage(evt);
    ws.send(response); 
}
