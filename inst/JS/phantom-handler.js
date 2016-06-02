messageHandler = function(ws) {
    return function (evt) {
        var msgJSON = JSON.parse(evt.data);
        if (msgJSON.type[0] === "PREPARETODIE") {
            var msg = JSON.stringify({ type: "DEAD",
                                       tag: msgJSON.tag[0],
                                       body: page.content
                                     });
            ws.send(msg);
        } else if (msgJSON.type[0] === "DIE") {
            phantom.exit();
        } else {
            var response = page.evaluate(handleMessage, evt);
            log("SENDING " + response);
            ws.send(response); 
        }
    }
}
