
# NOTE that this is a FUNCTION that generates an app
# (so that the app is contains closures that know which page they belong to)

# App that serves web page with given 'body'
# (but does NOT create web socket; that is expected to be handled by browser)
nullApp <- function(pageID, port, body) {
    list(
        call = function(req) {
            list(status = 200L,
                 headers = list(
                     'Content-Type' = 'text/html'
                 ),
                 body = body)
        },
        onWSOpen = function(ws) {
            registerPageSocket(pageID, ws)
            ws$onMessage(function(binary, msg) {
                if (binary)
                    stop("Unable to handle binary message")
                handleMessage(msg)
            })
        }
    )
}

# App that serves web page with given 'body'
# AND creates web socket (back to R) on load
wsApp <- function(pageID, port, body) {
    DOMjs <- readLines(system.file("JS", "DOM.js", package="DOM"))
    socketjs <- readLines(system.file("JS", "socket.js", package="DOM"))
    html <- paste(
        '<html>',
        '<head>',
        '<script>',
        paste(DOMjs, collapse="\n"),
        paste(socketjs, collapse="\n"),
        '</script>',
        '<script>',
        paste0('ws = new WebSocket("ws://localhost:', port, '");'),
        'initSocket(ws);',
        '</script>',
        '</head>',
        '<body>',
        paste(body, collapse="\n"),
        '</body>',
        '</html>',
        collapse="\n")
    list(
        call = function(req) {
            list(status = 200L,
                 headers = list(
                     'Content-Type' = 'text/html'
                 ),
                 body = html)
        },
        onWSOpen = function(ws) {
            registerPageSocket(pageID, ws)
            ws$onMessage(function(binary, msg) {
                if (binary)
                    stop("Unable to handle binary message")
                handleMessage(msg)
            })
        }
    )
}
