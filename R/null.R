
# NOTE that this is a FUNCTION that generates an app
# (so that the app is contains closures that know which page they belong to)

# App that returns empty-string body
# This is NOT designed to serve HTTP requests
# It is ONLY for establishing web socket connection with browser
nullApp <- function(pageID, port, body) {
    file <- system.file("JS", "DOM.js", package="DOM")
    html <- paste(
        '<html>',
        '<head>',
        '<script>',
        paste(readLines(file), collapse="\n"),
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
        }
    )
}
