
# NOTE that this is a FUNCTION that generates an app
# (so that the app is contains closures that know which server they belong to)

# App that creates blank web page
# with javascript code to respond to DOM requests
blankApp <- function(serverID, port) {
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
            registerServerSocket(serverID, ws)
        }
    )
}
