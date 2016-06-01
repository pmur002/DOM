
# NOTE that this is a FUNCTION that generates an app
# (so that the app is contains closures that know which page they belong to)

# App that serves web page with given 'body'
# (but does NOT create web socket; that is expected to be handled by browser)
# 'port' and 'tag' are not used because they are hard-coded in RDOM.user.js
nullApp <- function(pageID, port=52000, body, tag="-1") {
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
wsApp <- function(pageID, port, body, tag) {
    template <- readLines(system.file("templates", "app.html", package="DOM"))
    html <- whisker.render(template,
                           list(port=port, tag=tag,
                                body=paste(body, collapse="\n")))
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
