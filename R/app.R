
## Variant of Rook Builder class that satisfies 'httpuv' app
## (by having an 'onWSOpen' method)
wsBuilder <- setRefClass(
    'wsBuilder',
    contains = 'Builder',
    fields = 'init',
    methods = list(
        onWSOpen = function(ws) {
            init$onWSOpen(ws)
        },
	initialize = function(...){
	    objs <- list(...)
            init <<- objs[[length(objs)]]
	    callSuper(...)
	}
    )
)

# NOTE that this is a FUNCTION that generates an app
# (so that the app is contains closures that know which page they belong to)

# App that serves web page with given 'body'
# (but does NOT create web socket; that is expected to be handled by browser)
# 'port' and 'tag' are not used because they are provided by the client
# (e.g., they are hard-coded in RDOM.user.js)
nullApp <- function(pageID,
                    url="http://127.0.0.1", port=52000, body, head, tag="-1") {
    template <- readLines(system.file("templates", "app.html", package="DOM"))
    html <- whisker.render(template,
                           list(body=paste(body, collapse="\n"),
                                head=paste(head, collapse="\n")))
    init <- list(
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
                handleMessage(msg, ws)
            })
        }
    )
    wsBuilder$new(Static$new(urls="/assets", root=getwd()),
                  init)
}

# App that serves web page with given 'body'
# AND creates web socket (back to R) on load
wsApp <- function(pageID, host, port, body, head, tag) {
    template <- readLines(system.file("templates", "wsApp.html", package="DOM"))
    pkgVersion <- packageVersion("DOM")
    html <- whisker.render(template,
                           list(pkgVersion=as.character(pkgVersion),
                                host=host, port=port, tag=tag,
                                body=paste(body, collapse="\n"),
                                head=paste(head, collapse="\n")))
    init <- list(
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
                handleMessage(msg, ws)
            })
        })
    wsBuilder$new(Static$new(urls="/assets", root=getwd()),
                  init)
}
