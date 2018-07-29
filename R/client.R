
## A client is a function that will attempt to connect to a URL:port

makeClient <- function(run, kill=NULL, ws=TRUE) {
    if (ws) {
        app <- wsApp
    } else {
        app <- nullApp
    }
    client <- list(app=app, run=run, kill=kill)
    class(client) <- "DOMclient"
    client
}

## Some clients are defined in zzz.R
