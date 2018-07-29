
pageClosure <- function() {
    # Page IDs
    id <- 0
    getID <- function() {
        id <<- id + 1
        id
    }

    # Page info
    pages <- list()
    register <- function(id, handle, port, client) {
        if (id <= length(pages) && !is.null(pages[[id]])) {
            stop(paste0("Page ", id, " already registered"))
        }
        pages[[id]] <<- list(handle=handle,
                             port=port,
                             client=client)
    }
    registerSocket <- function(id, socket) {
        if (!is.null(pages[[id]]$socket)) {
            stop(paste0("Page ", id, " socket already registered"))
        }
        pages[[id]]$socket <<- socket
    }
    unregister <- function(id) {
        pages[[id]] <<- NULL
    }
    info <- function(id) {
        if (!is.numeric(id) || id > length(pages) || is.null(pages[[id]])) {
            stop(paste0("Page ", id, " not registered"))
        }
        pages[[id]]
    }
    inUse <- function(port) {
        port %in% sapply(pages, function(x) x$port)
    }
    kill <- function(id) {
        ## Kill PhantomJS
        ## nothing to be done for GUI browsers;
        ## do not have permission to close tabs or windows)
        client <- pages[[id]]$client
        if (!is.null(client$kill)) {
            client$kill(id)
        } else {
            NULL
        }
    }
    
    list(getID=getID,
         register=register,
         registerSocket=registerSocket,
         unregister=unregister,
         info=info,
         inUse=inUse,
         kill=kill)
}
pageFunctions <- pageClosure()

getPageID <- pageFunctions$getID
registerPage <- pageFunctions$register
registerPageSocket <- pageFunctions$registerSocket
unregisterPage <- pageFunctions$unregister
pageInfo <- pageFunctions$info
portInUse <- pageFunctions$inUse
killPage <- pageFunctions$kill

# http://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml
# "Dynamic and/or Private Ports (49152-65535)"
selectPort <- function() {
    sample(49152:65535, 1)
}

killClient <- function(client, pageID) {
}

# If 'port' is NULL, randomly select a port
startServer <- function(pageID, host, app, client,
                        port=NULL, body="", tag=NULL) {
    # Fail immediately if port is specified and is already in use by
    # an existing page
    if (!is.null(port) && portInUse(port)) {
        msg <- paste0("port ", port, " already in use")
        if (port == 52000) {
            msg <- paste0(msg, "; close existing filePage/urlPage.")
        }
        removeRequest(tag)
        stop(msg)
    }
    pageStarted <- FALSE
    attempts <- 0
    handle <- NULL
    while (!pageStarted && attempts < 10) {
        while (is.null(port) || portInUse(port)) {
            port <- selectPort()
        }
        result <-
            try(startDaemonizedServer("0.0.0.0", port,
                                      client$app(pageID, host, port,
                                                 body, tag)),
                silent=TRUE)
        attempts <- attempts + 1
        if (!inherits(result, "try-error")) {
            pageStarted <- TRUE
            handle <- result
        }
    }
    if (is.null(handle)) {
        removeRequest(tag)
        stop("Failed to start page")
    }
    registerPage(pageID, handle, port, client)
    invisible()
}

# Browse http://localhost:port/, with 'html' (character vector)
# supplying the <body> of the initial web page content
# (default is a blank page)
# PLUS open web socket between R and browser
htmlPage <- function(html="", host="127.0.0.1",
                     client=getOption("DOM.client")) {
    pageID <- getPageID()
    ## Register a request so can wait for a response from browser
    tag <- getRequestID()
    addRequest(tag, FALSE, NULL, "NULL", pageID)
    ## Start R server to handle web socket activity
    ## (and possibly serve initial HTML)
    startServer(pageID, host, client$app, client, body=html, tag=tag)
    port <- pageInfo(pageID)$port
    ## Use 127.0.0.1 rather than 'localhost' to keep PhantomJS happy (?)
    client$run(paste0("http://", host, ":", port, "/"), host, port, tag=tag)
    ## Block until web socket has been established by browser
    waitForResponse(tag, onTimeout=function() closePage(pageID))
    ## Register pageID with browser
    tag <- getRequestID()
    msg <- list(type="PAGEID", tag=tag, body=pageID)
    sendRequest(pageID, msg, tag, FALSE, NULL, "NULL")
    pageID
}

# Browse file://localhost:port/<file> (i.e., 'file' supplies the
# initial web page content)
# PLUS open web socket between R and browser
# (requires greasemonkey AND RDOM.user.js user script installed on browser)
filePage <- function(file, client=getOption("DOM.client")) {
    pageID <- getPageID()
    # Allow for "file://" missing
    if (!grepl("^file://", file)) {
        file <- paste0("file://", file)
    }
    addRequest("-1", FALSE, NULL, "NULL", pageID)
    startServer(pageID, "127.0.0.1", nullApp, client, 52000, tag="-1")
    client$run(file, "127.0.0.1", 52000, tag="-1")
    waitForResponse("-1", onTimeout=function() closePage(pageID))
    ## Register pageID with browser
    tag <- getRequestID()
    msg <- list(type="PAGEID", tag=tag, body=pageID)
    sendRequest(pageID, msg, tag, FALSE, NULL, "NULL")
    pageID
}

# Browser http://<url> (i.e., 'url' supplies the initial web page content)
# PLUS open web socket between R and browser
# (requires greasemonkey AND RDOM.user.js user script installed on browser)
urlPage <- function(url, client=getOption("DOM.client")) {
    pageID <- getPageID()
    # Allow for "http://" missing
    if (!grepl("^http://", url)) {
        url <- paste0("http://", url)
    }
    addRequest("-1", FALSE, NULL, "NULL", pageID)
    startServer(pageID, "127.0.0.1", nullApp, client, 52000, tag="-1")
    client$run(url, "127.0.0.1", 52000, tag="-1")
    waitForResponse("-1", onTimeout=function() closePage(pageID))
    ## Register pageID with browser
    tag <- getRequestID()
    msg <- list(type="PAGEID", tag=tag, body=pageID)
    sendRequest(pageID, msg, tag, FALSE, NULL, "NULL")
    pageID
}

closePage <- function(pageID) {
    pageContent <- killPage(pageID)
    stopDaemonizedServer(pageInfo(pageID)$handle)
    unregisterPage(pageID)
    invisible(pageContent)
}

