
pageClosure <- function() {
    # Page IDs
    id <- 0
    getID <- function() {
        id <<- id + 1
        id
    }

    # Page info
    pages <- list()
    register <- function(id, handle, port, headless) {
        if (id <= length(pages) && !is.null(pages[[id]])) {
            stop(paste0("Page ", id, " already registered"))
        }
        pages[[id]] <<- list(handle=handle,
                             port=port,
                             headless=headless)
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
        if (id > length(pages) || is.null(pages[[id]])) {
            stop(paste0("Page ", id, " not registered"))
        }
        pages[[id]]
    }
    inUse <- function(port) {
        port %in% sapply(pages, function(x) x$port)
    }        
    
    list(getID=getID,
         register=register,
         registerSocket=registerSocket,
         unregister=unregister,
         info=info,
         inUse=inUse)
}
pageFunctions <- pageClosure()

getPageID <- pageFunctions$getID
registerPage <- pageFunctions$register
registerPageSocket <- pageFunctions$registerSocket
unregisterPage <- pageFunctions$unregister
pageInfo <- pageFunctions$info
portInUse <- pageFunctions$inUse

# http://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml
# "Dynamic and/or Private Ports (49152-65535)"
selectPort <- function() {
    sample(49152:65535, 1)
}

# Run a browser from R (so that it can create a websocket connecting to R)
runBrowser <- function(url, port=NULL, headless=FALSE, tag=NULL) {
    if (headless) {
        phantomURL(url, port, tag)
    } else {
        browseURL(url)
    }
}

killBrowser <- function(pageID) {
    # Kill PhantomJS
    # (nothing to be done for GUI browsers;
    #  do not have permission to close tabs or windows)
    if (pageInfo(pageID)$headless) {
        kill(pageID)
    }
}

# If 'port' is NULL, randomly select a port
startServer <- function(pageID, app, port=NULL, body="",
                        tag=NULL, headless=FALSE) {
    # Fail immediately if port is specified and is already in use by
    # an existing page
    if (!is.null(port) && portInUse(port)) {
        msg <- paste0("port ", port, " already in use")
        if (port == 52000) {
            msg <- paste0(msg, "; close existing filePage/urlPage.")
        }
        stop(msg)
    }
    pageStarted <- FALSE
    attempts <- 0
    handle <- NULL
    while (!pageStarted && attempts < 10) {
        while (is.null(port) || portInUse(port)) {
            port <- selectPort()
        }
        result <- try(startDaemonizedServer("0.0.0.0", port,
                                            app(pageID, port, body, tag)),
                      silent=TRUE)
        attempts <- attempts + 1
        if (!inherits(result, "try-error")) {
            pageStarted <- TRUE
            handle <- result
        }
    }
    if (is.null(handle)) {
        stop("Failed to start page")
    }
    registerPage(pageID, handle, port, headless)
    invisible()
}

# Browse http://localhost:port/, with 'html' (character vector)
# supplying the <body> of the initial web page content
# (default is a blank page)
# PLUS open web socket between R and browser
htmlPage <- function(html="", headless=FALSE) {
    pageID <- getPageID()
    if (headless) {
        app <- nullApp
    } else {
        app <- wsApp
    }
    ## Register a request so can wait for a response from browser
    tag <- getRequestID()
    addRequest(tag, NULL)
    ## Start R server to handle web socket activity
    ## (and possibly serve initial HTML)
    startServer(pageID, app, body=html, tag=tag, headless=headless)
    port <- pageInfo(pageID)$port
    ## Use 127.0.0.1 rather than 'localhost' to keep PhantomJS happy (?)
    runBrowser(paste0("http://127.0.0.1:", port, "/"),
               port, headless, tag=tag)
    ## Block until web socket has been established by browser
    waitForResponse(tag)
    pageID
}

# Browse file://localhost:port/<file> (i.e., 'file' supplies the
# initial web page content)
# PLUS open web socket between R and browser
# (requires greasemonkey AND RDOM.user.js user script installed on browser)
filePage <- function(file, headless=FALSE) {
    pageID <- getPageID()
    # Allow for "file://" missing
    if (!grepl("^file://", file)) {
        file <- paste0("file://", file)
    }
    addRequest("-1", NULL)
    startServer(pageID, nullApp, 52000, tag="-1", headless=headless)
    runBrowser(file, 52000, headless, tag="-1")
    waitForResponse("-1")
    pageID
}

# Browser http://<url> (i.e., 'url' supplies the initial web page content)
# PLUS open web socket between R and browser
# (requires greasemonkey AND RDOM.user.js user script installed on browser)
urlPage <- function(url, headless=FALSE) {
    pageID <- getPageID()
    # Allow for "http://" missing
    if (!grepl("^http://", url)) {
        url <- paste0("http://", url)
    }
    addRequest("-1", NULL)
    startServer(pageID, nullApp, 52000, tag="-1", headless=headless)
    runBrowser(url, 52000, headless, tag="-1")
    waitForResponse("-1")
    pageID
}

closePage <- function(pageID) {
    pageContent <- killBrowser(pageID)
    stopDaemonizedServer(pageInfo(pageID)$handle)
    unregisterPage(pageID)
    invisible(pageContent)
}

