
serverFunctionGenerator <- function() {
    # Server IDs
    id <- 0
    getID <- function() {
        id <<- id + 1
        id
    }

    # Server info
    servers <- list()
    registerHandle <- function(id, handle) {
        if (id <= length(servers) && !is.null(servers[[id]])) {
            stop(paste0("Server ", id, " handle already registered"))
        }
        servers[[id]] <<- list(handle=handle)
    }
    registerPort <- function(id, port) {
        if (!is.null(servers[[id]]$port)) {
            stop(paste0("Server ", id, " port already registered"))
        }
        servers[[id]]$port <<- port
    }
    registerSocket <- function(id, socket) {
        if (!is.null(servers[[id]]$socket)) {
            stop(paste0("Server ", id, " socket already registered"))
        }
        servers[[id]]$socket <<- socket
    }
    unregister <- function(id) {
        servers[[id]] <<- NULL
    }
    info <- function(id) {
        if (id > length(servers) || is.null(servers[[id]])) {
            stop(paste0("Server ", id, " not registered"))
        }
        servers[[id]]
    }
    inUse <- function(port) {
        port %in% sapply(servers, function(x) x$port)
    }        
    
    list(getID=getID,
         registerHandle=registerHandle,
         registerPort=registerPort,
         registerSocket=registerSocket,
         unregister=unregister,
         info=info,
         inUse=inUse)
}
serverFunctions <- serverFunctionGenerator()

getServerID <- serverFunctions$getID
registerServerHandle <- serverFunctions$registerHandle
registerServerPort <- serverFunctions$registerPort
registerServerSocket <- serverFunctions$registerSocket
unregisterServer <- serverFunctions$unregister
serverInfo <- serverFunctions$info
portInUse <- serverFunctions$inUse

# http://www.iana.org/assignments/service-names-port-numbers/service-names-port-numbers.xhtml
# "Dynamic and/or Private Ports (49152-65535)"
selectPort <- function() {
    sample(49152:65535, 1)
}

# If 'port' is NULL, randomly select a port
startServer <- function(serverID, app, port=NULL) {
    # Fail immediately if port is specified and is already in use by
    # an existing server
    if (!is.null(port) && portInUse(port)) {
        msg <- paste0("port ", port, " already in use")
        if (port == 52000) {
            msg <- paste0(msg, "; stop existing fileServer/urlServer.")
        }
        stop(msg)
    }
    serverStarted <- FALSE
    attempts <- 0
    handle <- NULL
    while (!serverStarted && attempts < 10) {
        while (is.null(port) || portInUse(port)) {
            port <- selectPort()
        }
        result <- try(startDaemonizedServer("0.0.0.0", port,
                                            app(serverID, port)),
                      silent=TRUE)
        attempts <- attempts + 1
        if (!inherits(result, "try-error")) {
            serverStarted <- TRUE
            handle <- result
        }
    }
    if (is.null(handle)) {
        stop("Failed to start server")
    }
    registerServerHandle(serverID, handle)
    registerServerPort(serverID, port)
    invisible()
}

stopServer <- function(serverID) {
    stopDaemonizedServer(serverInfo(serverID)$handle)
    unregisterServer(serverID)
}

# Browse http://localhost:port/, with 'app' (i.e., R) supplying the
# initial web page content
# (some example apps are distributed with the package)
# PLUS open web socket between R and browser
appServer <- function(app) {
    serverID <- getServerID()
    startServer(serverID, app)
    browseURL(paste0("http://localhost:", serverInfo(serverID)$port, "/"))
    serverID
}

# Browse file://localhost:port/<file> (i.e., 'file' supplies the
# initial web page content)
# PLUS open web socket between R and browser
# (requires greasemonkey AND RDOM.user.js user script installed on browser)
fileServer <- function(file) {
    serverID <- getServerID()
    # Allow for "file://" missing
    if (!grepl("^file://", file)) {
        file <- paste0("file://", file)
    }
    startServer(serverID, nullApp, 52000)
    browseURL(file)
    serverID
}

# Browser http://<url> (i.e., 'url' supplies the initial web page content)
# PLUS open web socket between R and browser
# (requires greasemonkey AND RDOM.user.js user script installed on browser)
urlServer <- function(url) {
    serverID <- getServerID()
    # Allow for "http://" missing
    if (!grepl("^http://", url)) {
        url <- paste0("http://", url)
    }
    startServer(serverID, nullApp, 52000)
    browseURL(url)
    serverID
}
