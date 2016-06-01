
# R DOM API

# In javascript, we deal in pointers to javascript objects representing
# elements both within and outside of the DOM

# In R, we deal with HTML code for objects not yet in the DOM
# and CSS selectors to refer to objects within the DOM.
# When the javascript code returns a DOM object, we can ask for
# it to be HTML code OR a CSS selector.

# Tracking requests to execute callbacks on response
# Blocking requests until response received
DOMclosure <- function() {
    # request IDs
    id <- 0
    getID <- function() {
        id <<- id + 1
        as.character(id)
    }

    requests <- list()
    ## For debugging
    ls <- function() {
        requests
    }
    add <- function(tag, callback) {
        req <- requests[[tag]]
        if (!is.null(req)) {
            stop(paste0("Request ", tag, " already registered",
                        " (add failed)"))
        } else {
            requests[[tag]] <<- list(callback=callback,
                                     state="pending")
            cat("Adding request", tag, "\n")
        }
    }
    remove <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (remove failed)"))
        } else {
            requests[[tag]] <<- NULL
            cat("Removing request", tag, "\n")
        }
    }
    state <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (get state failed)"))
        } else {
            req$state
        }
    }
    callback <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (get callback failed)"))
        } else {
            req$callback
        }
    }
    handle <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (request handling failed)"))
        } else {
            requests[[tag]]$state <<- "handling"
            cat("Handling request", tag, "\n")
        }
    }
    setValue <- function(tag, value) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (setValue failed)"))
        } else {
            cat("Setting value for request", tag, "\n")
            requests[[tag]]$value <<- value
            requests[[tag]]$state <<- "complete"
        }
    }
    getValue <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (getValue failed)"))
        } else {
            requests[[tag]]$value
        }
    }
    pending <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (pending test failed)"))
        } else {
            requests[[tag]]$state != "complete"
        }
    }

    list(getID=getID,
         ls=ls,
         add=add,
         remove=remove,
         state=state,
         callback=callback,
         handle=handle,
         setValue=setValue,
         getValue=getValue,
         pending=pending)
}
DOMfunctions <- DOMclosure()

getRequestID <- DOMfunctions$getID
listRequests <- DOMfunctions$ls
addRequest <- DOMfunctions$add
removeRequest <- DOMfunctions$remove
getRequestState <- DOMfunctions$state
getRequestCallback <- DOMfunctions$callback
handleRequest <- DOMfunctions$handle
setRequestValue <- DOMfunctions$setValue
getRequestValue <- DOMfunctions$getValue
requestPending <- DOMfunctions$pending

# Handling messages
# RECEIVE either REQUEST or RESPONSE
handleMessage <- function(msgJSON) {
    # Assume content is character vector JSON
    msg <- fromJSON(msgJSON)
    if (msg$type == "ALIVE") {
        ## Page is waiting for this to know that browser has opened web socket
        setRequestValue(msg$tag, TRUE)
    } else if (msg$type == "DEAD") {
        ## Page is waiting for browser to die
        setRequestValue(msg$tag, msg$body)
    } else if (msg$type == "RESPONSE") {
        ## Get response value
        value <- msg$body$value
        ## Find the request that generated this response
        state <- getRequestState(msg$tag)
        if (state == "pending") {
            callback <- getRequestCallback(msg$tag)
            if (is.null(callback)) {
                ## Record value (someone will be waiting for it)
                setRequestValue(msg$tag, value)
            } else {
                ## Start handling request
                handleRequest(msg$tag)
                ## Run callback
                callback(value)
                ## Deregister request
                removeRequest(msg$tag)
            }
        } else {
            # we are already handling this message
            # (which can happen if a request callback includes a request)
            ;  
        }
    } else {
        stop("Cannot handle REQUESTs yet")
    }
}

## Block until response with 'tag' is received
## (if 'limit' is exceeded, error out)
waitForResponse <- function(tag, limit=5) {
    ptm <- proc.time()
    while (requestPending(tag)) {
        Sys.sleep(.1)
        if ((proc.time() - ptm)[3] > limit) {
            ## Give up on this request
            removeRequest(tag)
            stop("Exceeded wait time")
        }
    }
    value <- getRequestValue(tag)
    ## Deregister request
    removeRequest(tag)
    value
}

# SEND either REQUEST or RESPONSE
sendRequest <- function(pageID, msg, tag, callback) {
    sock <- pageInfo(pageID)$socket
    if (is.null(sock))
        stop("No socket open")
    msgJSON <- toJSON(msg)
    ## Register request (do it before send in case send returns instantly)
    addRequest(tag, callback)
    sock$send(msgJSON)
    ## EITHER have callback, in which case that will be called on receipt of
    ## response, OR block for response (and return response value)
    if (is.null(callback)) {
        return(waitForResponse(tag))
    } 
}

# 'node' should be a character value describing a valid HTML element
appendChild <- function(pageID, node, parent="body", css=TRUE,
                        callback=NULL, tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="appendChild", node=node, parent=parent, css=css))
    sendRequest(pageID, msg, tag, callback)
}

removeChild <- function(pageID, child, css=TRUE, 
                        callback=NULL, tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="removeChild", child=child, css=css))
    sendRequest(pageID, msg, tag, callback)
}

## For stopping a headless browser (PhantomJS)
kill <- function(pageID) {
    tag <- getRequestID()
    msg <- list(type="PREPARETODIE", tag=tag)
    result <- sendRequest(pageID, msg, tag, NULL)
    msg <- list(type="DIE")
    result
}
    
