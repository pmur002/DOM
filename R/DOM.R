
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
            requests[[tag]] <<- list(callback=callback)
        }
    }
    remove <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (remove failed)"))
        } else {
            requests[[tag]] <<- NULL
        }
    }
    fetch <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (fetch failed)"))
        } else {
            req
        }
    }
    setValue <- function(tag, value) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (setValue failed)"))
        } else {
            requests[[tag]]$value <<- value
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
        is.null(requests[[tag]]$value)
    }

    list(getID=getID,
         ls=ls,
         add=add,
         remove=remove,
         fetch=fetch,
         setValue=setValue,
         getValue=getValue,
         pending=pending)
}
DOMfunctions <- DOMclosure()

getRequestID <- DOMfunctions$getID
listRequests <- DOMfunctions$ls
addRequest <- DOMfunctions$add
removeRequest <- DOMfunctions$remove
fetchRequest <- DOMfunctions$fetch
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
    } else if (msg$type == "RESPONSE") {
        ## Get response value
        value <- msg$body$value
        ## Find the request that generated this response
        request <- fetchRequest(msg$tag)
        if (is.null(request$callback)) {
            ## Record value (someone will be waiting for it)
            setRequestValue(msg$tag, value)
        } else {
            ## Deregister request
            removeRequest(msg$tag)
            ## Run callback
            request$callback(value)
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
    msg <- list(type="DIE")
    sendRequest(pageID, msg, getRequestID(), callback=function() {})
}
    
