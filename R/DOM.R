
# R DOM API

# In javascript, we deal in pointers to javascript objects representing
# elements both within and outside of the DOM

# In R, we deal with HTML code for objects not yet in the DOM
# and CSS selectors to refer to objects within the DOM.
# When the javascript code returns a DOM object, we can ask for
# it to be HTML code OR a CSS selector.

dblog <- function(...) {
    if (getOption("DOM.debug")) 
        cat("-- RDOM R : ", ...)
}

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
    add <- function(tag, async, callback, returnType) {
        req <- requests[[tag]]
        if (!is.null(req)) {
            stop(paste0("Request ", tag, " already registered",
                        " (add failed)"))
        } else {
            requests[[tag]] <<- list(async=async,
                                     callback=callback,
                                     returnType=returnType,
                                     state="pending")
            dblog("Adding request", tag, "\n")
        }
    }
    remove <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (remove failed)"))
        } else {
            requests[[tag]] <<- NULL
            dblog("Removing request", tag, "\n")
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
    async <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (get async failed)"))
        } else {
            req$async
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
    type <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (get responseType failed)"))
        } else {
            req$returnType
        }        
    }
    setValue <- function(tag, value) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (setValue failed)"))
        } else {
            dblog("Setting value for request", tag, "\n")
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
         async=async,
         callback=callback,
         type=type,
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
getRequestAsync <- DOMfunctions$async
getRequestCallback <- DOMfunctions$callback
getRequestResponseType <- DOMfunctions$type
setRequestValue <- DOMfunctions$setValue
getRequestValue <- DOMfunctions$getValue
requestPending <- DOMfunctions$pending

DOMresponse <- function(x, type) {
    class(x) <- c(paste0(type, "_DOMresponse"), "DOMresponse")
    x
}

# Handling messages
# RECEIVE either REQUEST or RESPONSE
handleMessage <- function(msgJSON, ws) {
    # Assume content is character vector JSON
    msg <- fromJSON(msgJSON)
    if (msg$type == "ALIVE") {
        ## Page is waiting for this to know that browser has opened web socket
        setRequestValue(msg$tag, DOMresponse(TRUE, "ALIVE"))
    } else if (msg$type == "DEAD") {
        ## Page is waiting for browser to die
        setRequestValue(msg$tag, DOMresponse(msg$body, "DEAD"))
    } else if (msg$type == "ERROR") {
        ## A request has failed
        ## (so set the request value)
        ## (so any code waiting for this request will terminate)
        result <- paste0("Request ", msg$tag, " failed")
        ## Warning message will be the result of the request
        setRequestValue(msg$tag, DOMresponse(result, "ERROR"))
        ## Warning message will also print to screen
        message(result)
    } else if (msg$type == "RESPONSE") {
        ## Get response value
        value <- DOMresponse(msg$body$value, getRequestResponseType(msg$tag))
        ## When this is a response to a getElement* request, 'null'
        ## means no elements were found;  turn this into NA
        if (grepl("getElement", msg$body$fn)) {
            if (is.null(value)) {
                value <- DOMresponse(NA_character_,
                                     getRequestResponseType(msg$tag))
            }
        }
        ## Find the request that generated this response
        state <- getRequestState(msg$tag)
        if (state == "pending") {
            async <- getRequestAsync(msg$tag)
            if (!async) {
                ## Record response value (someone will be waiting for it)
                setRequestValue(msg$tag, value)
            } 
            callback <- getRequestCallback(msg$tag)
            if (!is.null(callback)) {
                ## Run callback
                callback(value)
            }
            if (async) {
                ## Deregister request
                removeRequest(msg$tag)
            }
        } else {
            stop(paste("Already handling response", msg$tag))
        }
    } else if (msg$type == "REQUEST") {
        dblog(capture.output(msg$body), sep="\n")
        result <- do.call(msg$body$fn,
                          list(msg$body$target, msg$body$targetRef))
        msg <- list(type="RESPONSE",
                    tag=msg$tag,
                    body=result)
        msgJSON <- toJSON(msg, null="null")
        ws$send(msgJSON)
    } else {
        stop("Unknown message type")
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
# IF 'callback' is non-NULL, the request is asynchronous and the callback
# will be called when a response with 'tag' is received
# IF 'callback' is NULL, the request is synchronous and R will block until
# a response with 'tag' is received AND the response value will be returned
sendRequest <- function(pageID, msg, tag, async, callback, returnType) {
    sock <- pageInfo(pageID)$socket
    if (is.null(sock))
        stop("No socket open")
    msgJSON <- toJSON(msg, null="null")
    ## Register request (do it before send in case send returns instantly)
    addRequest(tag, async, callback, returnType)
    sock$send(msgJSON)
    if (!async) {
        return(waitForResponse(tag))
    }
}

nodeSpec <- function(node, nodeRef) {
    if (!xor(is.null(node), is.null(nodeRef))) {
        stop("Specify exactly one of 'node' and 'nodeRef'")
    }
    if (is.null(node)) {
        node <- nodeRef
        byRef <- TRUE
    } else {
        byRef <- FALSE
    }
    list(node=node, byRef=byRef)
}

################################################################################
## The main API

appendChild <- function(pageID, child=NULL, childRef=NULL, 
                        parentRef="body", ns=NULL,
                        css=TRUE, async=!is.null(callback),
                        callback=NULL, tag=getRequestID()) {
    childSpec <- nodeSpec(child, childRef)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="appendChild",
                          child=childSpec$node, byRef=childSpec$byRef,
                          parent=parentRef, ns=ns, css=css, returnRef=FALSE))
    if (is.null(ns)) {
        responseType <- c("HTML", "XML")
    } else {
        responseType <- c(ns, "XML")
    }
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

appendChildCSS <- function(pageID, child=NULL, childRef=NULL, 
                           parentRef="body", ns=NULL,
                           css=TRUE, async=!is.null(callback),
                           callback=NULL, tag=getRequestID()) {
    childSpec <- nodeSpec(child, childRef)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="appendChild",
                          child=childSpec$node, byRef=childSpec$byRef,
                          parent=parentRef, ns=ns, css=css, returnRef=TRUE))
    sendRequest(pageID, msg, tag, async, callback, "CSS")
}

removeChild <- function(pageID, childRef, parentRef=NULL, css=TRUE, 
                        async=!is.null(callback), callback=NULL,
                        tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="removeChild", child=childRef, parent=parentRef,
                          css=css, returnRef=FALSE))
    sendRequest(pageID, msg, tag, async, callback, "XML")
}

removeChildCSS <- function(pageID, childRef, parentRef=NULL, css=TRUE, 
                           async=!is.null(callback), callback=NULL,
                           tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="removeChild", child=childRef, parent=parentRef,
                          css=css, returnRef=TRUE))
    sendRequest(pageID, msg, tag, async, callback, "CSS")
}

replaceChild <- function(pageID, newChild=NULL, newChildRef=NULL,
                         oldChildRef=NULL, parentRef=NULL, ns=NULL, css=TRUE, 
                         async=!is.null(callback), callback=NULL,
                         tag=getRequestID()) {
    newChildSpec <- nodeSpec(newChild, newChildRef)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="replaceChild",
                          newChild=newChildSpec$node, byRef=newChildSpec$byRef,
                          oldChild=oldChildRef, parent=parentRef,
                          ns=ns, css=css, returnRef=FALSE))
    if (is.null(ns)) {
        responseType <- c("HTML", "XML")
    } else {
        responseType <- c(ns, "XML")
    }
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

replaceChildCSS <- function(pageID, newChild=NULL, newChildRef=NULL,
                            oldChildRef=NULL, parentRef=NULL, ns=NULL, css=TRUE,
                            async=!is.null(callback), callback=NULL,
                            tag=getRequestID()) {
    newChildSpec <- nodeSpec(newChild, newChildRef)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="replaceChild",
                          child=newChildSpec$node, byRef=newChildSpec$byRef,
                          oldChild=oldChildRef, parent=parentRef,
                          ns=ns, css=css, returnRef=TRUE))
    sendRequest(pageID, msg, tag, async, callback, "CSS")
}

setAttribute <- function(pageID, eltRef, attrName, attrValue, css=TRUE,
                         async=!is.null(callback), callback=NULL,
                         tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="setAttribute", elt=eltRef,
                          attr=attrName, value=as.character(attrValue),
                          css=css))
    sendRequest(pageID, msg, tag, async, callback, "NULL")
}

getElementById <- function(pageID, id,
                           async=!is.null(callback),
                           callback=NULL, tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="getElementById", id=id, returnRef=FALSE))
    sendRequest(pageID, msg, tag, async, callback, "XML")
}

getElementByIdCSS <- function(pageID, id,
                              async=!is.null(callback),
                              callback=NULL, tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="getElementById", id=id, returnRef=TRUE))
    sendRequest(pageID, msg, tag, async, callback, "CSS")
}

getElementsByTagName <- function(pageID, name,
                                 async=!is.null(callback),
                                 callback=NULL, tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="getElementsByTagName", name=name,
                          returnRef=FALSE))
    sendRequest(pageID, msg, tag, async, callback, "XML")
}

getElementsByTagNameCSS <- function(pageID, name,
                                    async=!is.null(callback),
                                    callback=NULL, tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="getElementsByTagName", name=name,
                          returnRef=TRUE))
    sendRequest(pageID, msg, tag, async, callback, "CSS")
}

getElementsByClassName <- function(pageID, name, rootRef=NULL, css=TRUE,
                                   async=!is.null(callback),
                                   callback=NULL, tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="getElementsByClassName", name=name,
                          root=rootRef, css=css, returnRef=FALSE))
    sendRequest(pageID, msg, tag, async, callback, "XML")
}

getElementsByClassNameCSS <- function(pageID, name, rootRef=NULL, css=TRUE,
                                      async=!is.null(callback),
                                      callback=NULL, tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="getElementsByClassName", name=name,
                          root=rootRef, css=css, returnRef=TRUE))
    sendRequest(pageID, msg, tag, async, callback, "CSS")
}

appendScript <- function(pageID, script, 
                         parentRef="body", css=TRUE, async=!is.null(callback),
                         callback=NULL, tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="appendScript",
                          script=script,
                          parent=parentRef, css=css, returnRef=FALSE))
    sendRequest(pageID, msg, tag, async, callback, "NULL")
}

## This request is ALWAYS asynchronous
## Mostly for headless browser testing (?)
click <- function(pageID, eltRef, css=TRUE,
                  callback=NULL, tag=getRequestID()) {
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="click", elt=eltRef, css=css))
    sendRequest(pageID, msg, tag, TRUE, callback, "NULL")
}

################################################################################
## Some extra utilities

## For stopping a headless browser (PhantomJS)
kill <- function(pageID) {
    tag <- getRequestID()
    msg <- list(type="PREPARETODIE", tag=tag)
    result <- sendRequest(pageID, msg, tag, FALSE, NULL, "NULL")
    msg <- list(type="DIE")
    # Do not wait for browser response because browser will die before
    # it sends a response
    sendRequest(pageID, msg, getRequestID(), TRUE, NULL, "NULL")
    result
}
    
## Deliberately internal function, so have to call with DOM:::debug()
debug <- function(pageID) {
    options(DOM.debug=TRUE)
    msg <- list(type="DEBUG")
    sendRequest(pageID, msg, getRequestID(), TRUE, NULL, "NULL")
}
