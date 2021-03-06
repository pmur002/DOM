
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
    add <- function(tag, async, callback, returnType, pageID) {
        req <- requests[[tag]]
        if (!is.null(req)) {
            stop(paste0("Request ", tag, " already registered",
                        " (add failed)"))
        } else {
            requests[[tag]] <<- list(async=async,
                                     callback=callback,
                                     returnType=returnType,
                                     pageID=pageID,
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
    pageID <- function(tag) {
        req <- requests[[tag]]
        if (is.null(req)) {
            stop(paste0("Request ", tag, " not registered",
                        " (get responseType failed)"))
        } else {
            req$pageID
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
         pageID=pageID,
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
getRequestPageID <- DOMfunctions$pageID
setRequestValue <- DOMfunctions$setValue
getRequestValue <- DOMfunctions$getValue
requestPending <- DOMfunctions$pending

DOMresponse <- function(x, type, pageID) {
    # When a request is expected to return a DOM node, but returns null
    # (e.g., getElementById() finds no match)
    if (is.null(x) && type != "NULL")
        x <- character()
    switch(type,
           # Requests that have no return value
           "NULL"=NULL,
           # Requests that return a JSON object
           JSON=x,
           # Requests that return a DOM node of some sort
           DOM_node_HTML=,
           DOM_node_SVG=new(type, as.character(x)),
           DOM_node_CSS=,
           DOM_node_XPath=,
           DOM_node_ptr=,
           DOM_CSSStyleSheet_ptr=,
           DOM_CSSRule_ptr=,
           DOM_CSSStyleDeclaration_ptr=,
           DOM_obj_ptr=new(type, as.character(x), pageID=pageID),
           # Requests that return a basic value
           DOM_number=as.numeric(x),
           DOM_string=as.character(x),
           DOM_boolean=as.logical(x))
}

# Handling messages
# RECEIVE either REQUEST or RESPONSE
handleMessage <- function(msgJSON, ws) {
    # Assume content is character vector JSON
    msg <- fromJSON(msgJSON)
    if (msg$type == "ALIVE") {
        ## Page is waiting for this to know that browser has opened web socket
        setRequestValue(msg$tag, TRUE)
    } else if (msg$type == "DEAD") {
        ## Page is waiting for browser to die
        setRequestValue(msg$tag, msg$body)
    } else if (msg$type == "RENDERED") {
        ## Page is waiting for browser to render
        setRequestValue(msg$tag, TRUE)
    } else if (msg$type == "PAGECONTENT") {
        setRequestValue(msg$tag, msg$body)
    } else if (msg$type == "ERROR") {
        ## A request has failed
        ## (so set the request value)
        ## (so any code waiting for this request will terminate)
        result <- paste0("Request ", msg$tag, " failed: ", msg$body)
        ## Warning message will be the result of the request
        setRequestValue(msg$tag, new("DOM_error", result))
        ## Warning message will also print to screen
        message(result)
    } else if (msg$type == "RESPONSE") {
        ## Get response value
        ## If request response type differs from message body type 
        ## then the browser has determined the type of response, so use that
        if (getRequestResponseType(msg$tag) != msg$body$type) {
            responseType <- msg$body$type
        } else {
            responseType <- getRequestResponseType(msg$tag)
        }
        value <- DOMresponse(msg$body$value, responseType,
                             getRequestPageID(msg$tag))
        ## When this is a response to a getElement* request, 'null'
        ## means no elements were found;  turn this into character(0)
        if (grepl("getElement", msg$body$fn)) {
            if (is.null(value)) {
                value <- DOMresponse(character(),
                                     responseType,
                                     getRequestPageID(msg$tag))
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
        args <- mapply(DOMresponse, msg$body$args, msg$body$argsType,
                       MoreArgs=list(pageID=msg$pageID),
                       SIMPLIFY=FALSE, USE.NAMES=FALSE)
        callback <- getRegisteredCallback(msg$body$fn)
        result <- do.call(callback, c(args, list(pageID=msg$pageID)))
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
waitForResponse <- function(tag, limit=getOption("DOM.limit"), onTimeout=NULL) {
    ptm <- proc.time()
    while (requestPending(tag)) {
        ## Sys.sleep(.1)
        ## Make sure that requests are service while we are waiting
        service(100)
        if ((proc.time() - ptm)[3] > limit) {
            ## Give up on this request
            removeRequest(tag)
            if (!is.null(onTimeout))
                onTimeout()
            stop("Exceeded wait time")
        }
    }
    value <- getRequestValue(tag)
    ## Deregister request
    removeRequest(tag)
    value
}

# SEND either REQUEST or RESPONSE
# IF 'async' is TRUE, the request is asynchronous and the callback
# will be called when a response with 'tag' is received
# IF 'async' is FALSE, the request is synchronous and R will block until
# a response with 'tag' is received AND the response value will be returned
sendRequest <- function(pageID, msg, tag, async, callback, returnType) {
    sock <- pageInfo(pageID)$socket
    if (is.null(sock))
        stop("No socket open")
    msgJSON <- toJSON(msg, null="null")
    ## Register request (do it before send in case send returns instantly)
    addRequest(tag, async, callback, returnType, pageID)
    sock$send(msgJSON)
    if (!async) {
        return(waitForResponse(tag))
    }
}

# Function to check validity of DOM objects passed to API functions
# Default is to do nothing
setGeneric("checkDOMobj",
           function(object, ...) {})

# For _ptr objects, check that the pageID matches
setMethod("checkDOMobj",
          signature(object="DOM_obj_ptr"),
          function(object, pageID, ...) {
              if (object@pageID != pageID) {
                  stop("Invalid object pointer")
              }
          })
        

################################################################################
## The main API

createElement <- function(pageID, tagName, response=nodePtr(),
                          async=FALSE, callback=NULL, tag=getRequestID()) {
    responseType <- class(response)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="createElement",
                          tagName=as.character(tagName),
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

createElementNS <- function(pageID, namespace, tagName, response=nodePtr(),
                            async=FALSE, callback=NULL, tag=getRequestID()) {
    responseType <- class(response)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="createElementNS",
                          namespace=as.character(namespace),
                          tagName=as.character(tagName),
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

appendChildCore <- function(pageID, child, parent, response,
                            ns, async, callback, tag) {
    checkDOMobj(child, pageID)
    checkDOMobj(parent, pageID)
    responseType <- class(response)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="appendChild",
                          child=as.character(child),
                          childType=class(child),
                          parent=as.character(parent),
                          parentType=class(parent),
                          ns=ns,
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

setGeneric("appendChild",
           function(pageID, child, parent, ...) {
               standardGeneric("appendChild")
           },
           valueClass="DOM_node_OR_error_OR_NULL")

setMethod("appendChild",
          signature(pageID="numeric",
                    child="DOM_node",
                    parent="missing"),
          function(pageID, child, parent, response=htmlNode(),
                   ns=FALSE, async=FALSE, callback=NULL, tag=getRequestID()) {
              appendChildCore(pageID, child, parent=css("body"), response,
                              ns, async, callback, tag)
          })
               
setMethod("appendChild",
          signature(pageID="numeric",
                    child="DOM_node",
                    parent="DOM_node_ref"),
          function(pageID, child, parent, response=htmlNode(),
                   ns=FALSE, async=FALSE, callback=NULL, tag=getRequestID()) {
              appendChildCore(pageID, child, parent, response,
                              ns, async, callback, tag)
          })

removeChildCore <- function(pageID, child, parent, response,
                            async, callback, tag) {
    checkDOMobj(child, pageID)
    checkDOMobj(parent, pageID)
    responseType <- class(response)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="removeChild",
                          child=as.character(child),
                          childType=class(child),
                          parent=as.character(parent),
                          parentType=class(parent),
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

setGeneric("removeChild",
           function(pageID, child, parent, ...) {
               standardGeneric("removeChild")
           },
           valueClass="DOM_node_OR_error_OR_NULL")

setMethod("removeChild",
          signature(pageID="numeric",
                    child="DOM_node_ref",
                    parent="missing"),
          function(pageID, child, parent, response=htmlNode(),
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              removeChildCore(pageID, child, parent=NULL, response,
                              async, callback, tag)
          })
               
setMethod("removeChild",
          signature(pageID="numeric",
                    child="DOM_node_ref",
                    parent="DOM_node_ref"),
          function(pageID, child, parent, response=htmlNode(),
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              removeChildCore(pageID, child, parent, response,
                              async, callback, tag)
          })
               

replaceChildCore <- function(pageID, newChild, oldChild, parent, response,
                             ns, async, callback, tag) {
    checkDOMobj(newChild, pageID)
    checkDOMobj(oldChild, pageID)
    checkDOMobj(parent, pageID)
    responseType <- class(response)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="replaceChild",
                          newChild=as.character(newChild),
                          newChildType=class(newChild),
                          oldChild=as.character(oldChild),
                          oldChildType=class(oldChild),
                          parent=as.character(parent),
                          parentType=class(parent),
                          ns=ns,
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

setGeneric("replaceChild",
           function(pageID, newChild, oldChild, parent, ...) {
               standardGeneric("replaceChild")
           },
           valueClass="DOM_node_OR_error_OR_NULL")

setMethod("replaceChild",
          signature(pageID="numeric",
                    newChild="DOM_node",
                    oldChild="DOM_node_ref",
                    parent="missing"),
          function(pageID, newChild, oldChild, parent, response=htmlNode(),
                   ns=FALSE, async=FALSE, callback=NULL, tag=getRequestID()) {
              replaceChildCore(pageID, newChild, oldChild, parent=NULL,
                               response, ns, async, callback, tag)
          })
               
setMethod("replaceChild",
          signature(pageID="numeric",
                    newChild="DOM_node",
                    oldChild="DOM_node_ref",
                    parent="DOM_node_ref"),
          function(pageID, newChild, oldChild, parent, response=htmlNode(),
                   ns=FALSE, async=FALSE, callback=NULL, tag=getRequestID()) {
              replaceChildCore(pageID, newChild, oldChild, parent,
                               response, ns, async, callback, tag)
          })

getAttributeCore <- function(pageID, elt, attrName, response,
                             async, callback, tag) {
    checkDOMobj(elt, pageID)
    if (length(elt) == 0) {
        stop("No elt to get property for")
    } else if (length(elt) > 1) {
        warning("More than one elt; only using first")
        elt <- elt[1]
    }
    responseType <- class(response)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="getAttribute",
                          elt=as.character(elt),
                          eltType=class(elt),
                          attrName=attrName,
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

setGeneric("getAttribute",
           function(pageID, elt, attrName, ...) {
               standardGeneric("getAttribute")
           },
           valueClass="DOM_value_OR_error_OR_NULL")

setMethod("getAttribute",
          signature(pageID="numeric",
                    elt="DOM_node_ref",
                    attrName="character"),
          function(pageID, elt, attrName, response=NULL,
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              getAttributeCore(pageID, elt, attrName, response,
                               async, callback, tag)
          })

setAttributeCore <- function(pageID, elt, attrName, attrValue,
                             async, callback, tag) {
    checkDOMobj(elt, pageID)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="setAttribute",
                          elt=as.character(elt),
                          eltType=class(elt),
                          attrName=attrName,
                          attrValue=attrValue))
    sendRequest(pageID, msg, tag, async, callback, "NULL")
}

setGeneric("setAttribute",
           function(pageID, elt, attrName, attrValue, ...) {
               standardGeneric("setAttribute")
           },
           valueClass="NULL")

setMethod("setAttribute",
          signature(pageID="numeric",
                    elt="DOM_node_ref",
                    attrName="character",
                    attrValue="character"),
          function(pageID, elt, attrName, attrValue,
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              setAttributeCore(pageID, elt, attrName, attrValue,
                               async, callback, tag)
          })

getElementByIdCore <- function(pageID, id, response,
                               async, callback, tag) {
    responseType <- class(response)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="getElementById",
                          id=id,
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

setGeneric("getElementById",
           function(pageID, id, ...) {
               standardGeneric("getElementById")
           },
           valueClass="DOM_node_OR_error_OR_NULL")

setMethod("getElementById",
          signature(pageID="numeric",
                    id="character"),
          function(pageID, id, response=htmlNode(),
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              getElementByIdCore(pageID, id, response,
                                 async, callback, tag)
          })

getElementsByTagNameCore <- function(pageID, name, response,
                                     async, callback, tag) {
    responseType <- class(response)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="getElementsByTagName",
                          name=name,
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

setGeneric("getElementsByTagName",
           function(pageID, name, ...) {
               standardGeneric("getElementsByTagName")
           },
           valueClass="DOM_node_OR_error_OR_NULL")

setMethod("getElementsByTagName",
          signature(pageID="numeric",
                    name="character"),
          function(pageID, name, response=htmlNode(),
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              getElementsByTagNameCore(pageID, name, response,
                                       async, callback, tag)
          })

getElementsByClassNameCore <- function(pageID, name, root, response,
                                       async, callback, tag) {
    responseType <- class(response)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="getElementsByClassName",
                          name=name,
                          root=as.character(root),
                          rootType=class(root),
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

setGeneric("getElementsByClassName",
           function(pageID, name, root, ...) {
               standardGeneric("getElementsByClassName")
           },
           valueClass="DOM_node_OR_error_OR_NULL")

setMethod("getElementsByClassName",
          signature(pageID="numeric",
                    name="character",
                    root="missing"),
          function(pageID, name, root, response=htmlNode(),
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              getElementsByClassNameCore(pageID, name, root=NULL, response,
                                         async, callback, tag)
          })

setMethod("getElementsByClassName",
          signature(pageID="numeric",
                    name="character",
                    root="DOM_node_ref"),
          function(pageID, name, root, response=htmlNode(),
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              getElementsByClassNameCore(pageID, name, root, response,
                                         async, callback, tag)
          })

## See property.R for getProperty() and setProperty()

## This request is ALWAYS asynchronous
## Mostly for headless browser testing (?)
setGeneric("click",
           function(pageID, elt, ...) {
               standardGeneric("click")
           },
           valueClass="NULL")

setMethod("click",
          signature(pageID="numeric",
                    elt="DOM_node_ref"),
          function(pageID, elt, 
                   callback=NULL, tag=getRequestID()) {
              checkDOMobj(elt, pageID)
              msg <- list(type="REQUEST", tag=tag,
                          body=list(fun="click",
                                    elt=as.character(elt),
                                    eltType=class(elt)))
              sendRequest(pageID, msg, tag, TRUE, callback, "NULL")
          })

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

## Deliberately internal functions, so have to call with, e.g., DOM:::debug()
render <- function(pageID, outfile) {
    tag <- getRequestID()
    msg <- list(type="RENDER", tag=tag, outfile=outfile)
    sendRequest(pageID, msg, tag, FALSE, NULL, "NULL")
}

getPage <- function(pageID) {
    tag <- getRequestID()
    msg <- list(type="GETPAGE", tag=tag)
    sendRequest(pageID, msg, tag, FALSE, NULL, "NULL")
}

debug <- function(pageID) {
    options(DOM.debug=TRUE)
    msg <- list(type="DEBUG")
    sendRequest(pageID, msg, getRequestID(), TRUE, NULL, "NULL")
}
