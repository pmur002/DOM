
## Part of the DOM API concerned with getting and setting properties

################################################################################
## getProperty()
getPropertyCore <- function(pageID, object, propName, response,
                        async, callback, tag) {
    checkDOMobj(object, pageID)
    responseType <- class(response)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="getProperty",
                          object=as.character(object),
                          objectType=class(object),                          
                          propName=propName,
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

setGeneric("getProperty",
           function(pageID, object, propName, ...) {
               standardGeneric("getProperty")
           },
           valueClass="DOM_obj_response_OR_error_OR_NULL")

setMethod("getProperty",
          signature(pageID="numeric",
                    object="DOM_obj_ref",
                    propName="character"),
          function(pageID, object, propName, response=NULL,
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              getPropertyCore(pageID, object, propName, response,
                              async, callback, tag)
          })

################################################################################
## Determine whether a property can be used in setProperty()
## Cannot set some properties (e.g., style property on an element)
## https://developer.mozilla.org/en-US/docs/Web/API/CSS_Object_Model/Using_dynamic_styling_information
setGeneric("readonlyProperty",
           function(object, propName) {
               standardGeneric("readonlyProperty")
           })

## Sort of default, catch-all
## If you end up here then DOM basically knows nothing about your
## object so it allows you to have a go at any property you like
setMethod("readonlyProperty",
          signature(object="DOM_obj_ref",
                    propName="character"),
          function(object, propName) {
              FALSE
          })
          
setMethod("readonlyProperty",
          signature(object="DOM_node_ref",
                    propName="character"),
          function(object, propName) {
              propName %in% "style"
          })

################################################################################
## setProperty()
setPropertyCore <- function(pageID, object, propName, value, warn,
                            async, callback, tag) {
    checkDOMobj(object, pageID)
    checkDOMobj(value, pageID)
    if (readonlyProperty(object, propName)) {
        if (warn) {
            warning(paste0("Read-only property '", propName,
                           "' not set"))
        }
        invisible()
    } else {
        msg <- list(type="REQUEST", tag=tag,
                    body=list(fun="setProperty",
                              object=as.character(object),
                              objectType=class(object),
                              propName=propName,
                              value=as.character(value),
                              valueType=class(value)))
        sendRequest(pageID, msg, tag, async, callback, "NULL")
    }
}

setGeneric("setProperty",
           function(pageID, object, propName, value, ...) {
               standardGeneric("setProperty")
           },
           valueClass="NULL")

# In general, the value of the property should be an
# existing DOM object (because it can be a complex object)
setMethod("setProperty",
          signature(pageID="numeric",
                    object="DOM_obj_ref",
                    propName="character",
                    value="DOM_obj_ref"),
          function(pageID, object, propName, value, warn=TRUE,
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              setPropertyCore(pageID, object, propName, value, warn,
                              async, callback, tag)
          })

# Also allow for simple values (numbers, strings, booleans)
setMethod("setProperty",
          signature(pageID="numeric",
                    object="DOM_obj_ref",
                    propName="character",
                    value="DOM_value"),
          function(pageID, object, propName, value, warn=TRUE,
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              setPropertyCore(pageID, object, propName, value, warn,
                              async, callback, tag)
          })

################################################################################
## Some convenient syntactic sugar
setMethod("$",
          signature(x="DOM_obj_ref"),
          function(x, name) {
              getProperty(x@pageID, x, name)
          })

setMethod("$<-",
          signature(x="DOM_obj_ref"),
          function(x, name, value) {
              ## If the property is readonly, silently fail
              ## (silently do nothing because you can set specific style
              ##  properties and we want elt$style$color = "blue" to work)
              setProperty(x@pageID, x, name, value, warn=FALSE)
              ## The main purpose of this call is for its side-effect
              ## but return the object being modified because that
              ## retains the pointer to the real browser object
              x
          })

