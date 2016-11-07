
## Part of DOM API relating to CSS objects

styleSheets <- function(pageID, 
                        async=FALSE, callback=NULL, tag=getRequestID()) {
    responseType <- "DOM_StyleSheetList_ptr"
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="styleSheets",
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

################################################################################
insertRuleCore <- function(pageID, styleSheet, rule, index,
                           async, callback, tag) {
    checkDOMobj(styleSheet, pageID)
    if (length(styleSheet) == 0) {
        stop("No styleSheet to get property for")
    } else if (length(styleSheet) > 1) {
        warning("More than one styleSheet; only using first")
        styleSheet <- styleSheet[1]
    }
    responseType <- "DOM_number"
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="insertRule",
                          styleSheet=as.character(styleSheet),
                          styleSheetType=class(styleSheet),
                          rule=rule,
                          index=index,
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

setGeneric("insertRule",
           function(pageID, styleSheet, rule, index, ...) {
               standardGeneric("insertRule")
           },
           valueClass="DOM_value_OR_error")

setMethod("insertRule",
          signature(pageID="numeric",
                    styleSheet="DOM_CSSStyleSheet_ptr",
                    rule="character",
                    index="numeric"),
          function(pageID, styleSheet, rule, index=0,
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              insertRuleCore(pageID, styleSheet, rule, index,
                             async, callback, tag)
          })

################################################################################
deleteRuleCore <- function(pageID, styleSheet, index,
                           async, callback, tag) {
    checkDOMobj(styleSheet, pageID)
    if (length(styleSheet) == 0) {
        stop("No styleSheet to get property for")
    } else if (length(styleSheet) > 1) {
        warning("More than one styleSheet; only using first")
        styleSheet <- styleSheet[1]
    }
    responseType <- "NULL"
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="deleteRule",
                          styleSheet=as.character(styleSheet),
                          styleSheetType=class(styleSheet),
                          index=index,
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

setGeneric("deleteRule",
           function(pageID, styleSheet, index, ...) {
               standardGeneric("deleteRule")
           },
           valueClass="DOM_error_OR_NULL")

setMethod("deleteRule",
          signature(pageID="numeric",
                    styleSheet="DOM_CSSStyleSheet_ptr",
                    index="numeric"),
          function(pageID, styleSheet, index=0,
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              deleteRuleCore(pageID, styleSheet, index,
                             async, callback, tag)
          })

################################################################################
## Remove a property from a CSSStyleDeclaration
removePropertyCore <- function(pageID, style, propName, response,
                               async, callback, tag) {
    checkDOMobj(style, pageID)
    if (length(style) == 0) {
        stop("No style declaration to remove property from")
    } else if (length(style) > 1) {
        warning("More than one style declaration; only using first")
        style <- style[1]
    }
    responseType <- class(response)
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="removeProperty",
                          style=as.character(style),
                          styleType=class(style),
                          propName=propName,
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}

setGeneric("removeProperty",
           function(pageID, style, propName, ...) {
               standardGeneric("removeProperty")
           },
           valueClass="DOM_value_OR_error")

setMethod("removeProperty",
          signature(pageID="numeric",
                    style="DOM_CSSStyleDeclaration_ptr",
                    propName="character"),
          function(pageID, style, propName, response=NULL,
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              removePropertyCore(pageID, style, propName, response,
                                 async, callback, tag)
          })

################################################################################
## Get a list of the property names in a CSSStyleDeclaration
propertyNamesCore <- function(pageID, style, async, callback, tag) {
    checkDOMobj(style, pageID)
    if (length(style) == 0) {
        character()
    } else if (length(style) > 1) {
        warning("More than one style declaration; only using first")
        style <- style[1]
    }
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="propertyNames",
                          style=as.character(style),
                          styleType=class(style)))
    sendRequest(pageID, msg, tag, async, callback, "NULL")
}

setGeneric("propertyNames",
           function(pageID, style, ...) {
               standardGeneric("propertyNames")
           },
           valueClass="DOM_value_OR_error")

setMethod("propertyNames",
          signature(pageID="numeric",
                    style="DOM_CSSStyleDeclaration_ptr"),
          function(pageID, style,
                   async=FALSE, callback=NULL, tag=getRequestID()) {
              propertyNamesCore(pageID, style, async, callback, tag)
          })

