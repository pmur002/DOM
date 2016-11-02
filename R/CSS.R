
## Part of DOM API relating to CSS objects

styleSheets <- function(pageID, 
                        async=FALSE, callback=NULL, tag=getRequestID()) {
    responseType <- "DOM_StyleSheetList_ptr"
    msg <- list(type="REQUEST", tag=tag,
                body=list(fun="styleSheets",
                          responseType=responseType))
    sendRequest(pageID, msg, tag, async, callback, responseType)
}
