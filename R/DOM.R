
# R DOM API

# In javascript, we deal in pointers to javascript objects representing
# elements both within and outside of the DOM

# In R, we deal with HTML code for objects not yet in the DOM
# and CSS selectors to refer to objects within the DOM.
# When the javascript code returns a DOM object, we can ask for
# it to be HTML code OR a CSS selector.

# 'node' should be a character value describing a valid HTML element
appendChild <- function(pageID, node, parent="body", css=TRUE) {
    sock <- pageInfo(pageID)$socket
    if (is.null(sock))
        stop("No socket open")
    msg <- list(fun="appendChild", node=node, parent=parent, css=css)
    msgJSON <- toJSON(msg)
    sock$send(msgJSON)
}

removeChild <- function(pageID, child, css=TRUE) {
    sock <- pageInfo(pageID)$socket
    if (is.null(sock))
        stop("No socket open")
    msg <- list(fun="removeChild", child=child, css=css)
    msgJSON <- toJSON(msg)
    sock$send(msgJSON)
}

