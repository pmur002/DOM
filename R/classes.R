
# Classes for different representations of DOM objects in R: 
#   HTML
#   CSS selector
#   unique identifier
# ALL are essentially just a character vector, but different classes
# allow us to respond differently via S4 method dispatch

# Simple types that we will pass through
setClassUnion("DOM_value", c("numeric", "character", "logical"))

# Base class for all representations
setClass("DOM_obj",
         contains="character")

# Some basic values (for returning responses from browser)
setClass("DOM_string",
         contains="DOM_obj")
setClass("DOM_numeric",
         contains="DOM_obj")
setClass("DOM_boolean",
         contains="DOM_obj")

# A reference to an existing DOM object
setClass("DOM_obj_ref",
         contains="DOM_obj",
         slots=list(pageID="numeric"))
# A useful combination
setClassUnion("DOM_obj_response",
              c("DOM_obj_ref", "DOM_value"))
# A pointer to an existing DOM object
setClass("DOM_obj_ptr",
         contains="DOM_obj_ref")

# A DOM node
setClass("DOM_node",
         contains="DOM_obj")

# Representations that describe a new node
setClass("DOM_node_literal",
         contains="DOM_node")
# The representation is HTML
setClass("DOM_node_HTML",
         contains="DOM_node_literal")
# The representation is SVG
setClass("DOM_node_SVG",
         contains="DOM_node_literal")
# The representation is JavaScript
setClass("DOM_node_JS",
         contains="DOM_node_literal")

# Representations that refers to an existing node
setClass("DOM_node_ref",
         contains=c("DOM_node", "DOM_obj_ref"))
# The representation is a CSS selector
setClass("DOM_node_CSS",
         contains="DOM_node_ref")
# The representation is an XPath
setClass("DOM_node_XPath",
         contains="DOM_node_ref")
# The representation is a numeric index into the RDOM javascript global array
setClass("DOM_node_ptr",
         contains=c("DOM_node_ref", "DOM_obj_ptr"))

# CSS objects
setClass("DOM_StyleSheetList_ptr",
         contains="DOM_obj_ptr")
setClass("DOM_CSSStyleSheet_ptr",
         contains="DOM_obj_ptr")
setClass("DOM_CSSRule_ptr",
         contains="DOM_obj_ptr")
    
# Constructors
htmlNode <- function(x="") {
    new("DOM_node_HTML", x)
}

svgNode <- function(x="") {
    new("DOM_node_SVG", x)
}

javascript <- function(x="") {
    new("DOM_node_JS", x)
}

css <- function(x="", pageID=numeric()) {
     new("DOM_node_CSS", x, pageID=pageID)
}

xpath <- function(x="", pageID=numeric()) {
    new("DOM_node_XPath", x, pageID=pageID)
}

nodePtr <- function(x="", pageID=numeric()) {
    new("DOM_node_ptr", x, pageID=pageID)
}

objPtr <- function(x="", pageID=numeric()) {
    new("DOM_obj_ptr", x, pageID=pageID)
}

Numeric <- function(x="") {
    new("DOM_numeric", x)
}

String <- function(x="") {
    new("DOM_string", x)
}

Boolean <- function(x="") {
    new("DOM_boolean", x)
}

# Need to allow for errors
setClass("DOM_error",
         contains="character")

# Some class unions
allowErrorOrNull <- function(class) {
    # Allow for error (for example)
    setClassUnion(paste0(class, "_OR_error"),
                  c("DOM_error", class))
    # Allow for error OR NULL (for asynchronous requests)
    setClassUnion(paste0(class, "_OR_error_OR_NULL"),
                  c(paste0(class, "_OR_error"), "NULL"))
}
allowErrorOrNull("DOM_node")
allowErrorOrNull("DOM_obj_response")

# Subsetting DOM nodes should return DOM nodes ...
setMethod("[",
          signature(x="DOM_obj", i="ANY", j="missing", drop="missing"),
          function(x, i, j, ..., drop) {
              new(class(x), unclass(x)[i])
          })

setMethod("[",
          signature(x="DOM_obj_ref", i="ANY", j="missing", drop="missing"),
          function(x, i, j, ..., drop) {
              pageID <- x@pageID
              new(class(x), unclass(x)[i], pageID=pageID)
          })

