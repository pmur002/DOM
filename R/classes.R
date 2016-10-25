
# Classes for different representations of DOM objects in R: 
#   HTML
#   CSS selector
#   unique identifier
# ALL are essentially just a character vector, but different classes
# allow us to respond differently via S4 method dispatch

# Base class for all representations
setClass("DOM_node",
         contains="character")

# Representations that describe a new node
setClass("DOM_node_new",
         contains="DOM_node")
# The representation is HTML
setClass("DOM_node_HTML",
         contains="DOM_node_new")
# The representation is SVG
setClass("DOM_node_SVG",
         contains="DOM_node_new")
# The representation is JavaScript
setClass("DOM_node_JS",
         contains="DOM_node_new")

# Representations that refers to an existing node
setClass("DOM_node_ref",
         contains="DOM_node")
# The representation is a CSS selector
setClass("DOM_node_CSS",
         contains="DOM_node_ref")
# The representation is an XPath
setClass("DOM_node_XPath",
         contains="DOM_node_ref")
# The representation is a numeric index into the RDOM javascript global array
setClass("DOM_node_ptr",
         contains="DOM_node_ref")

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

css <- function(x="") {
     new("DOM_node_CSS", x)
}

xpath <- function(x="") {
    new("DOM_node_XPath", x)
}

nodePtr <- function(x="") {
    new("DOM_node_ptr", x)
}

# Need to allow for errors
setClass("DOM_error",
         contains="character")

# Allow for a DOM node OR an error (for example)
setClassUnion("DOM_node_OR_error", c("DOM_error", "DOM_node"))

# Allow for a DOM node OR an error OR NULL (for asynchronous requests)
setClassUnion("DOM_node_OR_error_OR_NULL", c("DOM_node_OR_error", "NULL"))

# Subsetting DOM nodes should return DOM nodes ...
setMethod("[",
          signature(x="DOM_node", i="ANY", j="missing", drop="missing"),
          function(x, i, j, ..., drop) {
              new(class(x), unclass(x)[i])
          })

