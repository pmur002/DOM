\name{htmlNode}
\alias{htmlNode}
\alias{svgNode}
\alias{javascript}
\alias{css}
\alias{xpath}
\alias{nodePtr}
\alias{objPtr}
\alias{Number}
\alias{String}
\alias{Boolean}
\alias{[,DOM_obj,ANY,missing,missing-method}
\alias{[,DOM_obj_ref,ANY,missing,missing-method}
\title{
  Create a DOM node representation.
}
\description{
  Create a new DOM node via explicit HTML or SVG or JavaScript code
  or a referece to an existing DOM node via CSS or XPath
}
\usage{
htmlNode(x="")
svgNode(x="")
javascript(x="")
css(x="", pageID=numeric())
xpath(x="", pageID=numeric())
nodePtr(x="", pageID=numeric())
objPtr(x="", pageID=numeric())
Number(x="")
String(x="")
Boolean(x="")
}
\arguments{
  \item{x}{ A character value consisting of HTML, SVG, JavaScript,
    CSS, or XPath code, as appropriate. }
  \item{pageID}{ A numeric value identifying a web page connection; see
    \code{\link{htmlPage}}.}
}
\value{
  An S4 object of class \code{DOM_node_HTML}, \code{DOM_node_SVG},
  \code{DOM_node_JS},
  \code{DOM_node_CSS}, \code{DOM_node_XPath},
  \code{DOM_node_ptr}, \code{DOM_obj_ptr}, \code{DOM_number},
  \code{DOM_character}, or \code{DOM_boolean}, respectively.
}
\examples{
htmlNode("<p>test</p>")
}
\keyword{ IO }
