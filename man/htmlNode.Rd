\name{htmlNode}
\alias{htmlNode}
\alias{svgNode}
\alias{javascript}
\alias{css}
\alias{xpath}
\alias{nodePtr}
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
css(x="")
xpath(x="")
nodePtr(x="")
}
\arguments{
  \item{x}{ A character value consisting of HTML, SVG, JavaScript,
    CSS, or XPath code, as appropriate.
  }
}
\value{
  An S4 object of class \code{DOM_node_HTML}, \code{DOM_node_SVG},
  \code{DOM_node_JS},
  \code{DOM_node_CSS}, or \code{DOM_node_XPath}, respectively.
}
\examples{
htmlNode("<p>test</p>")
}
\keyword{ IO }
