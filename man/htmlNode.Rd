\name{htmlNode}
\alias{htmlNode}
\alias{svgNode}
\alias{css}
\alias{xpath}
\title{
  Create a DOM node representation.
}
\description{
  Create a new DOM node via explicit HTML or SVG code
  or a referece to an existing DOM node via CSS or XPath
}
\usage{
htmlNode(x="")
svgNode(x="")
css(x="")
xpath(x="")
}
\arguments{
  \item{x}{ A character value consisting of HTML, SVG, CSS, or XPath
    code, as appropriate.
  }
}
\value{
  An S4 object of class \code{DOM_node_HTML}, \code{DOM_node_SVG},
  \code{DOM_node_CSS}, or \code{DOM_node_XPath}, respectively.
}
\examples{
htmlNode("<p>test</p>")
}
\keyword{ IO }
