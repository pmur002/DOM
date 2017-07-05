\name{RDOM.Rcall}
\alias{RDOM.Rcall}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Call R from a web browser.
}
\description{
  Call an R function from javascript code in a web page.
}
\details{
  This is \emph{not} an R function; it is a javascript function that is
  available within the web page that R is connected to.  It can be used
  to call an R function from the web page, e.g., in response to a user
  event, and optionally run a callback function in the web page
  when the R function returns a result.

  The \code{RDOM.Rcall} function takes four arguments:
  \itemize{
    \item The name of the R function to call (as a string).
    \item A reference to a DOM node (or a collection of DOM nodes).
    \item An array of types that determine how the nodes will be
    represented in R (one or more of \code{"HTML"}, \code{"SVG"},
    \code{"CSS"}, \code{"XPath"}).
    \item A callback function (or \code{null}).
  }
  
  A typical usage might look like this (to call an R function
  when a button is clicked on the web page):
  
  \preformatted{<button onclick='RDOM.Rcall("fnname", this, [ "HTML" ], null)'>}

  The R function that is called by \code{RDOM.Rcall} is provided with
  a variable number of arguments equal to the number of DOM nodes times
  the number of node types (with the number of types changing fastest).
  In the example above, the R function would receive only one argument.

  It is also possible to pass raw javascript values in the second
  argument, instead of DOM nodes.  In this case, the type argument
  should be one of \code{"string"}, \code{"number"}, \code{"boolean"},
  or \code{"JSON"} (for more complex sets of values).
  
  The return value of the R function (if any) is converted to a JSON
  object and passed to the callback function specified
  in the call to \code{RDOM.Rcall}).
}
\author{
  Paul Murrell
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{htmlPage}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
