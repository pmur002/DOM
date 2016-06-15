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

  The \code{RDOM.Rcall} function takes three arguments:
  \itemize{
    \item The name of the R function to call (as a string).
    \item A reference to an HTML element.
    \item A callback function (or \code{null}).
  }
  
  A typical usage might look like this (to call an R function
  when a button is clicked on the web page):
  
  \preformatted{<button onclick='RDOM.Rcall("fnname", this, null)'>}

  The R function that is called by \code{RDOM.Rcall} is provided with
  two arguments:
  \itemize{
    \item An HTML representation of the element passed to
    \code{RDOM.Rcall}
    \item A CSS selector for the element passed to \code{RDOM.Rcall}
  }

  The return value of the R function (if any) should be a single
  character value (which is passed to the callback function specified
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
