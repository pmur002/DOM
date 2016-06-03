\name{DOM interface}
\alias{appendChild}
\alias{removeChild}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Send DOM requests to a web page
}
\description{
  Send DOM requests to a web page to modify the web page content and/or
  access existing web page content.
}
\usage{
appendChild(pageID, node, parent = "body", css = TRUE,
            callback = NULL, tag = getRequestID())
removeChild(pageID, child, css = TRUE,
            callback = NULL, tag = getRequestID())
}
\arguments{
  \item{pageID}{
    A page identifier, as returned by, e.g., \code{\link{htmlPage}}.
  }
  \item{node}{
    A character vector describing a valid HTML element.
  }
  \item{parent}{
    A CSS selector (or XPath) describing the parent element to which
    a child is to be added.
  }
  \item{child}{
    A CSS selector (or XPath) describing the child element that is
    to be removed.
  }
  \item{css}{
    A logical indicating whether the \code{parent} is a CSS selector
    (otherwise, it is an XPath).
  }
  \item{callback}{
    A function to be called when the DOM request has been fulfilled.
  }
  \item{tag}{
    A unique request identifier.
  }
}
\details{
  These functions implement (some of) the basic DOM interface to web
  page content.
  
  If \code{callback} is \code{NULL}, the request is synchronous and R
  blocks until the browser has sent a response.

  Otherwise, the request is asynchronous and \code{callback} will be run
  when the browser responds.

  The \code{callback} can contain further requests, but they \emph{must}
  be asynchronous requests.
}
\value{
  All functions return a character vector.
  
  For \code{appendChild()}, the HTML code for the
  child element that was added.

  For \code{removeChild()}, the HTML code for the
  child element that was removed.
}
\references{
  \url{https://www.w3.org/DOM/}
}
\author{
  Paul Murrell
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{htmlPage}}
}
\examples{
headlessPage <- htmlPage(headless=TRUE)
appendChild(headlessPage, "<p>test</p>")   
closePage(headlessPage)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
