\name{DOM interface}
\alias{appendChild}
\alias{appendChildCSS}
\alias{removeChild}
\alias{removeChildCSS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Send DOM requests to a web page
}
\description{
  Send DOM requests to a web page to modify the web page content and/or
  access existing web page content.
}
\usage{
appendChild(pageID, child = NULL, childRef = NULL, parentRef = "body",
            css = TRUE, callback = NULL, tag = getRequestID())
appendChildCSS(pageID, child = NULL, childRef = NULL, parentRef = "body",
               css = TRUE, callback = NULL, tag = getRequestID())
removeChild(pageID, childRef, parentRef = NULL, css = TRUE,
            callback = NULL, tag = getRequestID())
removeChildCSS(pageID, childRef, parentRef = NULL, css = TRUE,
               callback = NULL, tag = getRequestID())
}
\arguments{
  \item{pageID}{
    A page identifier, as returned by, e.g., \code{\link{htmlPage}}.
  }
  \item{child}{
    A character vector describing a valid HTML element.
  }
  \item{childRef}{
    A character vector describing an existing element in the page as a CSS
    selector (or XPath, if \code{css=FALSE}).
  }
  \item{parentRef}{
    A character vector describing an existing element in the page as a CSS
    selector (or XPath, if \code{css=FALSE}).
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

  \itemize{
    \item \code{appendChild} adds a new element, \code{child}, to the
    page as a child of an existing element, \code{parentRef}, \emph{or}
    it adds an existing element, \code{childRef}, to \code{parentRef}.

    \item \code{removeChild} removes an existing element.  If
    \code{parentRef} is \code{NULL}, the parent is implicit, but it is
    also possible to explicitly specify the parent.  In the latter case,
    it is possible for the request to fail (because the specified parent
    and child are not actually related in the page).
  }
  
  If \code{callback} is \code{NULL}, the request is synchronous and R
  blocks until the browser has sent a response.

  Otherwise, the request is asynchronous and \code{callback} will be run
  when the browser responds.

  The \code{callback} can contain further requests, but they \emph{must}
  be asynchronous requests.
}
\value{
  All functions return a character vector.
  
  For \code{appendChild}, the HTML code for the
  child element that was added.

  For \code{appendChildCSS}, a CSS selector for the
  child element that was added.

  For \code{removeChild}, the HTML code for the
  child element that was removed.

  For \code{removeChildCSS}, a CSS selector for the
  child element that was removed (\emph{before} the child was removed).
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
