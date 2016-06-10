\name{DOM interface}
\alias{appendChild}
\alias{appendChildCSS}
\alias{removeChild}
\alias{removeChildCSS}
\alias{replaceChild}
\alias{replaceChildCSS}
\alias{setAttribute}
\alias{appendScript}
\alias{click}
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
replaceChild(pageID, newChild = NULL, newChildRef = NULL,
             oldChildRef = NULL, parentRef = NULL, css = TRUE,
             callback = NULL, tag = getRequestID())
replaceChildCSS(pageID, newChild = NULL, newChildRef = NULL,
                oldChildRef = NULL, parentRef = NULL, css = TRUE,
                callback = NULL, tag = getRequestID())
setAttribute(pageID, eltRef, attrName, attrValue, css = TRUE,
             callback = NULL, tag = getRequestID())
appendScript(pageID, script, parentRef = "body",
             css = TRUE, callback = NULL, tag = getRequestID())
click(pageID, eltRef, css = TRUE, callback = NULL, tag = getRequestID())
}
\arguments{
  \item{pageID}{
    A page identifier, as returned by, e.g., \code{\link{htmlPage}}.
  }
  \item{child}{
    A character value describing a valid HTML element.
  }
  \item{childRef}{
    A character value describing an existing element in the page as a CSS
    selector (or XPath, if \code{css=FALSE}).
  }
  \item{newChild}{
    A character value describing a valid HTML element.
  }
  \item{newChildRef}{
    A character value describing an existing element in the page as a CSS
    selector (or XPath, if \code{css=FALSE}).
  }
  \item{oldChildRef}{
    A character value describing an existing element in the page as a CSS
    selector (or XPath, if \code{css=FALSE}).
  }
  \item{script}{
    A character value containing JavaScript code (\emph{without}
    enclosing <script> tags).
  }
  \item{parentRef}{
    A character value describing an existing element in the page as a CSS
    selector (or XPath, if \code{css=FALSE}).
  }
  \item{eltRef}{
    A character value describing an existing element in the page as a CSS
    selector (or XPath, if \code{css=FALSE}).
  }
  \item{attrName}{
    The attribute name.
  }
  \item{attrValue}{
    The attribute value (will be coerced to a character value).
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

    \item \code{replaceChild} replaces an existing element.  The
    new element is a new element, \code{newChild}, or an existing
    element, \code{newChildRef}.  In the latter case, the new element is
    moved from its old position to the new position.
    If \code{parentRef} is \code{NULL}, the parent is implicit, but it is
    also possible to explicitly specify the parent.

    \item \code{click} sends a click event to an element, \code{eltRef}.
  }

  If \code{callback} is \code{NULL}, the request is synchronous and R
  blocks until the browser has sent a response.

  Otherwise, the request is asynchronous and \code{callback} will be run
  when the browser responds.

  The \code{callback} can contain further requests, but they \emph{must}
  be asynchronous requests.

  A \code{click} is \emph{always} asynchronous (the request may be
  fulfilled in R before the browser has completed handling the click
  event); this function is aimed at debugging with a headless browser.
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

  For \code{replaceChild}, the HTML code the old child element that was
  replaced.
  
  For \code{removeChildCSS}, a CSS selector for the
  old child element that was replaced (\emph{before} it was replaced).

  For \code{setAttribute}, \code{appendScript}, and \code{click},
  an empty string.
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
