\name{DOM interface}
\alias{appendChild}
\alias{appendChild,numeric,DOM_node,missing-method}
\alias{appendChild,numeric,DOM_node,DOM_node_ref-method}
\alias{removeChild}
\alias{removeChild,numeric,DOM_node_ref,missing-method}
\alias{removeChild,numeric,DOM_node_ref,DOM_node_ref-method}
\alias{replaceChild}
\alias{replaceChild,numeric,DOM_node,DOM_node_ref,missing-method}
\alias{replaceChild,numeric,DOM_node,DOM_node_ref,DOM_node_ref-method}
\alias{setAttribute}
\alias{setAttribute,numeric,DOM_node_ref,character,character-method}
\alias{getElementById}
\alias{getElementByIdCSS}
\alias{getElementsByTagName}
\alias{getElementsByTagNameCSS}
\alias{getElementsByClassName}
\alias{getElementsByClassNameCSS}
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
\S4method{appendChild}{numeric,DOM_node,missing}(pageID, child, parent, response=htmlNode(),
            ns = FALSE, async = FALSE, callback = NULL, tag = getRequestID())
\S4method{appendChild}{numeric,DOM_node,DOM_node_ref}(pageID, child, parent, response=htmlNode(),
            ns = FALSE, async = FALSE, callback = NULL, tag = getRequestID())
\S4method{removeChild}{numeric,DOM_node_ref,missing}(pageID, child, parent, response=htmlNode(),
            async = FALSE, callback = NULL, tag = getRequestID())
\S4method{removeChild}{numeric,DOM_node_ref,DOM_node_ref}(pageID, child, parent, response=htmlNode(),
            async = FALSE, callback = NULL, tag = getRequestID())
\S4method{replaceChild}{numeric,DOM_node,DOM_node_ref,missing}(pageID, newChild, oldChild, parent, response=htmlNode(),
            ns = FALSE, async = FALSE, callback = NULL, tag = getRequestID())
\S4method{replaceChild}{numeric,DOM_node,DOM_node_ref,DOM_node_ref}(pageID, newChild, oldChild, parent, response=htmlNode(),
            ns = FALSE, async = FALSE, callback = NULL, tag = getRequestID())
\S4method{setAttribute}{numeric,DOM_node_ref,character,character}(pageID, elt, attrName, attrValue,
             async = FALSE, callback = NULL, tag = getRequestID())
getElementById(pageID, id,
               async = !is.null(callback),
               callback = NULL, tag = getRequestID())
getElementByIdCSS(pageID, id,
                  async = !is.null(callback),
                  callback = NULL, tag = getRequestID())
getElementsByTagName(pageID, name, 
                     async = !is.null(callback),
                     callback = NULL, tag = getRequestID())
getElementsByTagNameCSS(pageID, name, 
                        async = !is.null(callback),
                        callback = NULL, tag = getRequestID())
getElementsByClassName(pageID, name, rootRef = NULL, 
                       css = TRUE, async = !is.null(callback),
                       callback = NULL, tag = getRequestID())
getElementsByClassNameCSS(pageID, name, rootRef = NULL, 
                          css = TRUE, async = !is.null(callback),
                          callback = NULL, tag = getRequestID())
appendScript(pageID, script, parentRef = "body",
             css = TRUE, async = !is.null(callback),
             callback = NULL, tag = getRequestID())
click(pageID, eltRef, css = TRUE, callback = NULL, tag = getRequestID())
}
\arguments{
  \item{pageID}{
    A page identifier, as returned by, e.g., \code{\link{htmlPage}}.
  }
  \item{child}{
    A DOM node representation, e.g., the result from \code{htmlNode()}
    (for a new node), or \code{css()} (for an existing node).
  }
  \item{parent}{
    A DOM node representation for an existing node (e.g., the result from 
    \code{css()}).
  }    
  \item{newChild}{
    A DOM node representation, e.g., the result from \code{htmlNode()}
    (for a new node), or \code{css()} (for an existing node).
  }
  \item{oldChild}{
    A DOM node representation for an existing node (e.g., the result from 
    \code{css()}).
  }
  \item{elt}{
    A DOM node representation for an existing node (e.g., the result from 
    \code{css()}).
  }
  \item{response}{
    A DOM node representation that provides a template for the return
    value, e.g., \code{htmlNode()} to return HTML or \code{css()} to
    return CSS.
  }
  \item{id}{
    A character value specifying an element id attribute.
  }
  \item{name}{
    A character value specifying a tag name (or \sQuote{*})
    or a class name (or 
    several class names separated by spaces).
  }
  \item{rootRef}{
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
  \item{attrName}{
    The attribute name.
  }
  \item{attrValue}{
    The attribute value (will be coerced to a character value).
  }
  \item{ns}{
    A namespace for the child element that is to be created.
    Valid values are \code{"HTML"} or \code{"SVG"} (or \code{NULL}).
  }
  \item{css}{
    A logical indicating whether the \code{parent} is a CSS selector
    (otherwise, it is an XPath).
  }
  \item{async}{
    A logical indicating whether the request is asynchronous.
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

  For adding SVG content within HTML, you should
  specify \code{ns="SVG"} and for adding
  HTML content within SVG, you should specify \code{ns="HTML"}.
  
  If \code{async} is \code{FALSE}, the request is synchronous and R
  blocks until the browser has sent a response.
  Otherwise, the request is asynchronous and control is returned to
  the command line.

  If \code{callback} is non-\code{NULL}, it will be run
  when the browser responds.  The \code{callback}
  function will be called with a single
  argument, which will contain the response from the web browser
  (see the Value section below).
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

  For \code{removeChild}, the HTML code for the
  child element that was removed.

  For \code{replaceChild}, the HTML code the old child element that was
  replaced.
  
  For \code{getElementById}, \code{getElementsByTagName}, and
  \code{getElementsByClassName}, the HTML code for the matching elements
  (or a missing value if no match is found).

  For \code{getElementByIdCSS}, \code{getElementsByTagNameCSS}, and
  \code{getElementsByClassNameCSS}, CSS selectors for the matching elements
  (or a missing value if no match is found).

  For \code{setAttribute}, \code{appendScript}, and \code{click}, \code{NULL}.
}
\references{
  \url{https://www.w3.org/DOM/}
}
\author{
  Paul Murrell
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{htmlPage}}, \code{\link{htmlNode}}
}
\examples{
headlessPage <- htmlPage(headless=TRUE)
appendChild(headlessPage, htmlNode("<p>test</p>"))
closePage(headlessPage)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
