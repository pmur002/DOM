\name{DOM interface}
\alias{createElement}
\alias{createElementNS}
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
\alias{getElementById,numeric,character-method}
\alias{getElementsByTagName}
\alias{getElementsByTagName,numeric,character-method}
\alias{getElementsByClassName}
\alias{getElementsByClassName,numeric,character,missing-method}
\alias{getElementsByClassName,numeric,character,DOM_node_ref-method}
\alias{getProp}
\alias{getProp,numeric,DOM_obj_ref,character-method}
\alias{setProp}
\alias{setProp,numeric,DOM_obj_ref,character,DOM_obj_ref-method}
\alias{setProp,numeric,DOM_obj_ref,character,DOM_value-method}
\alias{$,DOM_obj_ptr-method}
\alias{$<-,DOM_obj_ptr-method}
\alias{click}
\alias{click,numeric,DOM_node_ref-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Send DOM requests to a web page
}
\description{
  Send DOM requests to a web page to modify the web page content and/or
  access existing web page content.
}
\usage{
createElement(pageID, tagName, response=nodePtr(),
              async = FALSE, callback = NULL, tag = getRequestID())
createElementNS(pageID, namespace, tagName, response=nodePtr(),
                async = FALSE, callback = NULL, tag = getRequestID())
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
\S4method{getElementById}{numeric,character}(pageID, id, response=htmlNode(),
               async = FALSE, callback = NULL, tag = getRequestID())
\S4method{getElementsByTagName}{numeric,character}(pageID, name, response=htmlNode(),
                     async = FALSE, callback = NULL, tag = getRequestID())
\S4method{getElementsByClassName}{numeric,character,missing}(pageID, name, root, response=htmlNode(),
                       async = FALSE, callback = NULL, tag = getRequestID())
\S4method{getElementsByClassName}{numeric,character,DOM_node_ref}(pageID, name, root, response=htmlNode(),
                       async = FALSE, callback = NULL, tag = getRequestID())
\S4method{click}{numeric,DOM_node_ref}(pageID, elt, callback = NULL, tag
= getRequestID())
\S4method{getProp}{numeric,DOM_obj_ref,character}(pageID, object,
propName, response = NULL,
                       async = FALSE, callback = NULL, tag = getRequestID())
\S4method{setProp}{numeric,DOM_obj_ref,character,DOM_obj_ref}(pageID, object,
propName, value,
                       async = FALSE, callback = NULL, tag = getRequestID())
\S4method{setProp}{numeric,DOM_obj_ref,character,DOM_value}(pageID, object,
propName, value,
                       async = FALSE, callback = NULL, tag = getRequestID())
}
\arguments{
  \item{pageID}{
    A page identifier, as returned by, e.g., \code{\link{htmlPage}}.
  }
  \item{tagName}{
    A character value describing an HTML (or SVG) tag name.
  }
  \item{namespace}{
    A character value describing a valid namespace
    (e.g., \code{"http://www.w3.org/1999/xml"} or
    \code{"http://www.w3.org/2000/svg"})
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
  \item{object}{
    A DOM object representation for an existing node (e.g., the result
    from \code{objPtr()}).
  }
  \item{propName}{
    A character value specifying a DOM object property name.
  }
  \item{value}{
    A DOM object representation for an existing node (or a simple
    numeric, character, or logical value.
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
  \item{root}{
    A DOM node representation for an existing node (e.g., the result from 
    \code{css()}).
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
    page as a child of an existing element, \code{parent}, \emph{or}
    it adds an existing element, \code{child}, to \code{parent}.

    \item \code{removeChild} removes an existing element.  If
    \code{parent} is \code{NULL}, the parent is implicit, but it is
    also possible to explicitly specify the parent.  In the latter case,
    it is possible for the request to fail (because the specified parent
    and child are not actually related in the page).

    \item \code{replaceChild} replaces an existing element.  The
    new element is a new element, \code{newChild}, or an existing
    element, \code{newChild}.  In the latter case, the new element is
    moved from its old position to the new position.
    If \code{parent} is \code{NULL}, the parent is implicit, but it is
    also possible to explicitly specify the parent.

    \item \code{click} sends a click event to an element, \code{elt}.
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
  Most functions return a DOM node, 
  which could be, e.g., HTML or CSS, depending on the \code{response}
  argument.

  For \code{createElement}, the default is to return a
  \code{DOM_note_ptr} object (because the element created is not
  part of the document, so cannot be identified from R any other way).
  
  For \code{appendChild}, the child element that was added.

  For \code{removeChild}, the child element that was removed.

  For \code{replaceChild}, the old child element that was
  replaced.
  
  For \code{getElementById}, \code{getElementsByTagName}, and
  \code{getElementsByClassName}, the matching elements.

  Both \code{setAttribute} and \code{click} return \code{NULL}.
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
