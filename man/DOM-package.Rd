\docType{package}
\name{DOM-package}
\alias{DOM-package}
\alias{debug}
\alias{getPage}
\alias{R_PHANTOMJSCMD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Drive web browser content from R.
}
\description{
  Drive web browser content from R.
}
\details{
  Start a web browser from R and manipulate the document content in the
  browser from R (using a websocket connection).

  There are three options for the initial web page:
  \itemize{
    \item \code{\link{htmlPage}} opens a page with the web page <body>
    given in the call (default is a blank page).

    \item \code{\link{filePage}} opens a web page from a file on the
    local filesystem.

    \item \code{\link{urlPage}} opens a web page with the given URL
    (though only \code{http:} is currently supported.
  }

  Once the connection to the web page is established, functions are
  provided to perform basic DOM operations on the document content,
  e.g., \code{\link{appendChild}}, \code{\link{removeChild}},
  \code{\link{setAttribute}}, and so on.

  The DOM operations can be synchronous, in which case R blocks until a
  response is received from the browser, or asynchronous (with a callback).

  It is also possible to use \code{\link{RDOM.Rcall}} within javascript
  on the web page to (asynchronously) make a request to R from the
  browser (e.g., in response to a user event such as a mouse click).

  The connection to the browser should be closed with
  \code{\link{closePage}}.

  The hidden function \code{DOM:::debug} can be used to turn on
  debugging output in both R and the browser.

  The hidden function \code{DOM:::getPage} can be used to obtain
  the current page content.

  The environment variable \code{R_PHANTOMJSCMD} can be used to
  specify the path to the phantomJS executable (for the headless
  browser).

  The timeout for requests sent to the browser is controlled by
  \code{options("DOM.limit")} (default 5 [seconds]).  Setting this
  larger can be useful to stop R timing out while debugging in the
  browser.
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
appendChild(headlessPage, htmlNode("<p>test</p>"))
closePage(headlessPage)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ package }% use one of  RShowDoc("KEYWORDS")
