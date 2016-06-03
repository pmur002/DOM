\name{closePage}
\alias{closePage}
\title{
  Close a web page.
}
\description{
  Close a connection to a web page.
}
\usage{
closePage(pageID)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pageID}{
    A page identifier, as returned by, e.g., \code{\link{htmlPage}} et al.
  }
}
\details{
  This function severs the connection to the web browser that is
  showing the web page, so no further communication can occur.
  The browser tab or window that was opened to show the web page
  is not closed (though if the web page is being viewed in a
  headless browser, the headless browser is shut down).
}
\value{
  The content of the web page (though only for headless browsers currently).
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
