\name{Open Page}
\alias{htmlPage}
\alias{filePage}
\alias{urlPage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Open a web page in a browser.
}
\description{
  Open a web page in a browser and provide a handle to that web page.
}
\usage{
htmlPage(html = "", headless = FALSE)
filePage(file, headless = FALSE)
urlPage(url, headless = FALSE)
}
\arguments{
  \item{html}{
    A character vector containing HTML code, which will be used as the
    \emph{body} of
    the web page that is opened.
  }
  \item{file}{
    A path to a file on the local file system, which should be a text
    file containing HTML (plus possibly SVG, CSS, javascript) describing
    a web page.
  }
  \item{url}{
    A URL to a web page on the internet.
  }
  \item{headless}{
    A logical indicating whether to open the web page in a
    \dQuote{headless} browser (currently PhantomJS).  
  }
}
\details{
  By default, the web page described by \code{html}, \code{file},
  or \code{url} is opened in the default browser
  (\code{getOptions("browser")}).  A headless browser (PhantomJS) is
  supported, which may be useful for testing.
}
\value{
  A page identifier, to be used to send DOM requests to the web page, or to
  close the web page.
}
\author{
  Paul Murrell
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{appendChild}}
}
\examples{
headlessPage <- htmlPage(headless=TRUE)
appendChild(headlessPage, "<p>test</p>")   
closePage(headlessPage)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")