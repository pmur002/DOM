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
htmlPage(html = "", head = "",
         host = "127.0.0.1", client = getOption("DOM.client"))
filePage(file, client = getOption("DOM.client"))
urlPage(url, client = getOption("DOM.client"))
}
\arguments{
  \item{html}{
    A character vector containing HTML code, which will be used as the
    \emph{body} of
    the web page that is opened.
  }
  \item{head}{
    A character vector that will be added to the \emph{head} of the web
    page.
  }
  \item{host}{
    The client host.  By default this is the localhost, but in theory
    it allows for a remote client.
  }
  \item{client}{
    A definition of the client, as created by \code{makeClient}.
  }
  \item{file}{
    A path to a file on the local file system, which should be a text
    file containing HTML (plus possibly SVG, CSS, javascript) describing
    a web page.
  }
  \item{url}{
    A URL to a web page on the internet.
  }
}
\details{
  By default, the web page described by \code{html}, \code{file},
  or \code{url} is opened in the default browser
  (\code{getOptions("browser")}).  A headless browser (PhantomJS) is
  supported, which may be useful for testing.

  For \code{filePage()} and \code{urlPage()} to work, the
  \code{RDOM.user.js} user script must be installed in the browser
  (for greasemonkey with Firefox).  There is also \code{RDOM.tamper.js}
  (for Tampermonkey with, e.g., Chrome).  In addition, for
  \code{filePage()}, in Firefox at least,
  \code{greasemonkey.fileIsGreaseable} must be
  set to \code{true} in \code{about:config}.  \code{urlPage()} currently
  only supports the \code{http://} protocol (not \code{https://}).

  For \code{client=phantomClient} to work, PhantomJS must be installed.
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
headlessPage <- htmlPage(client=phantomClient)
appendChild(headlessPage, htmlNode("<p>test</p>"))
closePage(headlessPage)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
