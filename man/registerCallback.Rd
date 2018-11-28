\name{Register Callbacks}
\alias{registerCallback}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Register a DOM Callback.
}
\description{
  Register an R function as a callback.  This allows callbacks to
  not pollute the global environment.
}
\usage{
registerCallback(f, name = deparse(substitute(f))) 
}
\arguments{
  \item{f}{
    A function that will be called following a DOM request
    such as \code{\link{appendChild}}.  The function will be
    called with a single value, which is the result of the
    request.
  }
  \item{name}{
    The name by which the function will be known.
    This is the name that must be used in the DOM request.
  }
}
\value{
  Called for its side-effect of registering the callback function.
}
\author{
  Paul Murrell
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{htmlPage}},
  \code{\link{appendChild}}  
}
\examples{
cbgen <- function() {
    count <- 0
    function(value) {
        count <<- count + 1
        cat(count, "\n")
    }
}
cb <- cbgen()
registerCallback(cb)
headlessPage <- htmlPage(client=phantomClient)
appendChild(headlessPage, htmlNode("<p>test</p>"), callback=cb)
appendChild(headlessPage, htmlNode("<p>test</p>"), callback=cb)
closePage(headlessPage)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
