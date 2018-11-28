\name{Create Client}
\alias{makeClient}
\alias{browserClient}
\alias{phantomClient}
\alias{rstudioClient}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Define a DOM client.
}
\description{
  Define a DOM client  for R to communicate with.
  Several predefined clients are provided.
}
\usage{
makeClient(run, kill = NULL, ws = TRUE)
browserClient
phantomClient
rstudioClient
}
\arguments{
  \item{run}{
    A function that will be called to initialise the client.
    This function should accept at least a \code{url} argument, but
    will also be passed arguments \code{host},
    \code{port}, and \code{tag}.
  }
  \item{kill}{
    This function will be called to allow the client to shut itself down.
    It is not required to do anything.
  }
  \item{ws}{
    A logical indicating what sort of app the client will run.
  }
}
\details{
  The predefined \code{browserClient} opens a window (or tab)
  for the default web browser,
  the \code{phantomClient} runs PhantomJS (a headless browser),
  and the \code{rstudioClient} works with the R Studio Viewer panel.

  All clients create a web socket connection with  R.
}
\value{
  A \code{"DOMclient"} object.
}
\author{
  Paul Murrell
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{htmlPage}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ IO }% use one of  RShowDoc("KEYWORDS")
