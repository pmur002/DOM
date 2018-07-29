
.onLoad <- function(libname, pkgname)
{
    options(DOM.client=browserClient,
            # Spew debugging output (in R and in browser console)
            DOM.debug=FALSE,
            # Timeout for waiting for requests (seconds)
            DOM.limit=5,
            # Width/height of PhantomJS page
            DOM.width=800,
            DOM.height=600)
}

## Create default clients

## (system default) browser client
browserClient <-
    makeClient(
        run=function(url, host, port, tag) {
            browseURL(url)
        },
        ws=TRUE)

## phantomJS client
phantomClient <-
    makeClient(
        run=function(url, host, port, tag) {
            phantomURL(url, host, port, tag)
        },
        kill=kill,
        ws=FALSE)

## RStudio viewer client
rstudioClient <-
    makeClient(
        run=function(url, host, port, tag) {
            rsviewer <- getOption("viewer")
            if (viewer && !is.null(rsviewer)) {
                rsviewer(url)
            } else {
                browseURL(url)
            }
        },
        ws=TRUE)
