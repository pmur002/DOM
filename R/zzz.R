
.onLoad <- function(libname, pkgname)
{
    options(DOM.headless=FALSE,
            # Spew debugging output (in R and in browser console)
            DOM.debug=FALSE,
            # Timeout for waiting for requests (seconds)
            DOM.limit=5,
            # Width/height of PhantomJS page
            DOM.width=800,
            DOM.height=600)
}

    
