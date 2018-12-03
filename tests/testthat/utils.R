
# For testing, all pages are headless
options(DOM.client=phantomClient)

# Function to remove all whitespace (including newlines) from HTML text
# (to allow comparison between source HTML and HTML returned from PhantomJS)
minifyHTML <- function(x, collapse=TRUE) {
    x <- unclass(x)
    if (collapse) {
        x <- paste(x, collapse="")
    }
    gsub("> ", ">", gsub(" <", "<", gsub(" +", " ", gsub("\n", "", x))))
}
