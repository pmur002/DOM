
# Function to remove all whitespace (including newlines) from HTML text
# (to allow comparison between source HTML and HTML returned from PhantomJS)
minifyHTML <- function(x) {
    gsub(" +", "", gsub("\n", "", x))
}