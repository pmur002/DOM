
# Code for running PhantomJS (headless browser)

phantomURL <- function(url, port, tag) {
    if (!requireNamespace("whisker", quietly=TRUE)) {
        stop("Headless browser requires package 'whisker'")
    }
    template <- readLines(system.file("templates", "phantom.js", package="DOM"))
    DOMjs <- readLines(system.file("JS", "DOM.js", package="DOM"))
    ## jsFile <- tempfile(fileext=".js")
    jsFile <- "phantom-test.js"
    writeLines(c(DOMjs,
                 whisker::whisker.render(template,
                                         list(url=url, port=port, tag=tag))),
               jsFile)
    phantom_run(jsFile)
}

# Code below here taken almost entirely from Winston Chang's 'webshot' package
# https://github.com/wch/webshot/

phantom_run <- function(args, wait = FALSE) {
  phantom_bin <- find_phantom()

  # Make sure args is a char vector
  args <- as.character(args)

  system2(phantom_bin, args = args, wait = wait)
}


# Find PhantomJS from PATH, APPDATA, system.file('webshot'), ~/bin, etc
find_phantom <- function() {
    path <- Sys.which( "phantomjs" )
    if (path != "") return(path)
    for (d in phantom_paths()) {
        exec <- if (is_windows()) "phantomjs.exe" else "phantomjs"
        path <- file.path(d, exec)
        if (utils::file_test("-x", path))
            break
        else
            path <- ""
    }
    if (path == "") {
        stop("PhantomJS not found. ",
             "If it is installed, please make sure the phantomjs executable ",
             "can be found via the PATH variable.")
    }
    path.expand(path)
}

# Possible locations of the PhantomJS executable
phantom_paths <- function() {
    if (is_windows()) {
        path <- Sys.getenv('APPDATA', '')
        path <- if (dir_exists(path)) file.path(path, 'PhantomJS')
    } else if (is_osx()) {
        path <- '~/Library/Application Support'
        path <- if (dir_exists(path)) file.path(path, 'PhantomJS')
    } else {
        path <- '~/bin'
    }
    path <- c(path, system.file('PhantomJS', package = 'webshot'))
    path
}

dir_exists <- function(path) utils::file_test('-d', path)
is_windows <- function() .Platform$OS.type == "windows"
is_osx <- function() Sys.info()[['sysname']] == 'Darwin'

