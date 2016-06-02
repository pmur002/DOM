
library(DOM)
library(testthat)

source("utils.R")

fileURL <- system.file("HTML", "RDOM.html", package="DOM")
url <- "http://pmur002.neocities.org/index.html"

test_that("appendChild", {    
    headlessPage <- htmlPage(headless=TRUE)
    appendChild(headlessPage, "<p>test<p>")
    expect_equal(kill(headlessPage),
                 "<html><head></head><body><p>test</p></body></html>")
    closePage(headlessPage)

    headlessFile <- filePage(fileURL, headless=TRUE)
    appendChild(headlessFile, "<p>test<p>")
    expect_match(minifyHTML(kill(headlessFile)),
                 "<p>test</p></body></html>$")
    closePage(headlessFile)

    headlessURL <- urlPage(url, headless=TRUE)
    appendChild(headlessURL, "<p>test<p>")
    expect_match(minifyHTML(kill(headlessURL)),
                 "<p>test</p></body></html>$")
    closePage(headlessURL)
})

test_that("appendChild with callback", {    
    headlessPage <- htmlPage(headless=TRUE)
    result <- NULL
    appendChild(headlessPage, "<p>test</p>",
                callback=function(value) { result <<- value })
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.1)
    expect_equal(result, "<p>test</p>")
    kill(headlessPage)
    closePage(headlessPage)
})

test_that("appendChild with callback with callback", {    
    headlessPage <- htmlPage(headless=TRUE)
    appendChild(headlessPage, "<p>test</p>",
                callback=function(value) {
                    appendChild(headlessPage, "<p>test2</p>",
                                callback=function(value) {})
                })
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.2)
    expect_equal(kill(headlessPage),
                 "<html><head></head><body><p>test</p><p>test2</p></body></html>")
    closePage(headlessPage)
})

test_that("removeChild", {    
    headlessPage <- htmlPage(headless=TRUE)
    appendChild(headlessPage, "<p>test<p>")
    appendChild(headlessPage, "<p>test2<p>")
    removeChild(headlessPage, "p")
    expect_equal(kill(headlessPage),
                 "<html><head></head><body><p>test2</p></body></html>")
    closePage(headlessPage)

    headlessFile <- filePage(fileURL, headless=TRUE)
    removeChild(headlessFile, "h1")
    removeChild(headlessFile, "p")
    removeChild(headlessFile, "p")
    removeChild(headlessFile, "p")
    removeChild(headlessFile, "p")
    expect_equal(minifyHTML(kill(headlessFile)),
                 "<html><head></head><body></body></html>")
    closePage(headlessFile)

    headlessURL <- urlPage(url, headless=TRUE)
    removeChild(headlessURL, "h1")
    removeChild(headlessURL, "p")
    removeChild(headlessURL, "p")
    removeChild(headlessURL, "p")
    removeChild(headlessURL, "p")
    expect_equal(minifyHTML(kill(headlessURL)),
                 "<!DOCTYPEhtml><html><head></head><body></body></html>")
    closePage(headlessURL)
})
