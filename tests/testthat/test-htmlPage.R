
library(DOM)
library(testthat)
context("htmlPage")

test_that("htmlPage works", {    
    headlessPage <- htmlPage(headless=TRUE)
    expect_equal(kill(headlessPage),
                 "<html><head></head><body></body></html>")
    closePage(headlessPage)
})

test_that("appendChild works", {    
    headlessPage <- htmlPage(headless=TRUE)
    appendChild(headlessPage, "<p>test<p>")
    expect_equal(kill(headlessPage),
                 "<html><head></head><body><p>test</p></body></html>")
    closePage(headlessPage)
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
    result <- NULL
    appendChild(headlessPage, "<p>test</p>",
                callback=function(value) {
                    appendChild(headlessPage, "<p>test2</p>")
                })
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.2)
    expect_equal(kill(headlessPage),
                 "<html><head></head><body><p>test</p><p>test2</p></body></html>")
    closePage(headlessPage)
})

test_that("removeChild works", {    
    headlessPage <- htmlPage(headless=TRUE)
    appendChild(headlessPage, "<p>test<p>")
    appendChild(headlessPage, "<p>test2<p>")
    removeChild(headlessPage, "p")
    expect_equal(kill(headlessPage),
                 "<html><head></head><body><p>test2</p></body></html>")
    closePage(headlessPage)
})
