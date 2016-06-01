
library(DOM)
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

test_that("removeChild works", {    
    headlessPage <- htmlPage(headless=TRUE)
    appendChild(headlessPage, "<p>test<p>")
    appendChild(headlessPage, "<p>test2<p>")
    removeChild(headlessPage, "p")
    expect_equal(kill(headlessPage),
                 "<html><head></head><body><p>test2</p></body></html>")
    closePage(headlessPage)
})
