
library(DOM)
library(testthat)

source("utils.R")

context("opening (and closing) web pages")

test_that("page start (and stop) works", {    
    headlessPage <- htmlPage(headless=TRUE)
    expect_equal(kill(headlessPage),
                 "<html><head></head><body></body></html>")
    closePage(headlessPage)

    fileURL <- system.file("HTML", "RDOM.html", package="DOM")
    headlessFile <- filePage(fileURL, headless=TRUE)
    fileContents <- paste(readLines(fileURL), collapse="")
    expect_equal(minifyHTML(kill(headlessFile)),
                 minifyHTML(fileContents))
    closePage(headlessFile)

    url <- "http://pmur002.neocities.org/index.html"
    headlessURL <- urlPage(url, headless=TRUE)
    urlContents <- paste(readLines(url), collapse="")
    expect_equal(minifyHTML(kill(headlessURL)),
                 minifyHTML(urlContents))
    closePage(headlessURL)    
})

