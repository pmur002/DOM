
library(DOM)
library(testthat)

source("utils.R")

context("opening (and closing) web pages")

test_that("page start (and stop) works", {    
    headlessPage <- htmlPage()
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 "<html><head></head><body></body></html>")

    fileURL <- system.file("HTML", "RDOM.html", package="DOM")
    headlessFile <- filePage(fileURL)
    fileContents <- paste(readLines(fileURL), collapse="")
    pageContent <- closePage(headlessFile)
    expect_equal(minifyHTML(pageContent),
                 minifyHTML(fileContents))

    url <- "http://pmur002.neocities.org/index.html"
    headlessURL <- urlPage(url)
    urlContents <- paste(readLines(url), collapse="")
    pageContent <- closePage(headlessURL)
    expect_equal(minifyHTML(pageContent),
                 gsub("<!DOCTYPEhtml>", "", minifyHTML(urlContents)))
})

