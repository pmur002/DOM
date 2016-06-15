
library(DOM)
library(testthat)

source("utils.R")

fileURL <- system.file("HTML", "RDOM.html", package="DOM")
url <- "http://pmur002.neocities.org/index.html"

test_that("appendChild", {
    # Append HTML child
    headlessPage <- htmlPage()
    appendChild(headlessPage, "<p>test<p>")
    pageContent <- closePage(headlessPage)
    expect_equal(pageContent,
                 "<html><head></head><body><p>test</p></body></html>")
    # Append CSS child
    headlessPage <- htmlPage()
    appendChild(headlessPage, "<p>test<p>")
    appendChild(headlessPage, "<p>test2<p>")
    appendChild(headlessPage, childRef="p")
    pageContent <- closePage(headlessPage)
    expect_equal(pageContent,
                 "<html><head></head><body><p>test2</p><p>test</p></body></html>")
    # Append HTML child and return CSS
    headlessPage <- htmlPage()
    result <- appendChildCSS(headlessPage, "<p>test<p>")
    closePage(headlessPage)
    expect_equal(result, "p")
    # Append HTML child in filePage()
    headlessFile <- filePage(fileURL)
    appendChild(headlessFile, "<p>test<p>")
    pageContent <- closePage(headlessFile)
    expect_match(minifyHTML(pageContent),
                 "<p>test</p></body></html>$")
    # Append HTML child in urlPage()
    headlessURL <- urlPage(url)
    appendChild(headlessURL, "<p>test<p>")
    pageContent <- closePage(headlessURL)
    expect_match(minifyHTML(pageContent),
                 "<p>test</p></body></html>$")
})

test_that("appendChild with callback", {    
    headlessPage <- htmlPage()
    result <- NULL
    appendChild(headlessPage, "<p>test</p>",
                callback=function(value) { result <<- value })
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.1)
    closePage(headlessPage)
    expect_equal(result, "<p>test</p>")
})

test_that("appendChild with callback with callback", {    
    headlessPage <- htmlPage()
    appendChild(headlessPage, "<p>test</p>",
                callback=function(value) {
                    appendChild(headlessPage, "<p>test2</p>",
                                callback=function(value) {})
                })
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.2)
    pageContent <- closePage(headlessPage)
    expect_equal(pageContent,
                 "<html><head></head><body><p>test</p><p>test2</p></body></html>")
})

test_that("removeChild", {
    # Remove child that exists (parent implicit)
    headlessPage <- htmlPage()
    appendChild(headlessPage, "<p>test<p>")
    appendChild(headlessPage, "<p>test2<p>")
    removeChild(headlessPage, "p")
    pageContent <- closePage(headlessPage)
    expect_equal(pageContent,
                 "<html><head></head><body><p>test2</p></body></html>")
    # Remove child that exists (parent implicit) and return CSS
    headlessPage <- htmlPage()
    appendChild(headlessPage, "<p>test<p>")
    appendChild(headlessPage, "<p>test2<p>")
    result <- removeChildCSS(headlessPage, "p")
    closePage(headlessPage)
    expect_equal(result, "body > :nth-child(1)")
    # Remove child that does not exist
    headlessPage <- htmlPage()
    appendChild(headlessPage, "<p>test<p>")
    result <- removeChild(headlessPage, "h1", tag="removeNonExistentChild")
    expect_equal(result, "Request removeNonExistentChild failed")
    pageContent <- closePage(headlessPage)
    # Remove child that does not match parent
    headlessPage <- htmlPage()
    appendChild(headlessPage, "<p>test<p>")
    appendChild(headlessPage, "<p>test2<p>")
    result <- removeChild(headlessPage, "p", parentRef="p",
                          tag="removeNotChildOfParent")
    expect_equal(result, "Request removeNotChildOfParent failed")
    pageContent <- closePage(headlessPage)
    # Remove children from filePage
    headlessFile <- filePage(fileURL)
    removeChild(headlessFile, "h1")
    removeChild(headlessFile, "p")
    removeChild(headlessFile, "p")
    removeChild(headlessFile, "p")
    removeChild(headlessFile, "p")
    pageContent <- closePage(headlessFile)
    expect_equal(minifyHTML(pageContent),
                 "<html><head></head><body></body></html>")
    # Remove children from urlPage
    headlessURL <- urlPage(url)
    removeChild(headlessURL, "h1")
    removeChild(headlessURL, "p")
    removeChild(headlessURL, "p")
    removeChild(headlessURL, "p")
    removeChild(headlessURL, "p")
    pageContent <- closePage(headlessURL)
    expect_equal(minifyHTML(pageContent),
                 "<html><head></head><body></body></html>")
})

test_that("replaceChild", {
    # Replace child that exists (parent implicit)
    headlessPage <- htmlPage()
    appendChild(headlessPage, "<p>test<p>")
    replaceChild(headlessPage, "<p>test2</p>", oldChildRef="p")
    pageContent <- closePage(headlessPage)
    expect_equal(pageContent,
                 "<html><head></head><body><p>test2</p></body></html>")
})
    
test_that("setAttribute", {
    headlessPage <- htmlPage()
    appendChild(headlessPage, "<p>test<p>")
    setAttribute(headlessPage, "p", "onclick", 'alert("test")')
    pageContent <- closePage(headlessPage)
    expect_equal(pageContent,
                 '<html><head></head><body><p onclick="alert(&quot;test&quot;)">test</p></body></html>')
})

test_that("click", {
    headlessPage <- htmlPage()
    appendChild(headlessPage, "<p>test<p>")
    setAttribute(headlessPage, "p", "onclick",
                 'this.setAttribute("style", "color: red")')
    click(headlessPage, "p")
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.2)
    pageContent <- closePage(headlessPage)
    expect_equal(pageContent,
                 '<html><head></head><body><p onclick="this.setAttribute(&quot;style&quot;, &quot;color: red&quot;)" style="color: red">test</p></body></html>')
})

test_that("Rcall", {
    # Call R from browser
    headlessPage <- htmlPage()
    element <- ""
    elementRef <- ""
    assign("recordRequest",
           function(target, targetRef) {
               element <<- target
               elementRef <<- targetRef
           },
           envir=.GlobalEnv)
    appendChild(headlessPage, "<p>test<p>")
    setAttribute(headlessPage, "p", "onclick",
                 'RDOM.Rcall("recordRequest", this, null)')
    click(headlessPage, "p")
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.2)
    closePage(headlessPage)
    expect_equal(element,
                 '<p onclick="RDOM.Rcall(&quot;recordRequest&quot;, this, null)">test</p>')
    expect_equal(elementRef, "p")
    # Call R from browser, then call browser from R
    headlessPage <- htmlPage()
    callbackGen <- function(page) {
        function(target, targetRef) {
            require(xtable)
            require(XML)
            text <- xmlValue(xmlRoot(xmlParse(target)))
            wordCount <- table(strsplit(gsub("\n", "", gsub("  +", " ", text)),
                                        " ")[[1]])
            wordTab <- print(xtable(wordCount), type="html",
                             print.results=FALSE, comment=FALSE)
            replaceChild(page, newChild=wordTab, oldChildRef=targetRef,
                         callback=function(value) {})
        }
    }
    assign("replaceWithTable", callbackGen(headlessPage), envir=.GlobalEnv)
    appendChild(headlessPage, "<p>test<p>")
    setAttribute(headlessPage, "p", "onclick",
                 'RDOM.Rcall("replaceWithTable", this, null)')
    click(headlessPage, "p")
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.5)
    pageContent <- closePage(headlessPage)
    expect_equal(minifyHTML(pageContent),
                 '<html><head></head><body><tableborder="1"><tbody><tr><th></th><th>V1</th></tr><tr><tdalign="right">test</td><tdalign="right">1</td></tr></tbody></table></body></html>')
})
    
