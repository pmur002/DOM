
library(DOM)
library(testthat)

source("utils.R")

fileURL <- system.file("HTML", "RDOM.html", package="DOM")
url <- "http://pmur002.neocities.org/index.html"

test_that("createElement", {
    # Create HTML
    page <- htmlPage()
    html <- createElement(page, "p", htmlNode())
    closePage(page)
    expect_equal(unclass(html),
                 "<p></p>")
    # Create nodePtr and build "off-screen"
    page <- htmlPage()
    ptr <- createElement(page, "div")
    appendChild(page, htmlNode("<p>test</p>"), ptr)
    appendChild(page, ptr)
    pageContent <- closePage(page)
    expect_equal(unclass(pageContent),
                 "<html><head></head><body><div><p>test</p></div></body></html>")
    # Check that asking (stupidly) for CSS or XPath result does not
    # fail messily
    page <- htmlPage()
    createElement(page, "p", css())
    createElement(page, "p", xpath())
    closePage(page)
    # createElementNS
    page <- htmlPage()
    ptr <- createElementNS(page, "http://www.w3.org/2000/svg", "svg")
    setAttribute(page, ptr, "width", "100")
    setAttribute(page, ptr, "height", "100")
    appendChild(page, svgNode('<circle xmlns="http://www.w3.org/2000/svg" cx="50" cy="50" r="50"/>'), parent=ptr, ns=TRUE, response=svgNode())
    appendChild(page, ptr, ns=TRUE, response=svgNode())
    pageContent <- closePage(page)
    expect_equal(unclass(pageContent),
                 '<html><head></head><body><svg xmlns="http://www.w3.org/2000/svg" width="100" height="100"><circle xmlns="http://www.w3.org/2000/svg" cx="50" cy="50" r="50"></circle></svg></body></html>')
})
    
test_that("appendChild", {
    # Append HTML child
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 "<html><head></head><body><p>test</p></body></html>")
    # Append CSS child
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    appendChild(headlessPage, htmlNode("<p>test2<p>"))
    appendChild(headlessPage, css("p"))
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 "<html><head></head><body><p>test2</p><p>test</p></body></html>")
    # Append HTML child and return CSS
    headlessPage <- htmlPage()
    result <- appendChild(headlessPage, htmlNode("<p>test<p>"), response=css())
    closePage(headlessPage)
    expect_equal(as.character(result), "p")
    # Append HTML child in filePage()
    headlessFile <- filePage(fileURL)
    appendChild(headlessFile, htmlNode("<p>test<p>"))
    pageContent <- closePage(headlessFile)
    expect_match(minifyHTML(pageContent),
                 "<p>test</p></body></html>$")
    # Append HTML child in urlPage()
    headlessURL <- urlPage(url)
    appendChild(headlessURL, htmlNode("<p>test<p>"))
    pageContent <- closePage(headlessURL)
    expect_match(minifyHTML(pageContent),
                 "<p>test</p></body></html>$")
    # Append SVG within HTML
    headlessPage <- htmlPage()
    appendChild(headlessPage,
                svgNode('<svg xmlns="http://www.w3.org/2000/svg"><circle/></svg>'),
                ns=TRUE)
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 '<html><head></head><body><svg xmlns="http://www.w3.org/2000/svg"><circle></circle></svg></body></html>')
    # Append HTML within SVG within HTML
    headlessPage <- htmlPage()
    appendChild(headlessPage,
                svgNode('<svg xmlns="http://www.w3.org/2000/svg"><foreignObject id="fo"></foreignObject></svg>'),
                ns=TRUE)
    appendChild(headlessPage,
                htmlNode('<p xmlns="http://www.w3.org/1999/xhtml">test</p>'),
                ns=TRUE,
                parent=css("#fo"))
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 '<html><head></head><body><svg xmlns="http://www.w3.org/2000/svg"><foreignObject id="fo"><p xmlns="http://www.w3.org/1999/xhtml">test</p></foreignObject></svg></body></html>')
    # Append JavaScript
    headlessPage <- htmlPage()
    appendChild(headlessPage,
                javascript('document.body.appendChild(document.createElement("p"));'))
    pageContent <- closePage(headlessPage)
    ## Not just the <script>, but also the <p> that the script creates
    ## (to show that the script has been added AND run)
    expect_equal(unclass(pageContent),
                 '<html><head></head><body><script>document.body.appendChild(document.createElement("p"));</script><p></p></body></html>')    
})

test_that("appendChild with callback", {    
    headlessPage <- htmlPage()
    result <- NULL
    appendChild(headlessPage, htmlNode("<p>test</p>"),
                callback=function(value) { result <<- value })
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.1)
    closePage(headlessPage)
    expect_equal(unclass(result), "<p>test</p>")
})

test_that("appendChild with callback with appendChild", {    
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode("<p>test</p>"),
                callback=function(value) {
                    appendChild(headlessPage, htmlNode("<p>test2</p>"),
                                async=TRUE)
                })
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.2)
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 "<html><head></head><body><p>test</p><p>test2</p></body></html>")
})

test_that("removeChild", {
    # Remove child that exists (parent implicit)
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    appendChild(headlessPage, htmlNode("<p>test2<p>"))
    removeChild(headlessPage, css("p"))
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 "<html><head></head><body><p>test2</p></body></html>")
    # Remove child that exists (parent implicit) and return CSS
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    appendChild(headlessPage, htmlNode("<p>test2<p>"))
    result <- removeChild(headlessPage, css("p"), response=css())
    closePage(headlessPage)
    expect_equal(as.character(result), "body > :nth-child(1)")
    # Remove child that does not exist
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    result <- removeChild(headlessPage, css("h1"), tag="removeNonExistentChild")
    expect_match(unclass(result), "Request removeNonExistentChild failed:")
    pageContent <- closePage(headlessPage)
    # Remove child that does not match parent
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    appendChild(headlessPage, htmlNode("<p>test2<p>"))
    result <- removeChild(headlessPage, css("p"), parent=css("p"),
                          tag="removeNotChildOfParent")
    expect_match(unclass(result), "Request removeNotChildOfParent failed:")
    pageContent <- closePage(headlessPage)
    # Remove children from filePage
    headlessFile <- filePage(fileURL)
    removeChild(headlessFile, css("h1"))
    removeChild(headlessFile, css("p"))
    removeChild(headlessFile, css("p"))
    removeChild(headlessFile, css("p"))
    removeChild(headlessFile, css("p"))
    pageContent <- closePage(headlessFile)
    expect_equal(minifyHTML(pageContent),
                 "<html><head></head><body></body></html>")
    # Remove children from urlPage
    headlessURL <- urlPage(url)
    removeChild(headlessURL, css("h1"))
    removeChild(headlessURL, css("p"))
    removeChild(headlessURL, css("p"))
    removeChild(headlessURL, css("p"))
    removeChild(headlessURL, css("p"))
    pageContent <- closePage(headlessURL)
    expect_equal(minifyHTML(pageContent),
                 "<html><head></head><body></body></html>")
})

test_that("replaceChild", {
    # Replace child that exists (parent implicit)
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    replaceChild(headlessPage, htmlNode("<p>test2</p>"), oldChild=css("p"))
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 "<html><head></head><body><p>test2</p></body></html>")
})
    
test_that("setAttribute", {
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    setAttribute(headlessPage, css("p"), "onclick", 'alert("test")')
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 '<html><head></head><body><p onclick="alert(&quot;test&quot;)">test</p></body></html>')
})

test_that("getElementById", {
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode('<p id="x">test</p>'))
    elt <- getElementById(headlessPage, "x")
    css <- getElementById(headlessPage, "x", response=css())
    # id does not exist
    missing <- getElementById(headlessPage, "y")
    closePage(headlessPage)
    expect_equal(unclass(elt), '<p id="x">test</p>')
    expect_equal(as.character(css), '#x')
    expect_equal(unclass(missing), character())
})

test_that("getElementsByTagName", {
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode('<p>p1</p>'))
    appendChild(headlessPage, htmlNode('<p>p2</p>'))
    elts <- getElementsByTagName(headlessPage, "p")
    css <- getElementsByTagName(headlessPage, "p", response=css())
    ## test '*' special
    all <- getElementsByTagName(headlessPage, "*")
    ## tag does not exist
    missing <- getElementsByTagName(headlessPage, "table")
    closePage(headlessPage)
    expect_equal(unclass(elts), c("<p>p1</p>", "<p>p2</p>"))
    expect_equal(as.character(css),
                 c("body > :nth-child(1)", "body > :nth-child(2)"))
    expect_equal(unclass(all),
                 c("<html><head></head><body><p>p1</p><p>p2</p></body></html>",
                   "<head></head>",
                   "<body><p>p1</p><p>p2</p></body>",
                   "<p>p1</p>",
                   "<p>p2</p>"))
    expect_equal(unclass(missing), character())
})

test_that("getElementsByClassName", {
    ## test default document root
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode('<p class="c1">p1<p>'))
    appendChild(headlessPage, htmlNode('<div></div>'))
    appendChild(headlessPage, htmlNode('<p class="c1 c2">p2<p>'),
                parent=css("div"))
    elts <- getElementsByClassName(headlessPage, "c1")
    css <- getElementsByClassName(headlessPage, "c1", response=css())
    elt <- getElementsByClassName(headlessPage, "c1 c2")
    ## test non-document root
    nrelt <- getElementsByClassName(headlessPage, "c1", root=css("div"))
    ## tag does not exist
    missing <- getElementsByClassName(headlessPage, "c3")
    closePage(headlessPage)
    expect_equal(unclass(elts),
                 c("<p class=\"c1\">p1</p>", "<p class=\"c1 c2\">p2</p>"))
    expect_equal(as.character(css), c("body > :nth-child(1)", "div > .c1"))
    expect_equal(unclass(elt), "<p class=\"c1 c2\">p2</p>")
    expect_equal(unclass(nrelt), "<p class=\"c1 c2\">p2</p>")
    expect_equal(unclass(missing), character())
})

test_that("click", {
    headlessPage <- htmlPage()
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    setAttribute(headlessPage, css("p"), "onclick",
                 'this.setAttribute("style", "color: red")')
    click(headlessPage, css("p"))
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.2)
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 '<html><head></head><body><p onclick="this.setAttribute(&quot;style&quot;, &quot;color: red&quot;)" style="color: red">test</p></body></html>')
})

test_that("Rcall", {
    # Call R from browser
    headlessPage <- htmlPage()
    element <- ""
    elementCSS <- ""
    assign("recordRequest",
           function(target, targetCSS) {
               element <<- target
               elementCSS <<- targetCSS
           },
           envir=.GlobalEnv)
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    call <- 'RDOM.Rcall("recordRequest", this, [ "HTML", "CSS" ], null)'
    setAttribute(headlessPage, css("p"), "onclick", call)
    click(headlessPage, css("p"))
    # Call is asynchronous, so pause for it to finish
    Sys.sleep(.2)
    closePage(headlessPage)
    model <- '<p onclick="RDOM.Rcall(&quot;recordRequest&quot;, this, [ &quot;HTML&quot;, &quot;CSS&quot; ], null)">test</p>'
    expect_equal(unclass(element), model) 
    expect_equal(as.character(elementCSS), "p")
    # Call R from browser (no arguments)
    headlessPage <- htmlPage()
    testResult <- FALSE
    assign("noArgs", function() { testResult <<- TRUE }, envir=.GlobalEnv)
    noArgs <- 
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    setAttribute(headlessPage, css("p"), "onclick",
                 'RDOM.Rcall("noArgs", this, null, null)')
    click(headlessPage, css("p"))
    Sys.sleep(.2)
    closePage(headlessPage)
    expect_true(testResult)
    # Call R from browser (multiple objects, multiple formats)
    headlessPage <- htmlPage()
    testResult <- FALSE
    assign("manyArgs", function(...) { testResult <<- list(...) },
           envir=.GlobalEnv)
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    call <- 'RDOM.Rcall("manyArgs", [ this, this.parentNode ],
                         [ "HTML", "CSS" ], null)'
    setAttribute(headlessPage, css("p"), "onclick", call)
    click(headlessPage, css("p"))
    Sys.sleep(.2)
    closePage(headlessPage)
    expect_length(testResult, 4)
    expect_s4_class(testResult[[1]], "DOM_node_HTML")
    expect_s4_class(testResult[[2]], "DOM_node_CSS")
    expect_s4_class(testResult[[3]], "DOM_node_HTML")
    expect_s4_class(testResult[[4]], "DOM_node_CSS")
    # Call R from browser, then call browser from R
    headlessPage <- htmlPage()
    callbackGen <- function(page) {
        function(target, targetCSS) {
            require(xtable)
            require(XML)
            text <- xmlValue(xmlRoot(xmlParse(target)))
            wordCount <- table(strsplit(gsub("\n", "", gsub("  +", " ", text)),
                                        " ")[[1]])
            wordTab <- print(xtable(wordCount), type="html",
                             print.results=FALSE, comment=FALSE)
            replaceChild(page,
                         newChild=htmlNode(wordTab),
                         oldChild=css(targetCSS),
                         async=TRUE)
        }
    }
    assign("replaceWithTable", callbackGen(headlessPage), envir=.GlobalEnv)
    appendChild(headlessPage, htmlNode("<p>test<p>"))
    call <- 'RDOM.Rcall("replaceWithTable", this, [ "HTML", "CSS" ], null)'
    setAttribute(headlessPage, css("p"), "onclick", call)
    click(headlessPage, css("p"))
    Sys.sleep(.5)
    pageContent <- closePage(headlessPage)
    expect_equal(minifyHTML(pageContent),
                 '<html><head></head><body><tableborder="1"><tbody><tr><th></th><th>V1</th></tr><tr><tdalign="right">test</td><tdalign="right">1</td></tr></tbody></table></body></html>')
})
    
test_that("nodePtr", {
    headlessPage <- htmlPage()
    # Create new node and get DOM_node_ptr to it
    ptr <- appendChild(headlessPage, htmlNode("<p>test<p>"),
                       response=nodePtr())
    # Use DOM_node_ptr to specify a node 
    setAttribute(headlessPage, ptr, "style", "color: red")
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 '<html><head></head><body><p style="color: red">test</p></body></html>')
    
    ## Check that DOM_node_ptr does not change on reuse
    page <- htmlPage('<p id="p1">test</p>')
    p1 <- getElementById(page, "p1", response=nodePtr())
    p2 <- getElementById(page, "p1", response=nodePtr())
    expect_equal(p1, p2)
    closePage(page)
})

test_that("XPath", {
    headlessPage <- htmlPage()
    # Create new node and get DOM_node_ptr to it
    xpath <- appendChild(headlessPage, htmlNode("<p>test<p>"),
                         response=xpath())
    # Use DOM_node_ptr to specify a node 
    setAttribute(headlessPage, xpath, "style", "color: red")
    pageContent <- closePage(headlessPage)
    expect_equal(unclass(pageContent),
                 '<html><head></head><body><p style="color: red">test</p></body></html>')
})

test_that("properties", {
    page <- htmlPage()
    p <- createElement(page, "p")
    # Get DOM_obj_ptr using DOM_node_ptr
    style <- getProperty(page, p, "style")
    # Set property using DOM_obj_ptr, with DOM_value 
    setProperty(page, style, "color", "red")
    # Get property using DOM_obj_ptr, as R vector
    colour <- getProperty(page, style, "color")
    # Syntactic sugar short-hand
    p$style$color <- "green"
    sugarColour <- p$style$color
    expect_equal(colour, "red")
    expect_equal(sugarColour, "green")
    closePage(page)
    
    # Show difference between setAttribute() and setProperty()
    page <- htmlPage('<p style="color: red; font-style: italic">test</p>')
    # Change colour property
    style <- getProperty(page, css("p"), "style")
    style$color <- "green"
    # Should only affect colour NOT font
    newPropCol <- style$color
    newPropFontStyle <- style$"font-style"
    expect_equal(newPropCol, "green")
    expect_equal(newPropFontStyle, "italic")    
    # Change style attribute
    setAttribute(page, css("p"), "style", "color: green")
    # Should affect BOTH colour and font
    style <- getProperty(page, css("p"), "style")
    newAttrCol <- style$color
    newAttrFontStyle <- style$"font-style"
    expect_equal(newAttrCol, "green")
    expect_equal(newAttrFontStyle, "")
    closePage(page)

    # Use DOM_node_CSS as the element in short-hand
    page <- htmlPage('<p id="p1" style="color: red; font-style: italic">test</p>')
    p <- getElementById(page, "p1", response=css())
    col <- p$style$color
    expect_equal(col, "red")
    closePage(page)

    ## Test warning from trying to set readonly property
    page <- htmlPage('<p id="p1" style="color: red; font-style: italic">test</p>')
    p <- getElementById(page, "p1", response=css())
    style <- getProperty(page, p, "style")
    expect_warning(setProperty(page, p, "style", style), "Read-only property")
    closePage(page)
})
