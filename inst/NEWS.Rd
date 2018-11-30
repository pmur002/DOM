\name{NEWS}
\title{NEWS file for the DOM package}
\encoding{UTF-8}

\section{Changes in version 0.6-0}{
  \itemize{
    \item New \code{client} argument for \code{htmlPage()} (and
    \code{filePage()} and \code{urlPage()}).  This replaces (and
    generalises) \code{headless} and \code{viewer} arguments, so that it
    is easier to add new clients to interact with DOM server, including
    remote clients.
    \item New \code{makeClient()} function for defining new clients.
    \item New \code{registerCallback()} function (so that callbacks do
    not have to pollute global workspace).
    \item Update \pkg{DOM} for changes in \pkg{httpuv},
    particularly to add \code{service} within
    \code{waitForResponse} so that
    client (browser) requests are handled if R is blocking (to
    wait for request to be handled).
    \item \code{htmlPage} can now serve
    files from \code{/assets} directory (so we can, for example,
    generate HTML with external images or fonts).
    \item Added \code{head} argument to \code{htmlPage} so that
    head element content can be supplied (as well as body element
    content).
    \item \code{htmlPage} with \code{browserClient} now
    only calls \code{RDOM.init} once the page has fully loaded.
  }
}
\section{Changes in version 0.5-1}{
  \itemize{
    \item Added support for \code{"string"}, \code{"number"},
    \code{"boolean"}, and \code{"JSON"} as valid response type in
    \code{RDOM.Rcall}.  This allows arbitrary values to be passed from
    javascript back to R function call.  
  }
}
\section{Changes in version 0.5}{
  \itemize{
    \item There is a copy of \code{RDOM.user.js} called
    \code{RDOM.tamper.js} for use with Tampermonkey.  This can be
    installed in a browser like Google Chrome (with the Tampermonkey
    extension installed) to allow \code{filePage()} and \code{urlPage()}
    to work in browsers other than Firefox.

    \item Added \code{viewer} argument to \code{htmlPage};  if this
    argument is \code{TRUE}, we will connect to and render in the
    RStudio Viewer Pane, if it is available (i.e., if we are running in
    RStudio).
  }
}
\section{Changes in version 0.4}{
  \itemize{
    \item New classes: \code{DOM_obj} (new root of class hierarchy,
    to handle wider range of objects than just DOM nodes, e.g., CSS
    stylesheets); \code{DOM_obj_ref} (umbrella for references to nodes
    and to other objects); \code{DOM_obj_ptr} (pointer to general
    object); \code{DOM_number}, \code{DOM_string}, and
    \code{DOM_boolean} (to represent basic javascript types).

    The existing \code{DOM_node} class extends \code{DOM_obj}.
    The existing \code{DOM_node_ref} class extends \code{DOM_obj_ref}.
    The existing \code{DOM_node_ptr} class extends \code{DOM_obj_ptr}.

    The \code{DOM_obj_ref} class adds a \code{pageID} slot so that we
    can check that a pointer is being sent to the page that created it.

    \item New functions \code{objPtr()}, \code{Number()},
    \code{String()}, and \code{Boolean()} for specifying request
    response type.

    \item New functions \code{getProperty()} and \code{setProperty()} to
    allow getting and setting properties on DOM objects.  These only
    work with \code{DOM_obj_ref} objects.

    By default, \code{getProperty()} selects an appropriate class for
    the response value.
    
    NOTE that DOM object properties are different from DOM node
    attributes (\code{setProperty()} is different from
    \code{setAttribute}).  Attributes refer to the attributes of HTML
    elements, whereas properties refer to the components of DOM
    objects. A good example of the difference is setting the
    \code{style} attribute of an element (which replaces the entire
    style attribute) versus setting a property of the \code{style}
    property of a DOM object (which only affects that single property).  

    \item Some syntactic sugar is provided so that we can get and set
    properties for \code{DOM_obj_ref} objects using syntax like
    \code{x$y} and \code{x$y <- value}.

    \item New \code{styleSheets()} function to get stylesheets from
    a page.  Supported by new \code{DOM_CSSStyleSheet_ptr} class.

    \item New \code{insertRule()} and \code{deleteRule()} functions
    for adding/removing CSS rules to/from a stylesheet.
    
    \item Support for modifying CSS rule style within stylesheet via new
    \code{DOM_CSSRule_ptr} class.

    \item New function \code{getAttribute()}.

    \item New function \code{removeProperty()} (to remove a CSS property
    from a \code{DOM_CSSStyleDeclaration_ptr} object).
    
    \item New function \code{propertyNames()} (to get the CSS property
    names from a \code{DOM_CSSStyleDeclaration_ptr} object).
    
    \item New \code{options("DOM.limit")} to control timeout length (in
    seconds) for requests.

    \item New hidden function \code{DOM:::render()} for taking snapshots
    of a headless page.  Size of the snapshot is controlled by new
    \code{options("DOM.width")} and \code{options("DOM.height")}.
    
    \item DOM error messages now contain more information.
  }
}
\section{Changes in version 0.3-1}{
  \itemize{
    \item Better recovery from failed startServer() when port already in
    use (now removes request -1 so can try again without having to
    restart R).
    
    \item Add (and export) a subset method for \code{DOM_node} objects (so
    that subsets are still \code{DOM_node} objects).
    
    \item Allow RDOM.Rcall() to specify null as responseType, in which
    case the R function is called with ZERO arguments.

    \item Documented \code{DOM:::debug()} and \code{DOM:::getPage()}
    functions and the \code{R_PHANTOMJSCMD} environment variable.
  }
}
\section{Changes in version 0.3}{
  \itemize{
    \item This version represents a major refactoring,
    with impact on almost all user-level functions.
    
    \item The results of DOM API functions are now S4 objects
    (e.g., \code{DOM_node_HTML} or \code{DOM_node_CSS})

    \item There are constructors for creating S4 objects representing
    DOM nodes (e.g., \code{htmlNode()} and \code{css()})
    
    \item Functions like \code{appendChild()} are now S4 generics and have
    fewer arguments (e.g., \code{child} and \code{childRef} becomes just
    \code{child}, with dispatch occurring on the class of child).

    \item Variations like \code{appendChildCSS()} disappear;
    \code{appendChild()} now has a \code{response} argument that
    dictates how the web browser response should be provided.

    \item Added new \code{createElement()} and \code{createElementNS()}
    functions. 
  }
}
