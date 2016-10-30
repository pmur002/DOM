\name{NEWS}
\title{NEWS file for the DOM package}
\encoding{UTF-8}

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
