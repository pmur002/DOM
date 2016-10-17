\name{NEWS}
\title{NEWS file for the DOM package}
\encoding{UTF-8}

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
  }
}
