
## Functions to register callbacks for use with the
## RDOM.Rcall() javascript function

## (so that callbacks do NOT have to live in global workspace!)

callbacks <- new.env(parent=.GlobalEnv)

registerCallback <- function(f, name=deparse(substitute(f))) {
    assign(name, f, envir=callbacks)
}

getRegisteredCallback <- function(name) {
    get(name, envir=callbacks)
}
    
