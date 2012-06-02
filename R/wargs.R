wrap_function <- function(symb, args, envir){
#' @param symb symbolic name of function
#' @param args pairlist of arguments to use
#' @param envir the environment for the function
    newargs <- args
    for(a in setdiff(names(newargs), '...')) {
        newargs[[a]] <- as.name(a)
    }
    wdots <- names(newargs)=='...'
    newargs[wdots] <- list(as.symbol('...'))
    names(newargs)[wdots] <- ''
    c1 <- as.call(append(list(symb), as.list(newargs)))
    c2 <- as.call(c(as.name('{'), (c1)))
    as.function(append(args, list(c2)), envir=envir)
}

#'  Call with arguments
#'  @param f a function
#'  @param ... extra arguments
#' 
#'  @return a function that takes 1 argument and calls f with the 
#'  single argument and the additional \code{...} appended.
#'  @export
#'  @keywords utilities, misc
#'  @examples 
#'  mean2 <- wargs(mean, na.rm=T)
wargs <- function(f, ..., envir = parent.frame()){
    symb <- substitute(f)
    af   <- formals(f)
    args <- pairlist(...)
    new.args <- c(af[setdiff(names(af), names(args))], args)
    wrap_function(symb, new.args, envir)
}



#' create a function that redirects to the named function.
#' 
#' This is usefull for debugging to know what function has been called 
#' form within do.call or plyr functions.
#' 
#' @param f a function to wrap a call around
#' 
#' @export
redirf <- function(f, envir=parent.frame()){
    symb <- substitute(f)
    args <- formals(f)
    wrap_function(symb, args, envir)
}

