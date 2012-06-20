

#' Conditional Apply
#' 
#' A wrapper for \code{ifelse(test(x), fun(x, ...), x)}
#' 
#' @param test a test that returns a logical 
#' @param fun to apply
#' @param x data to apply fun to.
#' 
#' 
capply <- function(test, x, fun, ...){
    ifelse(Map(test, x), fun(x, ...), x)
}
