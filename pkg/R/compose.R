# Copyright Andrew Redd 2012
# Licensed under GPL v3 or newer

#' Nest functions
#' @author Andrew Redd
#' @aliases nest compose composition
#' @param ... functions to be nested together
#' @param .list alternatively an explicit list of functions.  
#'              If specified \code{...} will be ignored.
#' @details
#' compose creates a functional composition of the listed functions.
#' Functional composition of functions f and g is defined as f(g(.)).
#' Order matters the right most function listed will be the innermost 
#' function in the composition, same with the operator version.
#' To remember the order lists will be the order read out, ie. 
#' compose(f,g) = f(g(x))
#'
#' When using the operator version it is good to remember that parentheses 
#' are recommended see the examples
#'
#' @return new function consisting of the functions nested
#' @export
#' @keywords utilities, misc
#' @examples
#' compose(any, is.na)(c(NA,1:3))
#' (sum%.%is.na)(c(1,NA))  #correct
#' \dontrun{
#' sum%.%is.an(NA)  #incorrect
#' }
compose <- function(..., .list){
  l <- if(missing(.list)) {
    list(...)
  } else {
    .list
  }
  body <- as.name('...')
  for(i in rev(l)){
    body <- as.call(list(i,body))
  }
  as.function(append(alist(...=), body))
}

#' @rdname compose
#' @usage x \%.\% y
#' @param x a function
#' @param y a function
#' @export
`%.%` <- function(x,y){
  compose(.list=list(x,y))
}