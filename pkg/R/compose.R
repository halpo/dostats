# Copyright Andrew Redd 2012
# Licensed under GPL v3 or newer

#' Nest functions
#' @author Andrew Redd
#' @aliases nest compose composition
#' @param ... functions to be nested together
#' @param .list alternatively an explicit list of functions.  
#'              If specified \code{...} will be ignored.
#' @return new function consisting of the functions nested
#' @export
#' @keywords utilities, misc
#' @examples
#' compose(any, is.na)(c(NA,1:3))
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