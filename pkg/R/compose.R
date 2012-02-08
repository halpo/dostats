# Copyright Andrew Redd 2012
# Licensed under GPLv3 or newer

#' Nest functions
#' @author Andrew Redd
#' @aliases nest compose composition
#' @param ... functions to be nested together
#' @return new function cosisting of the functions nested
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
#' @param x a function
#' @param y a function
`%.%` <- function(x,y){
  compose(.list=list(x,y))
}