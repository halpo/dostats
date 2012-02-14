
#' collect results
#' @param v a vector, list, array, etc.
#' @param f a function to collect on
#' @param ... passed to f
#' 
#' @details
#' Collect results by resursively calling the elements of the vector \code{v}.
#' The first two elements are called as \code{fun(v[1], v[2],...)}  The result is x.
#' Then f(x, v[3]) is called and so forth, until all elements has been exhausted.
#' 
#' as such \code{fun} must take two arguments and return a single element, although
#' there are no restrictions on what that single thing might be.
#' 
#' @export
#' @examples
#' collect(v=letters, fun=function(x,y,...)paste(y,x, ...), sep='/')
collect <- function(v, fun, ...){
  len <- length(v)
  if(len < 2)
    stop("vector v not long enough.")
  x <- fun(v[[1]], v[[2]], ...)
  if(len>2) for(i in seq(from=3, to=len, by=1))
    x <- fun(x, v[[i]], ...)
  x
}


