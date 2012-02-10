
#' change first argument of a function
#' @param f the function
#' @param arg the arg to be called as the first argument
#' 
#' @return a function that calls \code{f} with \arg as the first argument.
#' @seealso \code{\link{wargs}},  \code{\link{dostats}}, and \code{\link{apply}}
#' @export
#' @examples
#' formals(runif)
#' onarg(runif, 'max')(1:10, 1)
#' onarg(runif, 'max')(1:10, 10)
#' #another version of contains
#' onarg(`%in%`, 'table')(letters, 'y')
onarg <- function(f, arg){
  carg <- as.character(substitute(arg))
  function(x, ...){
    do.call(f,args=append(structure(list(x), names=arg), list(...)))
  }
}

#' Does a table contain a value
#' @param table a table of values
#' @param y a value
#' @useage table \%contains\% y
#' 
#' @details
#' Literally %in% in reverse order, just for convenience.
#' 
#' @return a logical vector of the same length as \code{y} indicating if 
#'   \code{y} is in \code{table}, i.e. the \code{table} contains \code{y}.
#' @seealso \code{\link{%\in\%}}
#' @export
`%contains%` <- function(table,y){y %in% table}
