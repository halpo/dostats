#' Convenient interface for computing statistics on a vector
#'  @author Andrew Redd
#'  @importFrom plyr llply
#'
#'  @param x the vector
#'  @param ... statistics to compute, must take a vector and return a vector
#'  @param .na.action the action to take on NA values, for all statistics
#'
#'  @return A one row \code{data.frame} with columns named as in \code{...}
#'  @seealso \code{\link[plyr]{ldply}}
#'  @keywords utilities, misc
#'  @example inst/ex_dostats.R
dostats <- function(x, ..., .na.action=na.fail){
  if(any(is.na(x)))
    x <- .na.action(x)
  funs   <- list(...)
  fnames <- names(funs)
  inames <- as.character(substitute(c(...)))[-1]
  fnames <- if(is.null(fnames)) inames else ifelse(fnames != "", fnames, inames)
  l <- structure(llply(funs, do.call, list(x)), names=fnames)
  l <- llply(l, function(y)if(length(y)==1) y else t(y))
  do.call(data.frame, l)
}

#' Filter by class
#' @param x vector of any class
#' @param .class string for class to filter by
#' @param ... passed to \code{\link{dostats}}
#' @return data frame of computed statistics if x is of class \code{.class}
#'         otherwise returns \code{NULL}.
#' @sealso \code{\link{dostats}}
class.stats <- function(.class){
  if(class(.class)!="character")
    .class=as.character(substitute(.class))
  function(x, ...){if(inherits(x, .class))
    dostats(x, ...)
  else NULL
  }
}
numeric.stats <- class.stats(numeric)
factor.stats  <- class.stats(factor)
integer.stats <- class.stats(integer)