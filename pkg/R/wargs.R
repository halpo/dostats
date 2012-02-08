#'  Call with arguments
#' 
#'  @return a function that takes 1 argument and calls f with the 
#'  single argument and the additional \code{...} appended.
#'  @keywords utilities, misc
#'  @examples 
#'  mean2 <- wargs(mean, na.rm=T)
wargs <- function(f, ...){function(x)f(x,...)}

