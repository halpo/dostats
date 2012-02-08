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
wargs <- function(f, ...){function(x)f(x,...)}

