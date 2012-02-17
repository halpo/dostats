
#' compute an indicator to group consecutive values
#' @param x a vector
#' @param ... ignored, might be used for forward compatibility.
#' @return an integer vector.
#' @export
seq_consecutive <-
function(x,...){
  stopifnot(is.vector(x))
  y <- integer(length(x))
  y[1] <- 1
  if(length(x)>1)for(i in seq_len(length(x))[-1]){
    if(x[i]==x[i-1])
      y[i] <- y[i-1]
    else
      y[i] <- y[i-1] +1
  }
  return(y)
}
