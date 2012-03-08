
#' create a text vector
#' @rdname T
#' @param ... names, quoted or not, no substitution made
#' @export
#' @examples
#' .T(min, mean, 'median')
.T <-
function(...){
  as.character(substitute(c(...)))[-1]
}


