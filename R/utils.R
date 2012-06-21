{###############################################################################
# utils.R
# Copyright 2012 Andrew Redd
# Date: 5/30/2012
# 
# DESCRIPTION
# ===========
# Convenience functions and utilities for use in dostats.
# 
# LICENSE
# ========
# dostats is free software: you can redistribute it and/or modify it under the
# terms of the GNU General Public License as published by the Free Software 
# Foundation, either version 3 of the License, or (at your option) any later 
# version.
# 
# dostats is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS 
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with 
# dostats. If not, see http://www.gnu.org/licenses/.
# 
}###############################################################################

first <- wargs(head, n=1)
last <- wargs(tail, n=1)

is_uniform <- function(x){
    all(laply(x, isTRUE %.% all.equal, x[[1]]))
}

#' List rows of a data frame in a list.
#' 
#' @param d a data.frame
#' @param keep.attr should the split attributes be kept.
#' 
#' @export
listrows <- function(d, keep.attr=FALSE){
    x <- mlply(d, data.frame)
    if(keep.attr) x
    else structure(x, split_type=NULL, split_labels=NULL)
}

#' Make a helper ID counter
#' 
#'  This creates an object that counts how many ID's have been created.
#'  
#'  The object is a list of three elements, each a function.
#'  \enumerate{
#'      \item \code{$new(n=1)} get the next \code{n} id numbers.  
#'      \item \code{$reset(at=0)} reset the the chain to \code{at}, 
#'                                next ID number will be \code{at+!}.
#'      \item \code{$curr()} what was the last ID number issued.
#'  }
#' 
#' @param startat The base number to start counting at, ie. th enumber before 
#'                the first ID. 
#' @export
make_new_id <- function(startat=0){
    ..next_id <- startat
    list(
    new = function(n=1){
        ..next_id <<- ..next_id +n
        return((..next_id-n+1):..next_id)
    },
    reset = function(at=0){..next_id <<- at},
    curr  = function(){..next_id}
    )
}

#' Make a call with extra arguments incorporated into call.
#'
#' Usefull for using with plyr functions
#'
#' @param args a list of arguments
#' @param ... extra arguments to be incorporated into args
#' @param what the function to execute
#' @param quote should the arguments be quoted
#' @param envir the environment to call the function in
#' 
#' @seealso \code{\link{do.call}} which this function wraps.
#' @export
make_call <- function(args, ..., what, quote = F, envir=parent.frame()){
    args <- append(args, list(...))
    do.call(what=what, args = args, quote=quote, envir=envir)
}

#' Return the current function
#' @seealso \code{\link{sys.function}}
#' @export
me<- function(){
    # print(sys.function(-2))
    i <- 0
    while(T){
        i <- i-1
        fun <- sys.function(i)
        if(identical(environment(fun), asNamespace("plyr"))) next
        if(identical(environment(fun), asNamespace("harvestr"))) next
        if(!identical(fun, me)) return(fun)
    }
}


#' Fill vector to length with a specified value
#' 
#' Extend a vector to the desired length by appending/inserting \code{with} 
#' at \code{after}, repeating \code{with} as necessary.
#' 
#' @param x a vector
#' @param l the length desired
#' @param with the item to append to achieve the desired length
#' @param after where to insert the items.
#' 
#' @export
fill_v <- function(x, l=length(x), with=last(x), after=length(x)){
    stopifnot(length(x) <= l)
    append(x, rep(with, l-length(x)), after=after)
}
