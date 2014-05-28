{###############################################################################
# hdf.R
# This file is part of the R package dostats.
# 
# Copyright 2012 Andrew Redd
# Date: 6/1/2012
# 
#' @include wargs.R 
# 
# DESCRIPTION
# ===========
# Functions for handling Heirarchical Data Frames 
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
# Examples:
# =========
# Mean, sd and t.test for f2 within f1
# ..numeric ~ F1 * F2/t.test ~ mean + sd
# 
# cross-tabluations of variable by f2 within f1
# ..factor ~ f1 * f2/table
# 
# number of levels and number of missing
#
}###############################################################################

#' Heirachical Data Frames
#' 
#' The primary purpose of the hdf is to represent data with grouped columns
#' and grouped rows.
#' 
#' @param ... vector, data.frame, or hdf objects to bind together.
#' 
#' @export
hdf <- function(...) {
    l <- list(...)
    if(length(l)==1 && inherits(l[[1]], 'hdf')) return(l[[1]])
    rn <- Filter(Negate(is.null), llply(l, rownames))
    rnames <- if(length(rn)==0) {
        seq_len(NROW(l[[1]]))
    } else if (length(rn) > 1) {
        if(!all(laply(rn, all.equal, rn[[1]]))) {
            warning("Incompatible row names")
        } 
        rn[[1]]
    } else { # length(rn ==1)
        rn[[1]]
    }
    structure(l, class = c('hdf', 'data.frame', 'list'), row.names = rnames)
}

{## hdf - Heirachical Data Frame
center_string <- function(x, width= max(nchar(x))){
    sprintf("%*s%*s"
           , ceiling((width + nchar(x))/2), x
           , floor((width - nchar(x))/2), '')
}
#' @export
print.hdf <- function(x, ...) {
    
}


}## hdf - Heirachical Data Frame
{## find_headers
.empty_headers <- {data.frame(name = character(0), id=integer(0)
                    , terminal=logical(0), depth=integer(0)
                    , parent=integer(0), position=integer(0))}
find_headers <- function(x, ..., id_maker = make_new_id(), depth=0, parent=0){
    UseMethod('find_headers')
}
find_headers.default <- 
function(x, ..., id_maker = make_new_id(), depth=0, parent=0){.empty_headers}
find_headers.hdf <- function(x, ..., id_maker = make_new_id(), depth=0, parent=0){
    # x = args[[1]]
    # x = args[[1]][[2]]
    df1 <- data.frame(name = names(x), id = id_maker$new(length(x))
                     , terminal = NA
                     , depth = depth, parent=parent
                     , position = seq_along(x)
                     , stringsAsFactors=F)
    
    call.list <- Map(list, x=x, parent = df1$id)
       
    l<- llply(call.list, make_call, what=find_headers
             , depth = depth+1, id_maker=id_maker)
    df1$terminal <- laply(l, NROW)==0
    Reduce(rbind, l, init=df1)
}
find_headers.data.frame <- {
function(x, ..., id_maker = make_new_id(), depth=0, parent=0){
    data.frame(name = names(x), id = id_maker$new(length(x))
              , terminal=T
              , depth = depth, parent=parent
              , position = seq_along(x)
              , stringsAsFactors=F)
}}
find_headers.list <- find_headers.hdf
trace_parents <- function(header, id){
    if(length(id)>1) return(llply(id, me(), header=header))
    if(id==0) return(NULL)
    row <- header[header$id==id, ]
    stopifnot(nrow(row)==1)
    if(row$parent == 0) return(0)
    c(row$parent, Recall(header, row$parent))
}

# flatten_headers <- function(df, level.sep="|")
}## find_headers
{## format
#' @export
format.hdf <- function(x, ...){
    contents <- llply(x, format)
    
}
}## format
{## Conversion
# as.data.frame.hdf <- function(x){
    # reducer <- function(y) {if (inherits(y, 'hdf')) {
        # as.data.frame.hdf(y)
    # } else y}
    # x <- llply(x, reducer)
    # Reduce(data.frame, x)
# }
}## Conversion
{## Utilities
make_pmat <- function(par, id=NULL){
    #' make matrix of parentage.
    #' @param par parent list from trace_parents
    a <- Map(c, id, par)
    b <- Map(fill_v, a, l=max(laply(a, length)))
    make_call(what=rbind, b)
}
recombine_idx <- function(pmat){
    #' @param pmat pmat from make_pmat
    stopifnot(inherits(pmat, 'matrix'))
    s <- seq_consecutive(pmat[,1])
    if(ncol(pmat)==1) return(list(s))
    p2 <- unique(pmat)[, -1, drop=F]
    append(list(s), Recall(p2))
}
check_ri <- function(ri) {
    #' @param ri list of recombine indices from recombine_idx
    stopifnot(isTRUE(all.equal(
        head(sapply(ri, max)  , -1)
      ,      sapply(ri, length)[-1])))
    stopifnot(is_uniform(last(ri)))
    ri
}
recombine.hdf <- function(x, ri){
    f <- wargs(tapply, FUN=wargs(make_call, what=redirf(hdf)))
    Reduce(f, check_ri(ri), init=x)[[1]]
    
    # x <- 
    # tapply(x, ri[[1]], make_call, what=hdf)
    # if(length(ri)<=1) return(x)
    # Recall(x, ri[-1])
}
make_ppath <- function(id, header) {
    if(length(id) > 1) return(llply(id, make_ppath, header))
    par <- trace_parents(header, id)
    rev(header[c(id, par), 'position'])
}
copy_names <- function(x, header) {
    by.parent <- dlply(header, 'parent', I)
    for(d in by.parent){
        curr.parent = d$parent[1]
        path <- make_ppath(curr.parent, header)
        if(is.na(path[[1]])){
            names(x) <- d$name
        }else {
            names(x[[path]]) <- d$name
        }
    }
    x
}

#' @export
rbind.hdf <- function(..., check.headers=T) {
    # args <- evaluated
    args <- list(...)
    if(length(args==1) && inherits(args[[1]], 'hdf')) return(args[[1]])
    stopifnot(length(args)>=1)
    header <- find_headers(args[[1]])
    
    if(check.headers) {
        headers <- llply(args, find_headers)
        passed.check.headers <- laply(laply(headers, all.equal, header), isTRUE)
        if(!all(passed.check.headers))
            stop("Not all headers match for all elements; cannot rbind.")
    }
    {## extract paths
        leaves <- subset(header, header$terminal==T)
        par   <- trace_parents(header, leaves$id)
        ppath <- make_ppath(leaves$id, header)
    }
    vectors <- llply(ppath, function(path){
        elements <- llply(args, Reduce, f=`[[`, x=path)
        make_call(elements, what=c)
    })
    pmat <- make_pmat(par, leaves$id)
    ri = recombine_idx(pmat)
    ri = unique(ri)
    raw.new <- recombine.hdf(vectors, ri[-1])
    new <- copy_names(raw.new, header)
    new
}

#' @export
cbind.hdf <- redirf(hdf)

}## Utilities

