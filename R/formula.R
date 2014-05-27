{###############################################################################
# utils.R
# Copyright 2012 Andrew Redd
# Date: 6/1/2012
#
#! @include wargs.R
#
# DESCRIPTION
# ===========
# Table1 function for fomulaic specification of tables.
# Functions for formula parsing 
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
# Examples
# ========
# Mean, sd and t.test for f2 within f1
# ..numeric ~ F1 * F2/t.test ~ mean + sd
# 
# cross-tabluations of variable by f2 within f1
# ..factor ~ f1 * f2/table
# 
# number of levels and number of missing
#
}###############################################################################
{## Check functions
depth <- function(f){
    if(all(laply(f, length)==1)) return(1)
    max(laply(f, depth))+1
}
make_char_check <- function(x){
    function(f){identical(deparse(f), x)}
}
make_check  <- function(x, min.length=NULL, exact.length=NULL){
    check <- make_char_check(x)
    function(f){
        ((is.null(min.length)   || length(f) >= min.length)   && 
         (is.null(exact.length) || length(f) == exact.length)  ) && 
        check(f[[1]])
    }
}

is_formed     <- make_check('~', 3, 3)
is_nest       <- make_check('*', 2)
is_group      <- make_check('(', 2)
is_by_two_fun <- make_check('/', 3, 3)
is_identity   <- make_check('I', 2, 2)
is_bind       <- make_check('+', 3)
is_terminal   <- function(f) {
    (length(f)==1) || 
    is_identity(f)
}
is_unitary <- function(x){
    is.vector(x) && length(x)==1
}
}
{## roll
unroll <- function(f, check){
    if(!check(f))return(f)
    Reduce(append, llply(f, unroll, check)[-1])
}
reroll <- function(ilist, new.class = 'list'){
    rr <- function(a,b) {structure(list(a,b), class=new.class)}    
    Reduce(rr, ilist, right=TRUE)
}
remove_parentheses <- function(f){
    if(!is_group(f)) return(f)
    else f[[-1]]
}
}
{## Parsers
make_fcheck  <- function(class) {
    function(x){    
        cx <- class(x)
        ix <- grepl("dostats\\.formula", cx)
        any(ix) && any(grepl(class, cx[ix]))
    }
}
parse_nest <- function(level, data, reeval = sys.function(1)) {
    l <- unroll(level, is_nest)
    l <- llply(l, parse_level, data=data, reeval=reeval)
    l <- reroll(l)
    structure(l, call=level, class = c("dostats.formula.nest", 'list'))
}
parse_by_two_fun <- function(level, data) {
    l <- unroll(level, is_by_two_fun)
    l <- llply(l, parse_level, data=data)
    var <- Filter(make_fcheck('variable'), l)
    if(length(var) > 1) stop("Too many variables specified for two way function")
    
    funs <- Filter(make_fcheck('function'), l)
    structure(list(var=var[[1]], funs = funs), call = level
            , class=c('dostats.formula.by_two_fun', 'list'))
}
parse_bind <- function(level, data, reeval=sys.function(1)) {
    l <- unroll(level, is_bind)
    l <- llply(l, reeval, data=data)
    structure(l, call=level, class = c('dostats.formula.bind', 'list'))
}
parse_terminal <- function(node, data) {
    name <- deparse(node)
    x <- eval(node, data)
    fclass <- {
        if(inherits(x, 'selector.function')) 'dostats.formula.selector_function'
        else if(is.function(x)) 'dostats.formula.function' 
        else if(is_identity(node)) 'dostats.formula.Ivar' 
        else if(name %in% names(data)) 'dostats.formula.variable' 
        else {
            # stopifnot(length(x) == nrow(data))
            'dostats.formula.Xvariable'
        }
    }
    structure(x, call = node, name = name
            , class=c(fclass, 'dostats.formula.terminal', class(x)))
}
parse_level <- function(level, data, reeval=sys.function(1)) {
         if (is_terminal(level)) parse_terminal(level, data)
    else if (is_nest(level)) parse_nest(level, data, reeval)
    else if (is_group(level)) parse_level(level[[2]], data, reeval)
    else if (is_by_two_fun(level)) parse_by_two_fun(level, data)
    else if (is_bind(level)) parse_bind(level, data, reeval)
    else stop(sprintf("I Don't know what to do with '%s'", deparse(level)))
}
parse_right <- function(right, data) {
    stopifnot(!is_formed(right))
    right <- parse_level(right, data, redirf(parse_right))
    structure(right, class = c('dostats.formula.right', oldClass(right))) 
}
parse_left <- function(left, data) {
    stopifnot(!is_formed(left))
    level <- parse_level(left, data, redirf(parse_left))
         # if (is_terminal(left)) parse_terminal(left, data)
    # else if (is_bind(    left)) parse_bind(    left, data, reeval=parse_left)
    # else if (is_nest(    left)) parse_nest(    left, data, reeval=parse_left)
    # else stop(sprintf("I don't know what to do with '%s' on the left."
                     # , deparse(level)))
    structure(level, class = c('dostats.formula.left', oldClass(level)))
}
parse_table_formula <- function(formula, data) {
    stopifnot(is_formed(formula))
    ur <- unroll(formula, is_formed)
    left  <- parse_left(formula[[2]], data)
    right <- parse_right(formula[[3]], data)
    
    list(left = left, right=right)
}
}

#' @rdname internal
#' @export
take_names <- function(x)UseMethod('take_names')
#{## Take Names
#' @export
take_names.default <- function(x){
    if(is.list(x)){
        do.call(rbind, llply(x, take_names))
    } else {
        data.frame(name = attr(x, 'name'), class=first(class(x)))
    }
}
#' @export
take_names.dostats.formula.by_two_fun<- function(x) {
    data.frame(name = attr(x$var[[1]], 'name'), class=first(class(x$var[[1]])))
}
variable.classes <- paste('dostats', 'formula', .T(variable, Ivar, Xvariable), sep='.')
#}

#' @rdname internal
#' @title Internal Commandes
#' @export
get_vars <- function(x)UseMethod('get_vars')
#{## get_vars
#' @export
get_vars.dostats.formula.by_two_fun <- function(x){list(x$var)}
#' @export
get_vars.dostats.formula.nest <- function(x){
    l <- llply(x, get_vars)
    Filter(Negate(is.null), append(l[[1]], l[[2]]))
}
#' @export
get_vars.dostats.formula.variable <- function(x){list(x)}
#' @export
get_vars.dostats.formula.bind     <- function(x){Filter(Negate(is.null), llply(x, get_vars))}
#' @export
get_vars.default <- function(x)return(NULL)
#}

#{## Evaluators
	#' @rdname internal
	#' @param x parsed left side of formula
	#' @param y parsed right side of formula
	#' @param idf idata.frame object
	#' 
	#' @export
	dseval_left <- function(x, y, idf){UseMethod('dseval_left')}
    #{### Left
        #' @export
        dseval_left.dostats.formula.bind <- function(x, y, idf) {
            l <- llply(x, dseval_left, y, idf=idf)
            structure(Reduce(rbind.hdf, l))            
        }
        #' @export
        dseval_left.dostats.formula.nest <- function(x, y, idf) {
            stop()
            l <- tapply(x[[1]], x[[1]], dseval_left, y, idf=idf)
        }
        #' @export
        dseval_left.dostats.formula.left <- function(x, y, idf) {
            NextMethod('dseval_left')
        }
        #' @export
        dseval_left.dostats.formula.variable <- function(x, y, idf){
            dseval_right(y, x, idf)
        }
        #' @export
        dseval_left_var <- function(x, y, idf)UseMethod("dseval_left_var")
    #}

	#' @rdname internal
	#' 
	#' @export
	dseval_right <- function(y, x, idf) UseMethod('dseval_right')
    #{### Right
        #' @export
        dseval_right.dostats.formula.function <- function(y, x, idf){
            lnam <- attr(x, 'name')
            lvar <- attr(x, 'call')
            name <- attr(y, 'name')
            stopifnot(is.function(y))
            structure(
                hdf(y(eval(lvar, idf)))
                , ds.source = c('right', 'formula')
                , names = name)
        }
        #' @export
        dseval_right.dostats.formula.bind <- function(y, x, idf) {
            # y = right[[2]]
            l <- llply(y, dseval_right, x, idf)
            names <- laply(y, attr, 'name')
            structure(hdf(l)
                , ds.source = c('right', 'bind')
                , names = names
                , row.names = attr(x, 'name'))
        }
        #' @export
        dseval_right.dostats.formula.by_two_fun <- function(y, x, idf) {
            # y = right[[1]]
            # x = left[[1]]
            l <- llply(y$funs, apply_by_two_fun, x=x, y=y$var, idf=idf)
            structure(hdf(l)
                     , ds.source = c('right', 'by_two_fun')
                     , row.names = attr(x, 'name'))
        }
        #' @export
        apply_by_two_fun <- function(fun, x, y, idf){
            # y = right[[1]][[1]]
            # x = left[[1]]
            # fun = right[[1]][[2]][[1]]
            stopifnot(is.function(fun))
            lnam <- attr(x, 'name')
            lvar <- attr(x, 'call')
            fnam <- attr(fun, 'name')
            rnam <- attr(y, 'name')
            rvar <- attr(y, 'call')
            rslt <- fun(data.frame(eval(lvar, idf), eval(rvar, idf)))
            if(!is_unitary(rslt) && !is.data.frame(rslt)){
                rslt  = format.pval(pval(rslt))
            }
            structure(hdf(rslt), names = fnam)
        }
        #' @export
        dseval_right.dostats.formula.nest <- function(y, x, idf){
            # y = right
            # x = left[[1]]
            stopifnot(length(y)==2)
            by.var <- dseval_right(y[[1]], x=x, idf=idf)
            
            vars <- get_vars(y[[1]])
            by.level <- 
            llply(vars, dseval_fork, y=y[[2]], x=x, idf=idf, gen_call=dseval_right)
            names <- laply(vars, attr, 'name')
            names(by.level) <- names
            
            rslt <- structure(
                hdf(by.var,  by.level)
                , class = c('hdf', 'data.frame', 'list')
                , ds.source = c('right', 'nest')
                , row.names = attr(x, 'name'))
            rslt
        }
        #' @export
        dseval_fork <- function(var, y, x, idf, gen_call){
            l <- dlply(idf, attr(var, 'call'), gen_call, x=x, y=y)
            structure(l, class= c('hdf', 'data.frame', 'list')
                     , ds.source = c('right', 'fork')
                     , row.names = attr(x, 'name'))
        }        
        #' @export
        dseval_right.default <- function(y, x, idf)return(NULL)
    #}
#}


#' Create a table of descriptives of a dataset.
#'
#' @param formula a formula description of the table.
#' @param data a data.frame or environment to extract the data from.
#'
#' 
Table1 <- function(formula, data){
	idf <- if(inherits(data, 'idf')) data else idata.frame(data)
	parsed    <- parse_table_formula(formula, idf)
	evaluated <- dseval_left(parsed$left, y=parsed$right, idf)
  if(length(evaluated)>1)
    evaluated <- Reduce(rbind.hdf, evaluated)
  hdf(evaluated)
}

if(F){# build and test code
data <- 
ex.data <- {data.frame(
    y1 = rnorm(100)
  , y2 = rbinom(100, 10, .5)
  , f1 = sample(letters[1:3], 100, replace=T)
  , f2 = sample(c(T, F), 100, replace=T)
  )}
formula = y1 + y2 ~ (f2/lm) * (mean + sd)
}
    
