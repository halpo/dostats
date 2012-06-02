{###############################################################################
# test_internal_recombine.R
# Copyright 2012 Andrew Redd
# Date: 6/1/2012
# 
# DESCRIPTION
# ===========
# unit tests for recombine function
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
library(testthat)
library(dostats)
context("internal")
test_that("recombine_idx", {
    pmat <- cbind(c(1,2,3), c(1, 2, 2), c(0,0,0))
    ri <- recombine_idx(pmat)
    expect_equal(ri, list(1:3, c(1:2,2), c(1,1)))
    if(F){
        debug(recombine_idx)
        recombine_idx(pmat)
    }
})
test_that("recombine.hdf", {
a <- hdf(hdf(i=1:3), j=hdf(l=hdf(.T(a, b, c)), L=hdf(.T(A,B,C))))
x <-list(    i=1:3,        l=    .T(a, b, c) , L=    .T(A,B,C)  )
pmat <- cbind(c(1,2,3), c(1, 2, 2), c(0,0,0))
ri <- recombine_idx(pmat)
b  <- recombine.hdf(x, ri)

all.equal(a,b, check.attributes=F)
})
