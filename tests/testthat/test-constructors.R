# test-constructors.R - DESC
# /test-constructors.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# CONTEXT fwdControl(target=data.frame, iters=array) {{{

context("fwdControl(target=data.frame, iters=array)")

target <- data.frame(year=2000:2005, quantity='f')

  # TEST w/ iters as unnamed 3D [n,1,n] array {{{

  iters <- array(runif(60, 0.3, 0.8), dim=c(6,1,10))

  test_that("target df in object matches input", {

    res <- fwdControl(target=target, iters=iters)

    # equivalent, as factor levels in res@target == qlevels
    expect_equivalent(res@target[,names(target)], target)
  })
  # }}}

  # TEST w/ iters as unnamed 3D [n,3,1] array {{{

  iters <- array(c(rep(NA, 6), runif(6, 0.3, 0.8), rep(NA, 6)), dim=c(6,3,1))

  test_that("target df in object matches input", {

    res <- fwdControl(target=target, iters=iters)

    # equivalent, as factor levels in res@target == qlevels
    expect_equivalent(res@target[,names(target)], target)
  })
  # }}}

  # TEST w/ iters as right named 3D array {{{

  iters <- array(runif(60, 0.3, 0.8), dim=c(6,1,10),
    dimnames=list(row=1:6, val='value', iter=1:10))

  test_that("target df in object matches input", {

    res <- fwdControl(target=target, iters=iters)

    # equivalent, as factor levels in res@target == qlevels
    expect_equivalent(res@target[,names(target)], target)

    # values in iters
    expect_equal(c(res@iters)[!is.na(res@iters)], c(iters))
  })

  # }}}

# }}}

# CONTEXT fwdControl(target=data.frame, iters=matrix) {{{

context("fwdControl(target=data.frame, iters=matrix)")

target <- data.frame(year=2000:2005, quantity='f')

# }}}
