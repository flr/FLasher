## test-constructors.R - DESC
## FLasher/tests/testthat/test-constructors.R
#
## Copyright European Union, 2016
## Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
##
## Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.
#
## CONTEXT fwdControl(target=data.frame, iters=array) {{{
#
#context("fwdControl(target=data.frame, iters=array)")
#
#target <- data.frame(year=2000:2005, quant='f')
#
#  # TEST w/ iters as unnamed 3D [n,1,n] array {{{
#
#  iters <- array(runif(60, 0.3, 0.8), dim=c(6,1,10))
#
#  test_that("target df in object matches input", {
#
#    res <- fwdControl(target=target, iters=iters)
#
#    # equivalent, as factor levels in res@target == qlevels
#    expect_equivalent(res@target[,names(target)], target)
#  })
#  # }}}
#
#  # TEST w/ iters as unnamed 3D [n,3,1] array {{{
#
#  iters <- array(c(rep(NA, 6), runif(6, 0.3, 0.8), rep(NA, 6)), dim=c(6,3,1))
#
#  test_that("target df in object matches input", {
#
#    res <- fwdControl(target=target, iters=iters)
#
#    # equivalent, as factor levels in res@target == qlevels
#    expect_equivalent(res@target[,names(target)], target)
#  })
#  # }}}
#
#  # TEST w/ iters as right named 3D array {{{
#
#  iters <- array(runif(60, 0.3, 0.8), dim=c(6,1,10),
#    dimnames=list(row=1:6, val='value', iter=1:10))
#
#  test_that("target df in object matches input", {
#
#    res <- fwdControl(target=target, iters=iters)
#
#    # equivalent, as factor levels in res@target == qlevels
#    expect_equivalent(res@target[,names(target)], target)
#
#    # values in iters
#    expect_equal(c(res@iters)[!is.na(res@iters)], c(iters))
#  })
#
#  # }}}
#
## }}}
#
## CONTEXT fwdControl(target=data.frame, iters=matrix) {{{
#
#context("fwdControl(target=data.frame, iters=matrix)")
#
#target <- data.frame(year=2000:2005, quant='f')
#
## }}}
#
## CONTEXT fwdControl(target=data.frame, iters=numeric) {{{
#
#context("fwdControl(target=data.frame, iters=numeric)")
#
#target <- data.frame(year=2000:2005, quant='f', value=0.8)
#
#fwdControl(target=target, iters=25)
#
## }}}
#
## LIST
#
#target <- list(list(year=1990, quant='f', value=0.1),
#     list(year=1991, quant='ssb_end', value=2000))
#
#fwdControl(target)
#
##
#
#target <- list(list(year=1990:1992, quant='f', value=0.1),
#     list(year=1993, quant='ssb_end', value=2000))
#
#fwdControl(target)
#
#
#
# Actual constructor regression and edge-case tests

context("fwdControl constructor methods")

test_that("data.frame + array constructor keeps target and iters", {
  target <- data.frame(year = 2000:2002, quant = "f")
  iters <- array(1:9, dim = c(3, 3, 1),
    dimnames = list(row = 1:3, val = c("min", "value", "max"), iter = 1))

  ctrl <- fwdControl(target = target, iters = iters)

  expect_equal(ctrl$year, 2000:2002)
  expect_equal(as.character(ctrl$quant), rep("f", 3))
  expect_equal(c(iters(ctrl)[, "value", ]), c(2, 5, 8))
})

test_that("data.frame + numeric iters expands iterations", {
  target <- data.frame(year = 2000:2001, quant = "catch", value = c(100, 120))
  ctrl <- fwdControl(target = target, iters = 4)

  expect_equal(dim(iters(ctrl))[3], 4)
  expect_equal(c(iters(ctrl)[, "value", 1]), c(100, 120))
  expect_equal(c(iters(ctrl)[, "value", 4]), c(100, 120))
})

test_that("duplicate min/max rows are merged", {
  target <- data.frame(
    year = c(2000, 2000),
    quant = c("catch", "catch"),
    min = c(100, NA),
    max = c(NA, 150)
  )

  ctrl <- fwdControl(target = target)

  expect_equal(nrow(target(ctrl)), 1)
  expect_equal(c(iters(ctrl)[1, "min", ]), 100)
  expect_equal(c(iters(ctrl)[1, "max", ]), 150)
})

test_that("numeric iters argument rejects vectors longer than one", {
  expect_error(
    fwdControl(target = data.frame(year = 2000, quant = "f", value = 0.2), iters = c(2, 3)),
    "'iters' must be of length 1"
  )
})
##
#
#target <- list(list(year=1990, quant='f', value=runif(10, 0.1, 0.2)),
#     list(year=1991, quant='ssb_end', value=rnorm(10, 2000)))
#
#fwdControl(target)
#
##
#
#target <- list(list(year=1990:1992, quant='f', value=runif(10, 0.1, 0.2)),
#     list(year=1993, quant='ssb_end', value=rnorm(10, 2000)))
#
#fwdControl(target)
#
##
#
#target <- list(list(year=1990, quant='f', max=runif(10, 0.1, 0.2), min=0.05),
#     list(year=1991, quant='ssb_end', value=rnorm(10, 2000)))
#
#fwdControl(target)
#
##
#
#target <- list(list(year=1990, quant='f', max=runif(10, 0.1, 0.2), min=NA),
#     list(year=1991, quant='ssb_end', value=rnorm(10, 2000)))
#
#fwdControl(target)
#
#
## list elements with more than one row
#
#target <- list(list(year=1990:1992, quant='f', max=rep(1, 10), min=0.05),
#     list(year=1993, quant='ssb_end', value=rnorm(10, 200)))
#
#fwdControl(target)
#
##
#
#target <- list(list(year=1990, season=1:4, quant='f', max=rep(1, 10), min=0.05),
#     list(year=1993, quant='ssb_end', season=1, value=rnorm(10, 200)))
#
#fwdControl(target)
#
## wrong year order
#
#target <- list(list(year=1992:1990, quant='f', max=rep(1, 10), min=0.05),
#     list(year=1993, quant='ssb_end', value=rnorm(10, 200)))
#
#fwdControl(target)
#
## Large df
#
#target <- data.frame(year=1950:2025, quant='f')
#
#iters <- array(runif(60000, 0.3, 0.8), dim=c(76,1,10))
#
#fwdControl(target, iters)
#
#
## no biolGroup
#
## data.frame()
#
#
#
#x <- fwdControl(data.frame(year=2000:2001, value=200, quant="catch", biol=1))
#
## Accessor
#
#
#x[1, 'biol'] <- 3
#x[1, 'biol'] <- G(1,2)
#
#x$biol <- G(1,2)
#
#
#fwdControl(list(year=2000:2001, value=200, quant="catch", biol=G(1,2)),
#  list(year=2002:2003, value=100, quant="catch", biol=c(1,2)))
#
#fwdControl(list(year=2000:2001, value=200, quant="catch", biol=I(list(c(1,2)))))
#
#fwdControl(list(year=2000:2001, value=200, quant="catch", biol=c(1,2)))
#
#
#
