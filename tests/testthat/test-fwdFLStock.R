# test_fwdSingleFCB.R - DESC
# FLasher/tests/test_fwdSingleFCB.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("fwd(FLStock)")

data(ple4)
psr <- fmle(as.FLSR(ple4, model="bevholt"))

#
test_that("srparams with no year works", {
  expect_is(
    fwd(ple4, sr=psr, control=fwdControl(year=2006:2008, quant='f', value=0.1)),
    "FLStock"
  )
})

#
params(psr) <- FLPar(c(params(psr)),
  dimnames=list(params=c('a', 'b'), year='2006', iter=1))

test_that("srparams with year not matching control fails", {
  expect_error(
    fwd(ple4, sr=psr, control=fwdControl(year=2006:2008, quant='f', value=0.1))
  )
})

#
params(psr) <- FLPar(c(params(psr)),
  dimnames=list(params=c('a', 'b'), year=ac(2006:2008), iter=1))

test_that("srparams with year matching control works", {
  expect_is(
    fwd(ple4, sr=psr, control=fwdControl(year=2006:2008, quant='f', value=0.1)),
    "FLStock"
  )
})
