# test-FLStock_FLQuant_projection.R - DESC
# /test-FLStock_FLQuant_projection.R

# Copyright Iago MOSQUEIRA (WMR), 2020
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


context("FLStock projections with target as FLQuant")

# f=FLQuant is parsed properly
test_that("Target f=FLQuant()",{

  data(ple4)

  target <- fbar(ple4)[, ac(2000:2017)]

  res <- fwd(ple4, f=target,  sr=predictModel(model="geomean", 
    params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))

  res <- fwd(ple4, f=target,  sr=predictModel(model="geomean", 
    params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))),
    deviances=target * 0)

  expect_equal(fbar(res)[, ac(2000:2017)],
    fbar(ple4)[, ac(2000:2017)])
})
