# test-effort_max.R - DESC
# /test-effort_max.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


context("Effort_max in fwd(FLStock)")

test_that("Rel. F and effort_max",{

    data(ple4)

    control <- fwdControl(year=2000:2017, quant="f", value=seq(0.60, 3, length=18))

    res <- fwd(ple4, control=control, sr=predictModel(model="geomean", 
      params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))
})
