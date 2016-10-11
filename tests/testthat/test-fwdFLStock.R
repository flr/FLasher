# test_fwdSingleFCB.R - DESC
# FLasher/tests/test_fwdSingleFCB.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# SETUP
data(ple4)
psr <- fmle(as.FLSR(ple4, model="bevholt"))
om <- stf(ple4, 5)

#-- TEST targets met for all .qlevels, BUT ssb
f <- fbar

for(i in FLasher:::.qlevels[1:4]) {

  test_that(paste(i, "target"), {
    
    # GET last value (vector)
    value <- c(do.call(i, list(ple4)))
    value <- value[length(value)]
    
    # SET UP control
    control <- fwdControl(data.frame(year=2009:2013, quant=i, value=value))

    # RUN fwd
    res <- fwd(om, control=control, sr=psr)

    # (1) TEST output 2009:2013 equals target
    expect_equal(c(do.call(i, list(res))[,ac(2009:2013)]), rep(value, 5))
    })
}

# SSB
# BUG How to deal with ssb target in last year? (1) EXTEND object (2) ERROR
test_that("ssb target", {
  value <- 210000
  control <- fwdControl(data.frame(year=2009:2012, quant="ssb", value=value))

  res <- fwd(om, control=control, sr=psr)

  expect_equal(c(ssb(res)[,ac(2010:2012)]), rep(value, 3))
})


# RESIDUALS
residuals <- FLQuants(PLE=window(residuals(psr), start=1957))
res <- fwd(ple4, control=control, residuals=residuals[[1]], sr=fsr)
res <- fwd(ple4, control=control, sr=fsr)


# FLS + FLQ
res <- fwd(ple4, sr=psr, catch=FLQuant(1000, dimnames=list(year=1990:1995)))
res <- fwd(ple4, sr=psr, fbar=FLQuant(0.1, dimnames=list(year=1990:1995)))
res <- fwd(ple4, sr=psr, f=FLQuant(0.1, dimnames=list(year=1990:1995)))
