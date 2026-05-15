# test-fwd.R - Tests for fwd() projections with ple and ple4sex FLStock objects
# flr/FLasher/tests/testthat/test-fwd.R

# Copyright Iago MOSQUEIRA (WMR), 2026
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# SET data
data(ple4)

m.spwn(ple4) <- harvest.spwn(ple4) <- 0.3

# SRR
ple4_sr <- fmle(as.FLSR(ple4, model="bevholt"), control=list(trace=0))

# --- TESTS with ple4 FLStock --- {{{

# - Fbar

test_that("ple4: fbar target, single iter", {
  
  # Setup projection years
  ple4_mtf <- stf(ple4, nyears = 10)
  years <- 2009:2018
  fbar_target <- 0.25
  
  # Run projection with fbar target
  control <- fwdControl(data.frame(year=years, quant="fbar", value=fbar_target))
  res <- fwd(ple4_mtf, control=control, sr=ple4_sr)
  
  # Check fbar values match target
  expect_equal(c(fbar(res)[,ac(years)]), rep(fbar_target, length(years)), tolerance=1e-6)
})

# - Fbar with iters

test_that("ple4: fbar target with multiple iterations", {
  
  # Setup projection with multiple iterations
  niters <- 10
  ple4_mtf <- stf(ple4, nyears = 5)
  ple4_mtf <- propagate(ple4_mtf, niters)
  years <- 2009:2013
  fbar_target <- 0.25
  
  # Setup SR
  ple4_sr <- fmle(as.FLSR(ple4, model="bevholt"), control=list(trace=0))
  
  # Run projection
  control <- fwdControl(data.frame(year=years, quant="fbar", value=fbar_target))
  res <- fwd(ple4_mtf, control=control, sr=ple4_sr)
  
  # Check fbar across all iterations
  fbar_res <- fbar(res)[,ac(years)]
  expect_equal(dim(fbar_res)[6], niters)
  expect_equal(c(apply(fbar_res, 2:5, mean)), rep(fbar_target, length(years)), tolerance=1e-6)
})

# - Catch

test_that("ple4: catch target, single iter", {
  
  # Setup projection
  ple4_mtf <- stf(ple4, nyears = 10)
  years <- 2009:2018
  catch_target <- 100000
  
  # Run projection with catch target
  control <- fwdControl(data.frame(year=years, quant="catch", value=catch_target))
  res <- fwd(ple4_mtf, control=control, sr=ple4_sr)
  
  # Check catch values match target
  expect_equal(c(catch(res)[,ac(years)]), rep(catch_target, length(years)), tolerance=1e-4)
})

# - Catch w/iters

test_that("ple4: catch target with multiple iterations", {
  
  # Setup projection with multiple iterations
  niters <- 10
  ple4_mtf <- stf(propagate(ple4, niters), nyears = 5)
  years <- 2009:2013
  catch_target <- 95000
  
  # Setup SR
  ple4_sr <- fmle(as.FLSR(ple4, model="bevholt"), control=list(trace=0))
  
  # Run projection
  control <- fwdControl(data.frame(year=years, quant="catch", value=catch_target))
  res <- fwd(ple4_mtf, control=control, sr=ple4_sr)
  
  # Check catch across all iterations
  catch_res <- catch(res)[,ac(years)]
  expect_equal(dim(catch_res)[6], niters)
  expect_equal(c(apply(catch_res, 2:5, mean)), rep(catch_target, length(years)), tolerance=1e-4)
})

# }}}
