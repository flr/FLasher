context("ffwd and cfwd methods")

test_that("cfwd catches match explicit catch target", {
  data(ple4)
  sr <- predictModel(model = bevholt, params = FLPar(a = 140.4e4, b = 1.448e5))
  catch_target <- catch(ple4)[, ac(2000:2005)]

  out <- cfwd(ple4, catch = catch_target, sr = sr)

  expect_equal(c(catch(out)[, ac(2000:2005)]), c(catch_target), tolerance = 1e-6)
})

test_that("cfwd errors on unsupported fwdControl structure", {
  data(ple4)
  sr <- predictModel(model = bevholt, params = FLPar(a = 140.4e4, b = 1.448e5))
  ctrl <- fwdControl(list(year = 2000:2002, quant = "catch", min = 100))
  expect_error(cfwd(ple4, catch = ctrl, sr = sr), "min/max limits")
})
