test_that("cfwd rejects unsupported fwdControl with duplicate years", {
  data(ple4, package = "FLCore")
  sr <- rec(ple4)
  ctrl <- fwdControl(data.frame(year = c(2000, 2000), quant = "catch", value = c(1, 2)))

  expect_error(cfwd(ple4, sr = sr, catch = ctrl), "yearly targets")
})
