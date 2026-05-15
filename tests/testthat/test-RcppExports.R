context("Rcpp exports")

test_that("operatingModelRun wrapper is available", {
  expect_true(exists("operatingModelRun", mode = "function"))
  expect_true(is.function(operatingModelRun))
})
