context("Rcpp plugin")

test_that("inlineCxxPlugin is defined for package", {
  expect_true(exists("inlineCxxPlugin"))
  expect_true(is.function(inlineCxxPlugin))
  plugin <- inlineCxxPlugin()
  expect_true(is.list(plugin))
})
