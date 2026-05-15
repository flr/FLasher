context("fwdControl class")

test_that("default fwdControl class object is valid", {
  ctrl <- new("fwdControl")
  expect_true(validObject(ctrl))
  expect_true("catch" %in% FLasher:::.qlevels)
})

test_that("constructor rejects unsupported quant values", {
  expect_error(
    fwdControl(data.frame(year = 2000, quant = "unknown_quant", value = 1)),
    "Specified 'quant' currently not available"
  )
})
