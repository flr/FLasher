context("FLasherLib helper")

test_that("FLasherLib returns library path", {
  lib_path <- capture.output(FLasher:::FLasherLib())
  expect_length(lib_path, 1)
  expect_match(lib_path, "FLasher")
  expect_match(lib_path, .Platform$dynlib.ext)
})
