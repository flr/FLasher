test_that("FLasherLib prints package shared library path", {
  out <- paste(capture.output(FLasher:::FLasherLib()), collapse = "")
  expect_match(out, "FLasher")
  expect_true(endsWith(out, .Platform$dynlib.ext))
})
