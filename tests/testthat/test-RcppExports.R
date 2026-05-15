test_that("operatingModelRun wrapper has expected signature", {
  fn <- FLasher:::operatingModelRun
  expect_true(is.function(fn))
  expect_equal(
    names(formals(fn)),
    c("flfs", "biols", "ctrl", "effort_max", "effort_mult_initial", "indep_min", "indep_max", "nr_iters")
  )
  expect_identical(formals(fn)$nr_iters, 50L)
})

test_that("operatingModelRun validates missing required arguments before .Call", {
  expect_error(FLasher:::operatingModelRun(), "argument")
})
