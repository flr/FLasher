context("package data")

test_that("mixed fishery example datasets are available", {
  data(mixed_fishery_example_om)
  expect_is(biols, "FLBiols")
  expect_is(flfs, "FLFisheries")
  expect_true(length(biols) > 0)
  expect_true(length(flfs) > 0)
})
