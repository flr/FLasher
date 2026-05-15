test_that("mixed fishery example data loads", {
  data("mixed_fishery_example_om", package = "FLasher")

  expect_true(exists("biols"))
  expect_true(exists("flfs"))
  expect_s4_class(biols, "FLBiols")
  expect_s4_class(flfs, "FLFisheries")
})
