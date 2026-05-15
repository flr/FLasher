test_that("random FLQuant helper generators create expected structures", {
  set.seed(123)
  flq <- random_FLQuant_generator(fixed_dims = c(2, 3, 1, 1, 1, 2), sd = 1)
  expect_s4_class(flq, "FLQuant")
  expect_identical(dim(flq), c(2L, 3L, 1L, 1L, 1L, 2L))

  flqs <- random_FLQuant_list_generator(min_elements = 2, max_elements = 2, fixed_dims = c(1, 2, 1, 1, 1, 1))
  expect_equal(length(flqs), 2)
  expect_true(all(vapply(flqs, methods::is, logical(1), "FLQuant")))
})

test_that("random object helpers create FLR classes", {
  set.seed(42)
  expect_s4_class(random_FLBiolcpp_generator(fixed_dims = c(2, 3, 1, 1, 1, 1)), "FLBiolcpp")
  expect_s4_class(random_FLCatch_generator(fixed_dims = c(2, 3, 1, 1, 1, 1)), "FLCatch")
  expect_s4_class(random_FLCatches_generator(min_catches = 2, max_catches = 2, fixed_dims = c(2, 3, 1, 1, 1, 1)), "FLCatches")
  expect_s4_class(random_FLFishery_generator(min_catches = 2, max_catches = 2, fixed_dims = c(2, 3, 1, 1, 1, 1)), "FLFishery")
  expect_s4_class(random_FLFisheries_generator(min_fisheries = 2, max_fisheries = 2, min_catches = 2, max_catches = 2, fixed_dims = c(2, 3, 1, 1, 1, 1)), "FLFisheries")
})

test_that("random control and OM helpers return required components", {
  set.seed(99)
  ctrl <- random_fwdControl_generator(years = 2001:2002, nseasons = 1, max_nsim_target = 2, niters = 2)
  expect_s4_class(ctrl, "fwdControl")

  data(ple4, package = "FLCore")
  fcb <- matrix(c(1, 1, 1), ncol = 3, dimnames = list(1, c("F", "C", "B")))
  om <- make_test_operatingModel(ple4, FCB = fcb, nseasons = 1, recruitment_seasons = 1, recruitment_age = 1, niters = 2, sd = 0.01)

  expect_true(all(c("fisheries", "biols", "fwc") %in% names(om)))
  expect_s4_class(om$fisheries, "FLFisheries")
  expect_true(is.list(om$biols))
  expect_s4_class(om$fwc, "fwdControl")
})

test_that("FLQuant index helper functions map elements correctly", {
  flq <- FLQuant(1:16, dim = c(2, 2, 2, 1, 1, 2))

  i1 <- get_FLQuant_element(flq, c(1, 1, 1, 1, 1, 1))
  i2 <- get_FLQuant_element(flq, c(2, 2, 2, 1, 1, 2))
  idx <- get_FLQuant_elements(flq, c(1, 1, 1, 1, 1, 1), c(2, 2, 2, 1, 1, 2))

  expect_equal(i1, 1)
  expect_equal(i2, 16)
  expect_equal(length(idx), 16)
  expect_equal(sort(unique(idx)), 1:16)
})
