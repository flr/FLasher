context("test helper functions")

test_that("random_FLQuant_generator respects fixed dimensions", {
  flq <- random_FLQuant_generator(fixed_dims = c(2, 3, 1, 1, 1, 1))
  expect_equal(dim(flq), c(2, 3, 1, 1, 1, 1))
})

test_that("FLQuant element index helper functions return expected positions", {
  flq <- FLQuant(1:64, dim = c(2, 2, 2, 2, 2, 2))
  expect_equal(get_FLQuant_element(flq, c(1, 1, 1, 1, 1, 1)), 1)
  expect_equal(get_FLQuant_element(flq, c(2, 1, 1, 1, 1, 1)), 2)

  els <- get_FLQuant_elements(flq, c(1, 1, 1, 1, 1, 1), c(2, 1, 1, 1, 1, 1))
  expect_equal(els, c(1, 2))
})
