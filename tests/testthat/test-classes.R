test_that("internal target quant definitions are available", {
  expect_true(all(c("f", "catch", "effort") %in% FLasher:::.qlevels))
  expect_true(all(c("ssb", "biomass_end") %in% FLasher:::.biol_quants))
})

test_that(".foo expands fcb metadata rows", {
  x <- list(c("f", "fbar"), fishery = c(TRUE, FALSE), catch = c(TRUE), biol = c(TRUE))
  res <- FLasher:::.foo(x)

  expect_true(is.data.frame(res))
  expect_equal(unique(as.character(res$quant)), c("f", "fbar"))
})

test_that("fwdControl class validity catches bad quant and FCB dims", {
  obj <- new("fwdControl")

  obj@target$quant <- factor("not-a-quant", levels = c(levels(obj@target$quant), "not-a-quant"))
  expect_match(validObject(obj, test = TRUE), "not available")

  obj <- new("fwdControl")
  obj@FCB <- array(1, dim = c(1, 3, 1))
  expect_match(validObject(obj, test = TRUE), "2 dimensions")
})
