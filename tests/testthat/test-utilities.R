context("utilities")

test_that("G helper builds AsIs list wrapper", {
  val <- G("a", "b")
  expect_s3_class(val, "AsIs")
  expect_equal(unlist(val), c("a", "b"))
})

test_that("targetOrder sorts by year and season", {
  target <- data.frame(
    year = c(2001L, 2000L),
    season = c(2L, 1L),
    quant = c("catch", "catch"),
    stringsAsFactors = FALSE
  )
  iters <- array(NA, dim = c(2, 3, 1), dimnames = list(row = 1:2, val = c("min", "value", "max"), iter = 1))

  idx <- FLasher:::targetOrder(target, iters)
  expect_equal(idx, c(2, 1))
})
