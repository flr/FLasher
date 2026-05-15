context("coerce methods")

test_that("FLQuants can be coerced to fwdControl", {
  ctrl <- as(FLQuants(catch = FLQuant(1000, dimnames = list(year = 2000))), "fwdControl")
  expect_is(ctrl, "fwdControl")
  expect_equal(as.character(ctrl$quant), "catch")
})

test_that("fwdControl can be coerced to FLQuant", {
  ctrl <- fwdControl(data.frame(year = 2000:2001, quant = "catch", value = c(100, 120)))
  flq <- as(ctrl, "FLQuant")
  expect_is(flq, "FLQuant")
  expect_equal(c(flq), c(100, 120))
})

test_that("list to fwdControl coercion validates list length", {
  expect_error(as(list(FLQuants(), FLQuants()), "fwdControl"), "single element")
})
