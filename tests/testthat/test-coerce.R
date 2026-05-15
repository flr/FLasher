test_that("FLQuants coerces to fwdControl", {
  fq <- FLQuants(f = FLQuant(0.2, dimnames = list(year = 2001)))
  ctrl <- as(fq, "fwdControl")

  expect_s4_class(ctrl, "fwdControl")
  expect_equal(as.character(ctrl$quant), "f")
  expect_equal(ctrl$year, 2001L)
  expect_equal(ctrl$value, 0.2)
})

test_that("list to fwdControl coercion validates single top-level element", {
  bad <- list(a = FLQuants(f = FLQuant(0.2, dimnames = list(year = 2001))), b = FLQuants(f = FLQuant(0.3, dimnames = list(year = 2002))))
  expect_error(as(bad, "fwdControl"), "single element")
})

test_that("fwdControl coerces to FLQuant", {
  ctrl <- fwdControl(data.frame(year = 2000:2001, quant = "f", value = c(0.2, 0.3)))
  q <- as(ctrl, "FLQuant")

  expect_s4_class(q, "FLQuant")
  expect_equal(dim(q)[2], 2)
})
