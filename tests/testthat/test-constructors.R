test_that("fwdControl constructor handles data.frame input", {
  trg <- data.frame(year = 2000:2002, quant = "f", value = c(0.2, 0.3, 0.4))
  ctrl <- fwdControl(trg)

  expect_s4_class(ctrl, "fwdControl")
  expect_equal(nrow(target(ctrl)), 3)
  expect_equal(dim(iters(ctrl))[3], 1)
})

test_that("fwdControl constructor handles explicit iters argument", {
  trg <- data.frame(year = 2000:2001, quant = "f")
  it <- array(c(NA, NA, 0.2, 0.3, NA, NA), dim = c(2, 3, 1), dimnames = list(row = 1:2, val = c("min", "value", "max"), iter = 1))

  ctrl <- fwdControl(target = trg, iters = it)
  expect_equal(ctrl$value, c(0.2, 0.3))
})

test_that("fwdControl constructor handles list and numeric iters", {
  ctrl <- fwdControl(list(year = 2001:2002, quant = "f", value = c(0.2, 0.3)))
  expect_s4_class(ctrl, "fwdControl")

  ctrl_n <- fwdControl(data.frame(year = 2001:2002, quant = "f", value = c(0.2, 0.3)), iters = 3)
  expect_equal(dim(iters(ctrl_n))[3], 3)

  expect_error(
    fwdControl(data.frame(year = 2001, quant = "f", value = 0.2), iters = c(2, 3)),
    "length 1"
  )
})

test_that("fwdControl constructor handles FLQuant input", {
  fq <- FLQuant(0.3, dimnames = list(year = 2001))
  ctrl <- fwdControl(target = fq, quant = "fbar")

  expect_s4_class(ctrl, "fwdControl")
  expect_equal(as.character(ctrl$quant), "fbar")
  expect_equal(ctrl$value, 0.3)
})
