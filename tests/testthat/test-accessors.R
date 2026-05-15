context("fwdControl accessors")

test_that("target and iters accessors get and set slots", {
  ctrl <- fwdControl(data.frame(year = 2001:2002, quant = "catch", value = c(100, 110)))

  trg <- target(ctrl)
  expect_equal(trg$year, 2001:2002)

  trg$year <- 2003:2004
  target(ctrl) <- trg
  expect_equal(target(ctrl)$year, 2003:2004)

  its <- iters(ctrl)
  its[, "value", ] <- c(1, 2)
  iters(ctrl) <- its
  expect_equal(c(iters(ctrl)[, "value", ]), c(1, 2))
})

test_that("FCB accessor get and set works", {
  ctrl <- fwdControl()
  fcb <- matrix(c(1, 1, 1), nrow = 1, dimnames = list(1, c("F", "C", "B")))
  FCB(ctrl) <- fcb
  expect_equal(FCB(ctrl), fcb)
})
