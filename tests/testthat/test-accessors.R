test_that("target and iters accessors roundtrip", {
  ctrl <- fwdControl(data.frame(year = 2000:2001, quant = "f", value = c(0.2, 0.3)))

  expect_true(is.data.frame(target(ctrl)))
  expect_true(is.array(iters(ctrl)))

  new_target <- target(ctrl)
  new_target$unit <- "u1"
  target(ctrl) <- new_target
  expect_identical(target(ctrl)$unit, rep("u1", nrow(new_target)))

  new_iters <- iters(ctrl)
  new_iters[, "value", ] <- 0.5
  iters(ctrl) <- new_iters
  expect_true(all(ctrl$value == 0.5))
})

test_that("FCB accessor and replacement work", {
  ctrl <- fwdControl()
  fcb <- matrix(c(1, 1, 1), ncol = 3, byrow = TRUE, dimnames = list(1, c("F", "C", "B")))

  FCB(ctrl) <- fcb
  expect_identical(unname(FCB(ctrl)), unname(fcb))
  expect_identical(colnames(FCB(ctrl)), c("F", "C", "B"))
})
