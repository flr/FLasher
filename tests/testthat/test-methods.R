test_that("show and summary methods run for fwdControl", {
  ctrl <- fwdControl(data.frame(year = 2000:2001, quant = c("f", "catch"), value = c(0.2, 2000)))

  expect_output(show(ctrl), "fwdControl")
  expect_output(summary(ctrl), "fwdControl")
})

test_that("indexing and dollar methods expose and update slots", {
  ctrl <- fwdControl(data.frame(year = 2000:2002, quant = "f", value = c(0.2, 0.3, 0.4)))

  sub <- ctrl[1:2]
  expect_equal(nrow(target(sub)), 2)

  ctrl$unit <- c("u", "u", "u")
  expect_identical(ctrl$unit, c("u", "u", "u"))

  ctrl$min <- c(0.1, 0.1, 0.1)
  expect_equal(length(ctrl$min), 3)
})

test_that("replacement, propagate, merge and iter methods work", {
  ctrl <- fwdControl(data.frame(year = 2000:2001, quant = "f", value = c(0.2, 0.3)))
  ctrl[1, "value"] <- 0.5
  expect_equal(ctrl$value[1], 0.5)

  p <- propagate(ctrl, 3)
  expect_equal(dim(iters(p))[3], 3)

  merged <- merge(ctrl, ctrl)
  expect_s4_class(merged, "fwdControl")
  expect_equal(nrow(target(merged)), 4)

  it2 <- iter(p, 2)
  expect_equal(dim(iters(it2))[3], 1)
})
