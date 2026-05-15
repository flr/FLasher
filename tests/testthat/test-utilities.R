test_that("G wraps multi-biol input as AsIs list", {
  g <- G(1, 2, 3)
  expect_s3_class(g, "AsIs")
  expect_identical(unclass(g)[[1]], c(1, 2, 3))
})

test_that("targetOrder orders by year then numeric season", {
  target <- data.frame(year = c(2001, 2000, 2000), season = c("2", "1", "2"))
  iters <- array(NA_real_, dim = c(3, 3, 1), dimnames = list(row = 1:3, val = c("min", "value", "max"), iter = 1))

  idx <- FLasher:::targetOrder(target, iters)
  expect_identical(idx, c(2L, 3L, 1L))

  target$season <- c("A", "1", "2")
  expect_error(FLasher:::targetOrder(target, iters), "Season names cannot be ordered")
})

test_that("parsefwdList and add_target_order_fls return expected structures", {
  parsed <- FLasher:::parsefwdList(year = 2001:2002, quant = "f", value = c(0.2, 0.3))
  expect_true(is.list(parsed))
  expect_true(all(c("target", "iters") %in% names(parsed)))

  ctrl <- fwdControl(data.frame(year = c(2001, 2000), season = c(2, 1), quant = "f", value = c(0.2, 0.3)))
  sorted <- FLasher:::add_target_order_fls(ctrl)
  expect_true(all(diff(sorted@target$year) >= 0))
})

test_that("match_posns_names maps names to integer positions", {
  trg <- data.frame(
    fishery = I(list("F1")),
    catch = I(list("C1")),
    biol = I(list("B1")),
    relFishery = I(list("F1")),
    relCatch = I(list("C1")),
    relBiol = I(list("B1")),
    stringsAsFactors = FALSE
  )

  out <- match_posns_names(
    trg,
    biol_names = c("B1", "B2"),
    fishery_catch_names = list(F1 = c("C1", "C2"))
  )

  expect_identical(out$biol[[1]], 1)
  expect_identical(out$fishery[[1]], 1)
  expect_identical(out$catch[[1]], 1)
})
