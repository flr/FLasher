test_that("stf methods extend FLStock and FLBiol", {
  data(ple4, package = "FLCore")

  stf_stock <- stf(ple4, nyears = 2)
  expect_s4_class(stf_stock, "FLStock")
  expect_true(dims(stf_stock)$maxyear >= dims(ple4)$maxyear + 2)

  biol <- as(ple4, "FLBiol")
  stf_biol <- stf(biol, nyears = 2)
  expect_s4_class(stf_biol, "FLBiol")
  expect_true(dims(stf_biol)$maxyear >= dims(biol)$maxyear + 2)
})

test_that("stf FLBiol validates nyears/end consistency", {
  data(ple4, package = "FLCore")
  biol <- as(ple4, "FLBiol")
  expect_error(stf(biol, nyears = 1, end = dims(biol)$maxyear + 3), "do not match")
})
