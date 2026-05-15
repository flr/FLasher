context("stf window methods")

test_that("stf extends FLStock to requested years", {
  data(ple4)
  out <- stf(ple4, nyears = 2)
  expect_equal(dims(out)$maxyear, dims(ple4)$maxyear + 2)
})

test_that("stf on FLBiol checks nyears/end consistency", {
  data(ple4)
  biol <- as(ple4, "FLBiol")
  expect_error(stf(biol, nyears = 2, end = dims(biol)$maxyear + 3), "do not match")
})
