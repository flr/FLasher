test_that("fwd(FLStock, ..., control missing) validates args", {
  data(ple4, package = "FLCore")

  expect_error(
    fwd(ple4, sr = rec(ple4), foo = FLQuant(1, dimnames = list(year = 2000))),
    "Names of input FLQuant"
  )

  expect_error(
    fwd(ple4, sr = rec(ple4), ctrl = fwdControl()),
    "Did you mean"
  )
})

test_that("fwdControl class has fwd methods registered", {
  expect_s4_class(
    methods::selectMethod("fwd", signature = c("FLStock", "missing", "fwdControl")),
    "MethodDefinition"
  )
  expect_s4_class(
    methods::selectMethod("fwd", signature = c("FLBiol", "FLFishery", "missing")),
    "MethodDefinition"
  )
})
