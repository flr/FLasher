test_that("inline Cxx plugin factory is available", {
  expect_true(is.function(FLasher:::inlineCxxPlugin))
  plugin <- FLasher:::inlineCxxPlugin()
  expect_true(is.list(plugin))
  expect_true(length(plugin) > 0)
})
