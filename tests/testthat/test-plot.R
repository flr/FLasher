test_that("plot methods for projection overlays return ggplot objects", {
  data(ple4, package = "FLCore")

  ctrl <- fwdControl(data.frame(year = 2008:2010, quant = "f", value = 0.3))

  p1 <- plot(ple4, ctrl)
  p2 <- plot(ssb(ple4), ctrl)
  p3 <- plot(FLQuants(SSB = ssb(ple4), F = fbar(ple4)), ctrl)
  p4 <- plot(FLStocks(PLE4 = ple4), ctrl)

  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
  expect_s3_class(p3, "ggplot")
  expect_s3_class(p4, "ggplot")
})
