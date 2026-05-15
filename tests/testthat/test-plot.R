context("plot methods")

test_that("plot methods return ggplot object", {
  data(ple4)
  ctrl <- fwdControl(year = 2008:2010, quant = "f", value = 0.3)

  p_stock <- plot(ple4, ctrl)
  p_flq <- plot(ssb(ple4), ctrl)

  expect_s3_class(p_stock, "ggplot")
  expect_s3_class(p_flq, "ggplot")
})
