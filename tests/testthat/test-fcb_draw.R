context("fcb drawing classes and methods")

test_that("FCBDrawing constructor builds expected block counts", {
  fcb <- matrix(c(1, 1, 1, 1, 2, 2), ncol = 3, byrow = TRUE)
  drawing <- FCBDrawing(fcb)
  expect_is(drawing, "FCBDrawing")
  expect_equal(length(drawing@fisheryBlocks), 1)
  expect_equal(length(drawing@catchBlocks), 2)
  expect_equal(length(drawing@biolBlocks), 2)
})

test_that("draw method for matrix runs without error", {
  fcb <- matrix(c(1, 1, 1), ncol = 3)
  grDevices::pdf(tempfile(fileext = ".pdf"))
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_silent(draw(fcb))
})
