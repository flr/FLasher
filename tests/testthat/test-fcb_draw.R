test_that("FCBDrawing constructor builds expected block structure", {
  fcb <- matrix(c(1, 1, 1,
                  1, 2, 1,
                  2, 1, 2), ncol = 3, byrow = TRUE)

  d <- FCBDrawing(fcb)
  expect_s4_class(d, "FCBDrawing")
  expect_equal(length(d@fisheryBlocks), 2)
  expect_equal(length(d@biolBlocks), 2)
  expect_true(length(d@catchBlocks) >= 2)
})

test_that("draw methods run without error on basic objects", {
  bb <- new("basicBlock", height = 0.1, width = 0.1, x_centre = 0.5, y_centre = 0.5, name = "x", name_cex = 1)

  tf <- tempfile(fileext = ".pdf")
  grDevices::pdf(tf)
  on.exit({
    grDevices::dev.off()
    unlink(tf)
  }, add = TRUE)

  expect_no_error(draw(bb))
  expect_no_error(draw(matrix(c(1, 1, 1), ncol = 3)))
})
