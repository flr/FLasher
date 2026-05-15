context("zzz hooks")

test_that("jukebox returns one non-empty lyric line", {
  lyric <- FLasher:::jukebox()
  expect_type(lyric, "character")
  expect_length(lyric, 1)
  expect_gt(nchar(lyric), 0)
})
