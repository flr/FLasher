test_that("jukebox returns one startup lyric", {
  lyric <- FLasher:::jukebox()
  expect_type(lyric, "character")
  expect_equal(length(lyric), 1)
  expect_true(nzchar(lyric))
})

test_that(".onAttach emits startup message", {
  expect_message(FLasher:::.onAttach("", "FLasher"), "FLasher:")
})
