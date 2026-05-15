if (identical(Sys.getenv("FLASHER_RECORD_TEST_OUTPUT"), "true")) {
  library(testthat)

  out <- test_dir("testthat", reporter = "summary")
  saveRDS(out, file = file.path("tests", "test-results.rds"))
}
