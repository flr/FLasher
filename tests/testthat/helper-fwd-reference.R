load_fwd_reference_results <- function() {
  ref_path <- file.path("..", "fwd_reference_results.rds")
  required_names <- c(
    "stock_catch_target",
    "stock_fbar_target",
    "stock_ssb_end_target",
    "stock_relative_catch_target",
    "stock_minmax_bounds",
    "stock_multi_iter_catch_target",
    "biols_fisheries_catch_target"
  )

  if (!file.exists(ref_path)) {
    source(file.path("..", "fwd_reference_results.R"), local = TRUE)
  }

  if (file.exists(ref_path)) {
    current <- readRDS(ref_path)
    if (!all(required_names %in% names(current))) {
      source(file.path("..", "fwd_reference_results.R"), local = TRUE)
    }
  }

  if (!file.exists(ref_path)) {
    skip("fwd reference results are not available")
  }

  out <- readRDS(ref_path)
  if (!all(required_names %in% names(out))) {
    skip("fwd reference results do not include required scenarios")
  }

  out
}
