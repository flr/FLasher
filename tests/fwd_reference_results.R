# Script to generate baseline fwd() results for regression tests.
# Run this file manually (or from tests) to refresh the reference RDS.

suppressPackageStartupMessages(library(FLasher))

set.seed(42)

sr_geo <- function(stock) {
  predictModel(model = "geomean",
    params = FLPar(a = yearMeans(rec(stock)[, ac(2006:2008)])))
}

make_fwd_reference_results <- function(path = file.path("tests", "fwd_reference_results.rds")) {
  data(ple4)

  ple4_stf <- stf(ple4, nyears = 3)
  geomean_sr <- sr_geo(ple4)

  stock_catch_target <- fwd(
    ple4_stf,
    control = fwdControl(list(year = 2009:2011, quant = "catch",
      value = c(catch(ple4)[, "2008"]) * c(0.95, 0.9, 0.85))),
    sr = geomean_sr)

  stock_fbar_target <- fwd(
    ple4_stf,
    control = fwdControl(list(year = 2009:2011, quant = "fbar", value = 0.25)),
    sr = geomean_sr)

  stock_ssb_end_target <- fwd(
    ple4_stf,
    control = fwdControl(list(year = 2009, quant = "ssb_end", value = 120000)),
    sr = geomean_sr)

  stock_relative_catch_target <- fwd(
    ple4_stf,
    control = fwdControl(list(year = 2009:2011, quant = "catch",
      value = c(0.95, 0.95, 0.95), relYear = 2008:2010)),
    sr = geomean_sr)

  stock_minmax_bounds <- fwd(
    ple4_stf,
    control = fwdControl(
      list(year = 2009:2011, quant = "f", value = c(0.3, 0.25, 0.2)),
      list(year = 2009:2011, quant = "catch", min = 1000, max = 1e9)
    ),
    sr = geomean_sr)

  ple4_iter <- propagate(ple4_stf, 3)
  stock_multi_iter_catch_target <- fwd(
    ple4_iter,
    control = fwdControl(list(year = 2009:2011, quant = "catch",
      value = rep(c(catch(ple4)[, "2008"]) * c(0.8, 0.75, 0.7), 3))),
    sr = geomean_sr)

  data(mixed_fishery_example_om)
  fcb_single <- matrix(1, nrow = 1, ncol = 3, dimnames = list(1, c("F", "C", "B")))
  flfs_single <- FLFisheries(bt = FLFishery(pleBT = flfs[["bt"]][["pleBT"]]))
  flfs_single[["bt"]]@effort[] <- 1
  biols_single <- FLBiols(ple = biols[["ple"]])

  biols_fisheries_catch_target <- fwd(
    object = biols_single,
    fishery = flfs_single,
    control = fwdControl(list(year = 2:4, quant = "catch", biol = "ple", value = 100000),
      FCB = fcb_single))

  ref_results <- list(
    metadata = list(seed = 42, created = as.character(Sys.time())),
    stock_catch_target = stock_catch_target,
    stock_fbar_target = stock_fbar_target,
    stock_ssb_end_target = stock_ssb_end_target,
    stock_relative_catch_target = stock_relative_catch_target,
    stock_minmax_bounds = stock_minmax_bounds,
    stock_multi_iter_catch_target = stock_multi_iter_catch_target,
    biols_fisheries_catch_target = biols_fisheries_catch_target
  )

  saveRDS(ref_results, path)
  invisible(ref_results)
}

make_fwd_reference_results()
