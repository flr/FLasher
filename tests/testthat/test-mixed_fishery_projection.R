# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Mixed fishery projections")
# If these don't work we are in trouble!
# Adapted from the Mixed Fisheries tutorial
source("expect_funs.R")

test_that("Single fishery, single biol",{
    data(mixed_fishery_example_om)
    pleBT <- flfs[["bt"]][["pleBT"]]
    bt1 <- FLFishery(pleBT=pleBT)
    bt1@effort[] <- 1
    flfs1 <- FLFisheries(bt=bt1)
    biols1 <- FLBiols(ple=biols[["ple"]])
    fcb <- matrix(1, nrow=1, ncol=3, dimnames=list(1,c("F","C","B")))
    catch_target <- 100000
    years <- 2:20
    flasher_ctrl <- fwdControl(list(year=years, quant="catch", biol="ple", value=catch_target), FCB = fcb)
    test <- fwd(object=biols1, fishery=flfs1, control=flasher_ctrl)
    expect_equal(rep(catch_target, length(years)), c(catch(test[["fisheries"]][[1]][[1]])[,ac(years)]))
})

test_that("Single fishery, two biols",{
    data(mixed_fishery_example_om)
    bt <- flfs[["bt"]]
    flfs2 <- FLFisheries(bt=bt)
    fcb <- matrix(c(1,1,1,2,1,2), nrow=2, ncol=3, dimnames=list(1:2,c("F","C","B")))
    plaice_catch_target <- 250000
    # Fixed plaice catch
    years <- 2:20
    flasher_ctrl <- fwdControl(list(year = years, quant = "catch", biol = "ple", value = plaice_catch_target), FCB = fcb)
    test <- fwd(object=biols, fishery=flfs2, control=flasher_ctrl)
    expect_equal(rep(plaice_catch_target, length(years)), c(catch(test[["fisheries"]][["bt"]][["pleBT"]])[,ac(years)]))
    # With max sole F limit
    sole_f_limit <- 0.2
    flasher_ctrl <- fwdControl(
        list(year = 2:20, quant = "catch", biol = "ple", value = plaice_catch_target),
        list(year = 2:20, quant = "f", biol = "sol", max = sole_f_limit, minAge=2, maxAge=6),
        FCB = fcb)
    test <- fwd(object=biols, fishery=flfs2, control=flasher_ctrl)
    # Test max F of sole
    solef <- FLasher:::calc_F(test[["fisheries"]][["bt"]][["solBT"]], test[["biols"]][["sol"]], test[["fisheries"]][["bt"]]@effort)
    solfbar <- c(apply(solef[ac(2:6),ac(years)], 2:6, mean))
    # Tricky with max test - tolerance of 1e-6
    expect_true(all(abs((solfbar - sole_f_limit)[(solfbar - sole_f_limit >= 0)]) < 1e-6))
})

test_that("Two fisheries, one biol",{
    data(mixed_fishery_example_om)
    bt3 <- FLFishery(solBT=flfs[["bt"]][["solBT"]], desc="")
    bt3@effort[] <- 1
    gn3 <- FLFishery(solGN=flfs[["gn"]][["solGN"]], desc="")
    gn3@effort[] <- 1
    flfs3 <- FLFisheries(bt=bt3, gn=gn3)
    biols3 <- FLBiols(sol=biols[["sol"]])
    sole_bt_catch <- 10000
    sole_gn_catch <- 5000
    fcb <- matrix(c(1,2,1,1,1,1), nrow=2, ncol=3, dimnames=list(1:2,c("F","C","B")))
    years <- 2:20
    flasher_ctrl <- fwdControl(
        list(year=years, quant="catch", fishery="bt", catch="solBT", value=sole_bt_catch),
        list(year=years, quant="catch", fishery="gn", catch="solGN", value=sole_gn_catch),
        FCB = fcb)
    test <- fwd(object=biols3, fishery=flfs3, control=flasher_ctrl)
    expect_equal(rep(sole_bt_catch, length(years)), c(catch(test[["fisheries"]][["bt"]][["solBT"]])[,ac(years)]))
    expect_equal(rep(sole_gn_catch, length(years)), c(catch(test[["fisheries"]][["gn"]][["solGN"]])[,ac(years)]))
    # Relative between the catches
    sole_catch_target <- 12000
    sole_bt_gn_catch_relative <- 2
    flasher_ctrl <-fwdControl(list(year=years, quant="catch", biol="sol", value=sole_catch_target),
                           list(year=years, quant="catch",relYear=2:20, fishery="bt", catch="solBT", relFishery="gn", relCatch="solGN", value=sole_bt_gn_catch_relative),
                           FCB=fcb)
    test <- fwd(object=biols3, fishery=flfs3, control=flasher_ctrl)
    expect_equal(rep(sole_catch_target, length(years)), c((catch(test[["fisheries"]][["bt"]][["solBT"]])+catch(test[["fisheries"]][["gn"]][["solGN"]]))[,ac(years)]))
    expect_equal(rep(sole_bt_gn_catch_relative, length(years)), c((catch(test[["fisheries"]][["bt"]][["solBT"]]) / catch(test[["fisheries"]][["gn"]][["solGN"]]))[,ac(years)]))
})

test_that("Two fisheries, two biols",{
    data(mixed_fishery_example_om)
    years <- 2:20
    fcb <- matrix(c(1,1,1,1,2,2,2,1,1,2,2,2), byrow=TRUE, ncol=3, dimnames=list(1:4,c("F","C","B")))
    sole_catch_target <- 12000
    plaice_bt_gn_catch_relative <- 1.5
    flasher_ctrl <- fwdControl(list(year=years, quant="catch",biol="sol",value=sole_catch_target),
               list(year=years, quant="catch", relYear=2:20, fishery="bt", catch="pleBT", relFishery="gn", relCatch="pleGN", value=plaice_bt_gn_catch_relative),
               FCB=fcb)
    test <- fwd(object=biols, fishery=flfs, control=flasher_ctrl)
    expect_equal(rep(sole_catch_target, length(years)), c((catch(test[["fisheries"]][["bt"]][["solBT"]])+catch(test[["fisheries"]][["gn"]][["solGN"]]))[,ac(years)]))
    expect_equal(rep(plaice_bt_gn_catch_relative, length(years)), c((catch(test[["fisheries"]][["bt"]][["pleBT"]]) / catch(test[["fisheries"]][["gn"]][["pleGN"]]))[,ac(years)]))
    # Decreasing sole catch
    sole_catch_target_initial <- 20000
    sole_catch_decrease <- 0.9
    plaice_bt_gn_catch_relative <- 1.5
    flasher_ctrl <- fwdControl(
        list(year=2, quant="catch", biol="sol", value=sole_catch_target_initial),
        list(year=years[-1], quant="catch", relYear=2:19, biol="sol", relBiol="sol", value=sole_catch_decrease),
        list(year=years, quant="catch", relYear=2:20, fishery="bt", catch="pleBT", relFishery="gn", relCatch="pleGN",value=plaice_bt_gn_catch_relative),
        FCB=fcb)
    test <- fwd(object=biols, fishery=flfs, control=flasher_ctrl)
    solecatch <- c((catch(test[["fisheries"]][["bt"]][["solBT"]])+catch(test[["fisheries"]][["gn"]][["solGN"]]))[,ac(years)])
    expect_equal(solecatch, c(sole_catch_target_initial, sole_catch_target_initial * 0.9^(1:(length(years)-1))))
    expect_equal(rep(plaice_bt_gn_catch_relative, length(years)), c((catch(test[["fisheries"]][["bt"]][["pleBT"]]) / catch(test[["fisheries"]][["gn"]][["pleGN"]]))[,ac(years)]))
})

test_that("Two fisheries, two biols, economic target",{
    data(mixed_fishery_example_om)
    years <- 2:20
    sole_catch_target_initial <- 20000
    sole_catch_decrease <- 0.9
    plaice_bt_gn_catch_relative <- 1.5
    bt_min_revenue <- 150000
    gn_min_revenue <- 100000
    fcb <- matrix(c(1,1,1,1,2,2,2,1,1,2,2,2), byrow=TRUE, ncol=3, dimnames=list(1:4,c("F","C","B")))
    flasher_ctrl <- fwdControl(
        list(year=2, quant="catch", biol="sol", value=sole_catch_target_initial),
        list(year=years[-1], quant="catch", relYear=2:19, biol="sol", relBiol="sol", value=sole_catch_decrease),
        list(year=years, quant="catch", relYear=2:20, fishery="bt", catch="pleBT", relFishery="gn", relCatch="pleGN",value=plaice_bt_gn_catch_relative),
        list(year=years, quant="revenue", fishery="bt", min=bt_min_revenue),
        list(year=years, quant="revenue", fishery="gn", min=gn_min_revenue),
        FCB=fcb)
    test <- fwd(object=biols, fishery=flfs, control=flasher_ctrl)
    # Check min revenue for bt and gn - tolerance of 1e-6
    revgn <- c(Reduce("+",lapply(test[["fisheries"]][["gn"]], function(x){ quantSums(landings.n(x) * landings.wt(x) * price(x)) }))[,ac(years)])
    revbt <- c(Reduce("+",lapply(test[["fisheries"]][["bt"]], function(x){ quantSums(landings.n(x) * landings.wt(x) * price(x)) }))[,ac(years)])
    expect_true(all(abs((bt_min_revenue - revbt)[(bt_min_revenue - revbt >= 0)]) < 1e-6))
    expect_true(all(abs((gn_min_revenue - revgn)[(gn_min_revenue - revgn >= 0)]) < 1e-6))
})


