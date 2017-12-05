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
    # No F on sole is greater than sole_f_limit
    expect_true(all(solfbar < (sole_f_limit + 1e-6)))
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
    expect_true(all(revbt > (bt_min_revenue - 1e-6)))
    expect_true(all(revgn > (gn_min_revenue - 1e-6)))
})

test_that("Single fishery, two catches, single TAC on two Biols",{
    data(mixed_fishery_example_om)
    years <- 2:20
    bt <- flfs[["bt"]]
    flfs2 <- FLFisheries(bt=bt)
    fcb <- matrix(c(1,1,1,2,1,2), nrow=2, ncol=3, dimnames=list(1:2,c("F","C","B")))
    combined_catch_target <- 250000
    # biol by number
    ctrl <- fwdControl(list(year=years, quant="catch", value=combined_catch_target, biol=G(1,2)), FCB=fcb)
    test <- fwd(object=biols, fishery=flfs2, control=ctrl)
    expect_equal(c((catch(test[["fisheries"]][[1]][[1]]) + catch(test[["fisheries"]][[1]][[2]]))[,ac(years)]), rep(combined_catch_target, length(years)))
    # biol by name
    ctrl <- fwdControl(list(year=years, quant="catch", value=combined_catch_target, biol=G("ple","sol")), FCB=fcb)
    test <- fwd(object=biols, fishery=flfs2, control=ctrl)
    expect_equal(c((catch(test[["fisheries"]][[1]][[1]]) + catch(test[["fisheries"]][[1]][[2]]))[,ac(years)]), rep(combined_catch_target, length(years)))
    # Relative TAC
    rel_combined_TAC <- 0.9
    ctrl <- fwdControl(list(year=years, quant="catch", value=rel_combined_TAC, biol=G("ple","sol"), relYear=years-1, relBiol=G("ple","sol")), FCB=fcb)
    test <- fwd(object=biols, fishery=flfs2, control=ctrl)
    cout <- (catch(test[["fisheries"]][[1]][[1]]) + catch(test[["fisheries"]][[1]][[2]]))
    expect_equal(c(cout[,ac(years)] / cout[,ac(years-1)]), rep(rel_combined_TAC, length(years)), tolerance=1e-6)
    # Wrong names in control
    ctrl <- fwdControl(list(year=years, quant="catch", value=rel_combined_TAC, biol=G("plaice","sole"), relYear=years-1, relBiol=G("ple","sol")), FCB=fcb)
    expect_error(fwd(object=biols, fishery=flfs2, control=ctrl))
})


test_that("Two fisheries, one catch each, joint catch",{
    data(mixed_fishery_example_om)
    bt <- FLFishery(solBT=flfs[["bt"]][["solBT"]], desc="")
    bt@effort[] <- 1
    gn <- FLFishery(solGN=flfs[["gn"]][["solGN"]], desc="")
    gn@effort[] <- 1
    flfs <- FLFisheries(bt=bt, gn=gn)
    biols <- FLBiols(sol=biols[["sol"]])
    sole_catch <- 10000
    rel_catch <- 0.9
    fcb <- matrix(c(1,2,1,1,1,1), nrow=2, ncol=3, dimnames=list(1:2,c("F","C","B")))
    years <- 2:20
    ctrl <- fwdControl(
        list(year=years, quant="catch", fishery=G("bt","gn"), catch=G("solBT", "solGN"), value=sole_catch),
        list(year=years, relYear=years, quant="catch", fishery = "bt", relFishery="gn", catch="solBT", relCatch="solGN", value=rel_catch),
        FCB = fcb)
    test <- fwd(object=biols, fishery=flfs, control=ctrl)
    c11 <- catch(test[["fisheries"]][[1]][[1]])
    c22 <- catch(test[["fisheries"]][[2]][[1]])
    expect_equal(c((c11+c22)[,ac(years)]),rep(sole_catch, length(years)))
    expect_equal(c((c11/c22)[,ac(years)]),rep(rel_catch, length(years)))
    # Test with non matching names in fishery and catch
    # Catch names in wrong order
    ctrl <- fwdControl(
        list(year=years, quant="catch", fishery=G("bt","gn"), catch=G("solGN", "solBT"), value=sole_catch),
        list(year=years, relYear=years, quant="catch", fishery = "bt", relFishery="gn", catch="solBT", relCatch="solGN", value=rel_catch),
        FCB = fcb)
    expect_error(fwd(object=biols, fishery=flfs, control=ctrl))
})

test_that("Two fisheries, two catches each, joint catch",{
    data(mixed_fishery_example_om)
    rel_sol_catch <- 0.8
    ple_f <- 0.15 # set sum of partial Fs
    fcb <- matrix(c(1,1,1,1,2,2,2,1,1,2,2,2), byrow=TRUE, ncol=3, dimnames=list(1:4,c("F","C","B")))
    years <- 2:20
    # Total F target on plaice, relative catch on sole catches
    ctrl <- fwdControl(
        list(year=years, quant="f", minAge=2, maxAge=6, value=ple_f, fishery=G("bt","gn"), catch=G("pleBT", "pleGN"), biol="ple"), # need to specify biol too, bit awkward
        list(year=years, quant="catch", value = 0.8, fishery="bt", catch="solBT", relYear=years, relFishery="gn", relCatch="solGN"),
        FCB=fcb)
    test <- fwd(object=biols, fishery=flfs, control=ctrl)
    solc1 <- catch(test[["fisheries"]][["bt"]][["solBT"]])
    solc2 <- catch(test[["fisheries"]][["gn"]][["solGN"]])
    expect_equal(c((solc1/solc2)[,ac(years)]), rep(rel_sol_catch, length(years)))
    plef1 <- FLasher:::calc_F(test[["fisheries"]][["bt"]][["pleBT"]], test[["biols"]][["ple"]], test[["fisheries"]][["bt"]]@effort)
    plef2 <- FLasher:::calc_F(test[["fisheries"]][["gn"]][["pleGN"]], test[["biols"]][["ple"]], test[["fisheries"]][["gn"]]@effort)
    plefbar <- c(apply((plef1+plef2)[ac(2:6),ac(years)], 2:6, mean))
    expect_equal(c(plefbar), rep(ple_f, length(years)))
})
