# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Test operating model projection methods")
source("expect_funs.R")

# Have already tested recruitment and F methods
# Need to test that survivors are calculated correctly and are placed in the correct age group

# Test annual model
# Test seasonal model - 1 unit
# Test seasonal model - multiple unit

test_that("operatingModel annual project_biol", {
    FCB <- array(c(1,1,1), dim=c(1,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    # Annual model
    min_age <- 1
    niters <- 20
    om <- make_test_operatingModel(ple4, FCB, nseasons = 1, recruitment_age = min_age, niters = niters, sd = 0.1)
    # Project a random year
    dms <- dim(n(om[["biols"]][[1]][["biol"]]))
    year <- round(runif(1,min=min_age+1, max= dms[2]))
    prev_indices_min <- c(1,year-1,1,1,1,1)
    prev_indices_max <- c(dms[1],year-1,dms[3],dms[4],dms[5],dms[6])
    indices_min <- c(1,year,1,1,1,1)
    indices_max <- c(dms[1],year,dms[3],dms[4],dms[5],dms[6])
    # Get F
    fin <- test_operatingModel_get_f_B_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, prev_indices_min,  prev_indices_max)
    zin <- fin + m(om[["biols"]][[1]][["biol"]])[1:dms[1], year-1, 1:dms[3], 1:dms[4], 1:dms[5], 1:dms[6]]
    survivors <- n(om[["biols"]][[1]][["biol"]])[1:dms[1], year-1, 1:dms[3], 1:dms[4], 1:dms[5], 1:dms[6]] * exp(-zin)
    # pgroup etc
    nextn <- survivors
    nextn[2:dms[1],] <- survivors[1:(dms[1]-1),]
    nextn[dms[1],] <- nextn[dms[1],] + survivors[dms[1],]
    # Test recruitment - already tested elsewhere - just confirm here
    rec_out <- test_operatingModel_calc_rec(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 1, year)
    nextn[1,] <- rec_out
    om_out <- test_operatingModel_project_biols(om[["fisheries"]], om[["biols"]], om[["fwc"]], year)
    nextn_out <- n(om_out[["biols"]][[1]])[,year,]
    expect_FLQuant_equal(nextn_out, nextn)
})


test_that("operatingModel seasonal project_biol single unit", {
    FCB <- array(c(1,1,1), dim=c(1,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    min_age <- 1
    niters <- 20
    nseasons <- 4
    for (rec_season in 1:nseasons){
        om <- make_test_operatingModel(ple4, FCB, nseasons = nseasons, recruitment_seasons = rec_season, recruitment_age = min_age, niters = niters, sd = 0.1)
        dms <- dim(n(om[["biols"]][[1]][["biol"]]))
        year <- round(runif(1,min=min_age+1, max= dms[2]))
        for (season in 1:nseasons){
            indices_min <- c(1,year,1,season,1,1)
            indices_max <- c(dms[1],year,dms[3],season,dms[5],dms[6])
            if (season == 1){
                prev_indices_min <- c(1,year-1,1,nseasons,1,1)
                prev_indices_max <- c(dms[1],year-1,dms[3],nseasons,dms[5],dms[6])
            }
            if (season > 1) {
                prev_indices_min <- c(1,year,1,season-1,1,1)
                prev_indices_max <- c(dms[1],year,dms[3],season-1,dms[5],dms[6])
            }
            # Get F
            fin <- test_operatingModel_get_f_B_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, prev_indices_min,  prev_indices_max)
            zin <- fin + m(om[["biols"]][[1]][["biol"]])[1:dms[1], prev_indices_min[2], 1:dms[3], prev_indices_min[4], 1:dms[5], 1:dms[6]]
            # Survivors
            survivors <- n(om[["biols"]][[1]][["biol"]])[1:dms[1], prev_indices_min[2], 1:dms[3], prev_indices_min[4], 1:dms[5], 1:dms[6]] * exp(-zin)
            timestep <- ((year-1)*nseasons) + season
            om_out <- test_operatingModel_project_biols(om[["fisheries"]], om[["biols"]], om[["fwc"]], timestep)
            nextn_out <- n(om_out[["biols"]][[1]])[,year,,season]
            nextn <- survivors
            if(rec_season == season){
                # Birthday! pgroup etc
                nextn[2:dms[1],] <- survivors[1:(dms[1]-1),]
                nextn[dms[1],] <- nextn[dms[1],] + survivors[dms[1],]
                # Test recruitment - already tested elsewhere - just confirm here
                rec_out <- test_operatingModel_calc_rec(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 1, timestep)
                nextn[1,] <- rec_out
            }
            expect_FLQuant_equal(nextn_out, nextn)
        }
    }
})

test_that("operatingModel seasonal project_biol multiple units", {
    FCB <- array(c(1,1,1), dim=c(1,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    min_age <- 1
    niters <- 20
    nseasons <- 4
    no_rec_seasons <- 2
    rec_seasons <- sort(sample(1:nseasons, no_rec_seasons))
    om <- make_test_operatingModel(ple4, FCB, nseasons = nseasons, recruitment_seasons = rec_seasons, recruitment_age = min_age, niters = niters, sd = 0.1)
    dms <- dim(n(om[["biols"]][[1]][["biol"]]))
    year <- round(runif(1,min=min_age+1, max= dms[2]))
    for (season in 1:nseasons){
        indices_min <- c(1,year,1,season,1,1)
        indices_max <- c(dms[1],year,dms[3],season,dms[5],dms[6])
        if (season == 1){
            prev_indices_min <- c(1,year-1,1,nseasons,1,1)
            prev_indices_max <- c(dms[1],year-1,dms[3],nseasons,dms[5],dms[6])
        }
        if (season > 1) {
            prev_indices_min <- c(1,year,1,season-1,1,1)
            prev_indices_max <- c(dms[1],year,dms[3],season-1,dms[5],dms[6])
        }
        # Get F
        fin <- test_operatingModel_get_f_B_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, prev_indices_min,  prev_indices_max)
        zin <- fin + m(om[["biols"]][[1]][["biol"]])[1:dms[1], prev_indices_min[2], 1:dms[3], prev_indices_min[4], 1:dms[5], 1:dms[6]]
        # Survivors
        survivors <- n(om[["biols"]][[1]][["biol"]])[1:dms[1], prev_indices_min[2], 1:dms[3], prev_indices_min[4], 1:dms[5], 1:dms[6]] * exp(-zin)
        timestep <- ((year-1)*nseasons) + season
        om_out <- test_operatingModel_project_biols(om[["fisheries"]], om[["biols"]], om[["fwc"]], timestep)
        nextn_out <- n(om_out[["biols"]][[1]])[,year,,season]
        nextn <- survivors
        for (unit in 1:no_rec_seasons){
            if(rec_seasons[unit] == season){
                # Birthday! pgroup etc
                nextn[2:dms[1],,unit] <- survivors[1:(dms[1]-1),,unit]
                nextn[dms[1],,unit] <- nextn[dms[1],,unit] + survivors[dms[1],,unit]
                # Test recruitment - already tested elsewhere - just confirm here
                rec_out <- test_operatingModel_calc_rec(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, unit, timestep)
                nextn[1,,unit] <- rec_out
            }
        }
        expect_FLQuant_equal(nextn_out, nextn)
    }
})

# Project fisheries
# Fisheries do not care about the season / recruitment unit thing
# Make a seasonal model based on the complicated scenario with 2 fisheries / 2 catches
# Include units just to make sure
test_that("operatingModel seasonal project_fisheries", {
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    min_age <- 1
    niters <- 20
    nseasons <- 4
    no_rec_seasons <- 2
    rec_seasons <- sort(sample(1:nseasons, no_rec_seasons))
    om <- make_test_operatingModel(ple4, FCB, nseasons = nseasons, recruitment_seasons = rec_seasons, recruitment_age = min_age, niters = niters, sd = 0.1)
    dms <- dim(n(om[["biols"]][[1]][["biol"]]))
    year <- round(runif(1,min=min_age+1, max= dms[2]))
    season <- round(runif(1,min=1, max= dms[4]))
    indices_min <- c(1,year,1,season,1,1)
    indices_max <- c(dms[1],year,dms[3],season,dms[5],dms[6])
    # Project fisheries
    om_out <- test_operatingModel_project_fisheries(om[["fisheries"]], om[["biols"]], om[["fwc"]], ((year-1)*nseasons)+nseasons)
    # Get catch etc of F1C1
    fishery <- 1
    catch <- 1
    biol <- 1
    fin111 <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery, catch, biol, indices_min, indices_max)
    min1 <- m(om[["biols"]][[biol]][["biol"]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    zin1 <- fin111 + min1
    nin1 <- n(om[["biols"]][[biol]][["biol"]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    dr11 <- (discards.n(om_out[["fisheries"]][[fishery]][[catch]]) / catch.n(om_out[["fisheries"]][[fishery]][[catch]]))[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    catch_in111 <- (fin111 / zin1) * (1 - exp(-zin1)) * nin1
    catch_out11 <- catch.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(catch_in111, catch_out11)
    discards_in111 <- catch_in111 * dr11
    discards_out11 <- discards.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(discards_in111, discards_out11)
    landings_in111 <- catch_in111 * (1-dr11)
    landings_out11 <- landings.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(landings_in111, landings_out11)
    # Get catch etc of F1C2 - shared with F2C1
    fishery <- 1
    catch <- 2
    biol <- 2
    fin122 <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, biol, indices_min, indices_max)
    fin212 <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 1, biol, indices_min, indices_max)
    min2 <- m(om[["biols"]][[biol]][["biol"]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    zin2 <- fin122 + fin212 + min2
    nin2 <- n(om[["biols"]][[biol]][["biol"]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    dr12 <- (discards.n(om_out[["fisheries"]][[fishery]][[catch]]) / catch.n(om_out[["fisheries"]][[fishery]][[catch]]))[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    catch_in122 <- (fin122 / zin2) * (1 - exp(-zin2)) * nin2
    catch_out12 <- catch.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(catch_in122, catch_out12)
    discards_in122 <- catch_in122 * dr12
    discards_out12 <- discards.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(discards_in122, discards_out12)
    landings_in122 <- catch_in122 * (1-dr12)
    landings_out12 <- landings.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(landings_in122, landings_out12)
    # Get catch etc of F2C1 - shared with F1C2
    fishery <- 2
    catch <- 1
    biol <- 2
    dr21 <- (discards.n(om_out[["fisheries"]][[fishery]][[catch]]) / catch.n(om_out[["fisheries"]][[fishery]][[catch]]))[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    catch_in212 <- (fin212 / zin2) * (1 - exp(-zin2)) * nin2
    catch_out21 <- catch.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(catch_in212, catch_out21)
    discards_in212 <- catch_in212 * dr21
    discards_out21 <- discards.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(discards_in212, discards_out21)
    landings_in212 <- catch_in212 * (1-dr21)
    landings_out21 <- landings.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(landings_in212, landings_out21)
    # Get catch etc of F2C2 - fishes two biols
    fishery <- 2
    catch <- 2
    fin223 <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 3, indices_min, indices_max)
    fin224 <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 4, indices_min, indices_max)
    min3 <- m(om[["biols"]][[3]][["biol"]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    min4 <- m(om[["biols"]][[4]][["biol"]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    zin3 <- fin223 + min3
    zin4 <- fin224 + min4
    nin3 <- n(om[["biols"]][[3]][["biol"]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    nin4 <- n(om[["biols"]][[4]][["biol"]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    dr22 <- (discards.n(om_out[["fisheries"]][[fishery]][[catch]]) / catch.n(om_out[["fisheries"]][[fishery]][[catch]]))[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    catch_in223 <- (fin223 / zin3) * (1 - exp(-zin3)) * nin3
    catch_in224 <- (fin224 / zin4) * (1 - exp(-zin4)) * nin4
    catch_out22 <- catch.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(catch_in223 + catch_in224, catch_out22)
    discards_in223 <- catch_in223 * dr22
    discards_in224 <- catch_in224 * dr22
    discards_out22 <- discards.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(discards_in223 + discards_in224, discards_out22)
    landings_in223 <- catch_in223 * (1-dr22)
    landings_in224 <- catch_in224 * (1-dr22)
    landings_out22 <- landings.n(om_out[["fisheries"]][[fishery]][[catch]])[1:dms[1], indices_min[2], 1:dms[3], indices_min[4], 1:dms[5], 1:dms[6]]
    expect_FLQuant_equal(landings_in223 + landings_in224, landings_out22)
})
