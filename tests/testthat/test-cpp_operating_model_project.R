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
    indices_min <- c(1,year,1,1,1,1)
    indices_max <- c(dms[1],year,dms[3],dms[4],dms[5],dms[6])
    # Get F
    fin <- test_operatingModel_get_f_B_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, indices_min,  indices_max)
    zin <- fin + m(om[["biols"]][[1]][["biol"]])[1:dms[1],year,1:dms[3], 1:dms[4], 1:dms[5], 1:dms[6]]
    survivors <- n(om[["biols"]][[1]][["biol"]])[1:dms[1],year,1:dms[3], 1:dms[4], 1:dms[5], 1:dms[6]] * exp(-zin)
    # pgroup etc
    nextn <- survivors
    nextn[2:dms[1],] <- survivors[1:(dms[1]-1),]
    nextn[dms[1],] <- nextn[dms[1],] + survivors[dms[1],]

    om_out <- test_operatingModel_project_biols(om[["fisheries"]], om[["biols"]], om[["fwc"]], year)
    n(om_out[["biols"]][[1]])[,year,]
    nextn
    # oops



}

test_that("operatingModel project_biol", {
    # Assume get_f method works (it does...)
    # Single biol fished by one catch
    # Seasonal Biol with 4 units (M / F and 2 spawning morphs)
    # Min age = 1
    #flq <- random_FLQuant_generator(fixed_dims = c(5,6,4,4,1,10))
    flq <- random_FLQuant_generator(fixed_dims = c(5,6,1,4,1,10))
    flbs <- random_fwdBiols_list_generator(min_biols = 1, max_biols = 1, fixed_dims = dim(flq))
    # Set SR params - same every year but broken down by unit and season - necessary
    # Units 1 and 2 recruit in season 1
    # Units 3 and 4 recruit in season 3
    #srr_params <- FLQuant(NA, dim=c(2,1,4,4,1,10))
    #srr_params[,,c(1,2),1,] <- rnorm(40)
    #srr_params[,,c(3,4),3,] <- rnorm(40) 
    srr_params <- FLQuant(NA, dim=c(2,1,1,4,1,10))
    srr_params[,,1,3,] <- rnorm(20)
    flbs[[1]][["srr_params"]] <- srr_params
    flbs[[1]][["srr_model_name"]] <- "bevholt"
    flbs[[1]][["srr_residuals_mult"]] <- TRUE
    # Pull out just FLBiol for testing
    flb_in <- flbs[[1]][["biol"]]
    # Make fishery - single catch
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=1, max_fisheries=1, min_catches=1, max_catches=1)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Fix FCB
    FCB <- array(NA, dim=c(1,3))
    FCB[1,] <- c(1,1,1)
    fc@FCB <- FCB
    # Project in each season in a random year
    year <- round(runif(1,min=2,dim(flq)[2]))
    # Season 1
    season <- 1
    timestep <- test_year_season_to_timestep(flq, year, season)
    om_out <- test_operatingModel_project_biols(flfs, flbs, fc, timestep)
    fout <- test_operatingModel_get_f_B(flfs, flbs, fc, 1)[,year-1,,4,]
    old_n <- n(flb_in)[,year-1,,4,,]
    survivors <- old_n * exp(-(fout + m(flb_in)[,year-1,,4,]))
    new_n_out <- n(om_out[["biols"]][[1]])[,year,,1,]
    # Check each unit
    # Unit 1 and 2 should have birthday!
    #for (unit in c(1,2)){
    #    expect_FLQuant_equal(new_n_out[2:4,,unit,], survivors[1:3,,unit])
    #    expect_FLQuant_equal(new_n_out[5,,unit], survivors[4,,unit]+survivors[5,,unit])
    #    # Recruitment for units 1 and 2
    #    a <- srr_params[1,1,,season]
    #    b <- srr_params[2,1,,season]
    #    srp_in <- test_operatingModel_SRP_FLQ_subset(flfs, flbs, fc, 1, c(year-1,1,4,1,1), c(year-1,4,4,1,10))
    #    rec_in <- (a * srp_in / (srp_in + b)) * flbs[[1]][["srr_residuals"]][,year,,1,]
    #    expect_FLQuant_equal(rec_in[,,c(1,2)], new_n_out[1,,c(1,2)])
    #}
    #for (unit in c(3,4)){
    for (unit in 1){
        expect_FLQuant_equal(new_n_out[,,unit], survivors[,,unit])
    }
    # Season 2
    season <- 2
    timestep <- test_year_season_to_timestep(flq, year, season)
    om_out <- test_operatingModel_project_biols(flfs, flbs, fc, timestep)
    fout <- test_operatingModel_get_f_B(flfs, flbs, fc, 1)[,year,,season-1,]
    old_n <- n(flb_in)[,year,,season-1,,]
    survivors <- old_n * exp(-(fout + m(flb_in)[,year,,season-1,]))
    new_n_out <- n(om_out[["biols"]][[1]])[,year,,season,]
    # Check each unit at the same time - no recruitment
    expect_FLQuant_equal(new_n_out, survivors)
    # Season 3
    season <- 3
    timestep <- test_year_season_to_timestep(flq, year, season)
    om_out <- test_operatingModel_project_biols(flfs, flbs, fc, timestep)
    fout <- test_operatingModel_get_f_B(flfs, flbs, fc, 1)[,year,,season-1,]
    old_n <- n(flb_in)[,year,,season-1,,]
    survivors <- old_n * exp(-(fout + m(flb_in)[,year,,season-1,]))
    new_n_out <- n(om_out[["biols"]][[1]])[,year,,season,]
    # Check each unit
    #for (unit in c(1,2)){
    #    expect_FLQuant_equal(new_n_out[,,unit], survivors[,,unit])
    #}
    ## Unit 3 and 4 should have birthday!
    #for (unit in c(3,4)){
    for (unit in c(1)){
        expect_FLQuant_equal(new_n_out[2:4,,unit,], survivors[1:3,,unit])
        expect_FLQuant_equal(new_n_out[5,,unit], survivors[4,,unit]+survivors[5,,unit])
        # Recruitment for units 3 and 4
        a <- srr_params[1,1,,season]
        b <- srr_params[2,1,,season]
        #srp_in <- test_operatingModel_SRP_FLQ_subset(flfs, flbs, fc, 1, c(year,1,3,1,1), c(year,4,3,1,10))
        srp_in <- test_operatingModel_SRP_FLQ_subset(flfs, flbs, fc, 1, c(year,1,3,1,1), c(year,1,3,1,10))
        rec_in <- (a * srp_in / (srp_in + b)) * flbs[[1]][["srr_residuals"]][,year,,3,]
        #expect_FLQuant_equal(rec_in[,,c(3,4)], new_n_out[1,,c(3,4)])
        expect_FLQuant_equal(rec_in[,,c(1)], new_n_out[1,,c(1)])
    }
    # Season 4
    season <- 4
    timestep <- test_year_season_to_timestep(flq, year, season)
    om_out <- test_operatingModel_project_biols(flfs, flbs, fc, timestep)
    fout <- test_operatingModel_get_f_B(flfs, flbs, fc, 1)[,year,,season-1,]
    old_n <- n(flb_in)[,year,,season-1,,]
    survivors <- old_n * exp(-(fout + m(flb_in)[,year,,season-1,]))
    new_n_out <- n(om_out[["biols"]][[1]])[,year,,season,]
    # Check each unit at the same time - no recruitment
    expect_FLQuant_equal(new_n_out, survivors)
})

test_that("operatingModel project_fisheries", {
    # Seasonal FLQ with Units
    # Assume get_f method works (it does...)
    # FLB with 2 units - second biol only
    #flq_unit <- random_FLQuant_generator(fixed_dims = c(5,6,2,4,1,10))
    #flbs_unit <- random_fwdBiols_list_generator(min_biols = 1, max_biols = 1, fixed_dims = dim(flq_unit))
    flq_unit <- random_FLQuant_generator(fixed_dims = c(5,6,1,4,1,10))
    flbs_unit <- random_fwdBiols_list_generator(min_biols = 1, max_biols = 1, fixed_dims = dim(flq_unit))
    # Other FLB with 1 unit
    flq <- random_FLQuant_generator(fixed_dims = c(5,6,1,4,1,10))
    flbs <- random_fwdBiols_list_generator(min_biols = 3, max_biols = 3, fixed_dims = dim(flq))
    # Insert FLB 2 into correct place
    flbs[["the 4th"]] <- flbs[[3]]
    flbs[[3]] <- flbs[[2]]
    flbs[[2]] <- flbs_unit[[1]]
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    # Make the fisheries - but we want FLCs 12 and 21 to be based on the units
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
    flfs[[1]][[2]] <- flc_unit <- random_FLCatch_generator(fixed_dims = dim(flq_unit))
    flfs[[2]][[1]] <- flc_unit <- random_FLCatch_generator(fixed_dims = dim(flq_unit))
    flfs@desc <- "Choose life"
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Fix FCB
    FCB <- array(NA, dim=c(5,3))
    FCB[1,] <- c(1,1,1)
    FCB[2,] <- c(1,2,2)
    FCB[3,] <- c(2,1,2)
    FCB[4,] <- c(2,2,3)
    FCB[5,] <- c(2,2,4)
    fc@FCB <- FCB
    # Random timestep
    season <- round(runif(1,min=1,max=4))
    year <- round(runif(1,min=1,dim(flq)[2]))
    timestep <- test_year_season_to_timestep(flq, year, season)
    # Go!
    om_out <- test_operatingModel_project_fisheries(flfs, flbs, fc, timestep)
    # FC 11 - no units
    fishery_no <- 1
    catch_no <- 1
    biol_no <- 1
    lout <- landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dout <- discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    cout <- catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    # Check that timestep is correct
    fin <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,,season,1,]
    zin <- fin + m(flbs_in[[biol_no]])[,year,,season,1,]
    cin <- (fin / zin) * (1 - exp(-zin)) * n(flbs_in[[biol_no]])[,year,,season,1]
    dr <- (discards.n(flfs[[fishery_no]][[catch_no]]) / (discards.n(flfs[[fishery_no]][[catch_no]]) + landings.n(flfs[[fishery_no]][[catch_no]])))[,year,,season,]
    # Catch, Landings, Discards numbers
    expect_FLQuant_equal(cin, cout)
    expect_FLQuant_equal(cin*(1-dr), lout)
    expect_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq))) %in% get_FLQuant_elements(flq, c(1,year,1,season,1,1), c(dim(flq)[1], year,1,season, 1, dim(flq)[6])))
    expect_equal(c(landings.n(flfs[[fishery_no]][[catch_no]]))[elem], c(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    expect_equal(c(discards.n(flfs[[fishery_no]][[catch_no]]))[elem], c(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    # FC 12 - units
    fishery_no <- 1
    catch_no <- 2
    biol_no <- 2
    lout <- landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dout <- discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    cout <- catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    fin <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,,season,]
    total_fin <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,,season,]
    zin <- total_fin + m(flbs_in[[biol_no]])[,year,,season,]
    cin <- (fin / zin) * (1 - exp(-zin)) * n(flbs_in[[biol_no]])[,year,,season,]
    dr <- (discards.n(flfs[[fishery_no]][[catch_no]]) / (discards.n(flfs[[fishery_no]][[catch_no]]) + landings.n(flfs[[fishery_no]][[catch_no]])))[,year,,season,]
    # Catch, Landings, Discards numbers
    expect_FLQuant_equal(cin, cout)
    expect_FLQuant_equal(cin*(1-dr), lout)
    expect_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq_unit))) %in% get_FLQuant_elements(flq_unit, c(1,year,1,season,1,1), c(dim(flq_unit)[1], year, dim(flq_unit)[3], season, 1, dim(flq_unit)[6])))
    expect_equal(c(landings.n(flfs[[fishery_no]][[catch_no]]))[elem], c(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    expect_equal(c(discards.n(flfs[[fishery_no]][[catch_no]]))[elem], c(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    # FC 21
    fishery_no <- 2
    catch_no <- 1
    biol_no <- 2
    lout <- landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dout <- discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    cout <- catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    fin <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,,season,]
    total_fin <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,,season,]
    zin <- total_fin + m(flbs_in[[biol_no]])[,year,,season,]
    cin <- (fin / zin) * (1 - exp(-zin)) * n(flbs_in[[biol_no]])[,year,,season,]
    dr <- (discards.n(flfs[[fishery_no]][[catch_no]]) / (discards.n(flfs[[fishery_no]][[catch_no]]) + landings.n(flfs[[fishery_no]][[catch_no]])))[,year,,season,]
    # Catch, Landings, Discards numbers
    expect_FLQuant_equal(cin, cout)
    expect_FLQuant_equal(cin*(1-dr), lout)
    expect_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq_unit))) %in% get_FLQuant_elements(flq_unit, c(1,year,1,season,1,1), c(dim(flq_unit)[1], year, dim(flq_unit)[3], season, 1, dim(flq_unit)[6])))
    expect_equal(c(landings.n(flfs[[fishery_no]][[catch_no]]))[elem], c(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    expect_equal(c(discards.n(flfs[[fishery_no]][[catch_no]]))[elem], c(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    # FC 22
    fishery_no <- 2
    catch_no <- 2
    lout <- landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dout <- discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    cout <- catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dr <- (discards.n(flfs[[fishery_no]][[catch_no]]) / (discards.n(flfs[[fishery_no]][[catch_no]]) + landings.n(flfs[[fishery_no]][[catch_no]])))[,year,,season,]
    biol_no <- 3
    fin3 <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,,season,]
    total_fin3 <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,,season,]
    zin3 <- total_fin3 + m(flbs_in[[biol_no]])[,year,,season,]
    cin3 <- (fin3 / zin3) * (1 - exp(-zin3)) * n(flbs_in[[biol_no]])[,year,,season,]
    biol_no <- 4
    fin4 <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,,season,]
    total_fin4 <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,,season,]
    zin4 <- total_fin4 + m(flbs_in[[biol_no]])[,year,,season,]
    cin4 <- (fin4 / zin4) * (1 - exp(-zin4)) * n(flbs_in[[biol_no]])[,year,,season,]
    cin <- cin3 + cin4
    # Catch, Landings, Discards numbers
    expect_FLQuant_equal(cin, cout)
    expect_FLQuant_equal(cin*(1-dr), lout)
    expect_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq))) %in% get_FLQuant_elements(flq, c(1,year,1,season,1,1), c(dim(flq)[1], year, 1, season, 1, dim(flq)[6])))
    expect_equal(c(landings.n(flfs[[fishery_no]][[catch_no]]))[elem], c(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    expect_equal(c(discards.n(flfs[[fishery_no]][[catch_no]]))[elem], c(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
})

