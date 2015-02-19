context("Test operating model")

test_that("timestep_to_year_season_conversion",{
    flq_in <- random_FLQuant_generator()
    year <- round(runif(1,min=1,max=dim(flq_in)[2]))
    season <- round(runif(1,min=1,max=dim(flq_in)[4]))
    expect_that(test_year_season_to_timestep_FLQuant_double(flq_in, year, season), equals((year-1)*dim(flq_in)[4] + season))
    expect_that(test_year_season_to_timestep_FLQuant_adouble(flq_in, year, season), equals((year-1)*dim(flq_in)[4] + season))
    expect_that(test_year_season_to_timestep(flq_in, year, season), equals((year-1)*dim(flq_in)[4] + season))
    # And the other way
    timestep <- test_year_season_to_timestep(flq_in, year, season)
    expect_that(test_timestep_to_year_season_FLQuant_double(flq_in, timestep), equals(c(year,season)))
    expect_that(test_timestep_to_year_season_FLQuant_adouble(flq_in, timestep), equals(c(year,season)))
    expect_that(test_timestep_to_year_season(flq_in, timestep), equals(c(year,season)))
})

test_that("operatingModel constructors and updaters",{
    # Empty constructor - jusy check they don't fail
    test_operatingModel_empty_constructor()

    # Main constructor test
    # Set up parameters for full test 
    flq <- random_FLQuant_generator()
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- FLBiols(lapply(flbs, function(x) return(x[["biol"]])))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    fc <- dummy_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Test as and wrap
    out <- test_operatingModel_full_constructor(flfs, flbs, fc)
    attr(fc@target, "FCB") <- NULL # To remove FCB attribute for test
    expect_that(out[["biols"]], is_identical_to(flbs_in))
    expect_that(out[["fisheries"]], is_identical_to(flfs))
    expect_that(out[["ctrl"]], is_identical_to(fc))
    # Here

    # biol - dimension check
})

test_that("operatingModel Q methods",{
    flq <- random_FLQuant_generator()
    flq <- random_FLQuant_generator(fixed_dims = c(10,52,1,1,1,20))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- FLBiols(lapply(flbs, function(x) return(x[["biol"]])))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    fc <- dummy_fwdControl_generator(years = 1, niters = dim(flq)[6])
    indices <- round(runif(5, min = 1, max = dim(flq)[-1]))
    fishery_no <- round(runif(1,min=1, max=length(flfs)))
    catch_no <- round(runif(1,min=1, max=length(flfs[[fishery_no]])))
    biol_no <- round(runif(1,min=1, max=length(flbs)))
    cq_flq <- as(catch.q(flfs[[fishery_no]][[catch_no]]), "FLQuant")
    # single value
    qout <- test_operatingModel_catch_q_adouble(flfs, flbs, fc, fishery_no, catch_no, biol_no, indices)
    cq_flq_indices <- pmin(dim(cq_flq)[-1],indices)
    cq <- c(cq_flq[,cq_flq_indices[1],cq_flq_indices[2],cq_flq_indices[3],cq_flq_indices[4],cq_flq_indices[5]])
    biomass <- c(quantSums(n(flbs[[biol_no]][["biol"]]) * wt(flbs[[biol_no]][["biol"]]))[1,indices[1],indices[2],indices[3],indices[4],indices[5]])
    expect_that(qout, equals(cq[1] * biomass ^ (-cq[2])))
    # FLQuantAD
    qout <- test_operatingModel_catch_q_FLQuantAD(flfs, flbs, fc, fishery_no, catch_no, biol_no)
    qout_orig <- test_operatingModel_catch_q_orig_FLQuantAD(flfs, flbs, fc, fishery_no, catch_no, biol_no) # Remove method after confirming correctness
    biomass <- quantSums(n(flbs[[biol_no]][["biol"]]) * wt(flbs[[biol_no]][["biol"]]))
    qin <- sweep(sweep(biomass, 6, -cq_flq[2,], "^"), 6, cq_flq[1], "*")
    expect_that(qout@.Data, equals(qin@.Data))
    expect_that(qout_orig@.Data, equals(qin@.Data))
    # FLQuantAD Year Season
    qout <- test_operatingModel_catch_q_FLQuantAD_YS(flfs, flbs, fc, fishery_no, catch_no, biol_no, indices[2], indices[4])
    expect_that(c(qout), equals(c(qin[,indices[2],,indices[4],])))
})

# Test F method with random Biols and Fisheries
# No check if FC catches B
test_that("operatingModel basic F method",{
    flq <- random_FLQuant_generator()
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- FLBiols(lapply(flbs, function(x) return(x[["biol"]])))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    fc <- dummy_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # FCB needed for constructor but not actually used
    attr(fc@target, "FCB") <- array(0, dim = c(10,3))
    fishery_no <- round(runif(1,min=1, max=length(flfs)))
    catch_no <- round(runif(1,min=1, max=length(flfs[[fishery_no]])))
    biol_no <- round(runif(1,min=1, max=length(flbs)))
    cq_flq <- as(catch.q(flfs[[fishery_no]][[catch_no]]), "FLQuant")
    # Any FLC FLB combination
    fout <- test_operatingModel_F(flfs, flbs, fc, fishery_no, catch_no, biol_no)
    biomass <- quantSums(n(flbs[[biol_no]][["biol"]]) * wt(flbs[[biol_no]][["biol"]]))
    qin <- sweep(sweep(biomass, 6, -cq_flq[2,], "^"), 6, cq_flq[1], "*")
    fin <- sweep(catch.sel(flfs[[fishery_no]][[catch_no]]), 2:6, qin * effort(flfs[[fishery_no]]), "*")
    expect_that(fout@.Data, equals(fin@.Data))
})

# F, partial F, Z, SSB, C, L, D
# Using the annual test operating model
# Epic!
test_that("operatingModel annual project",{
    om <- make_test_operatingModel1(5)

    # Random timestep
    dims <- dim(n(om[["biols"]][[1]][["biol"]]))
    year <- round(runif(1,min=1,max=dims[2]))
    season <- round(runif(1,min=1,max=dims[4]))
    timestep <- (year-1)*dims[4] + season
    next_year <-  floor((timestep) / dims[4]) + 1; 
    next_season <- timestep %% dims[4] + 1;
    om_out <- test_operatingModel_project_timestep(om[["fisheries"]], om[["biols"]], om[["fwc"]], timestep)

    #------------------
    # 1 biol -> 1 catch
    #------------------
    biol_no <- 1
    fishery_no <- 1
    catch_no <- 1
    # Total F on the biol
    f1 <- test_operatingModel_F_B(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
    # Partial F from FC
    pf11 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    # Total Z of biol
    z1 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
    # SSB of biol
    ssb1_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)

    # Get metrics from IP OM
    biomass_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
    qin <- sweep(sweep(biomass_in, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
    f1in <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
    z1in <- f1in + m(om[["biols"]][[biol_no]][["biol"]])
    ssb1_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]) * fec(om[["biols"]][[biol_no]][["biol"]]) * exp(-((f1in+ m(om[["biols"]][[biol_no]][["biol"]]))* spwn(om[["biols"]][[biol_no]][["biol"]]))))
    catch.n1 <- (f1in / z1in) * (1 - exp(-z1in)) * n(om[["biols"]][[biol_no]][["biol"]])
    landings.n1 <- (catch.n1[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]))
    discards.n1 <- (catch.n1[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]))
    # Biol abundances
    next_n <- n(om[["biols"]][[biol_no]][["biol"]])[,next_year,,next_season,,]
    next_n[] <- 0
    next_n[2:dims[1],] <- (n(om[["biols"]][[biol_no]][["biol"]])[,year,,season,,] * exp(-z1in[,year,,season,,]))[1:(dims[1]-1),]
    next_n[dims[1]] <- next_n[dims[1]] + (n(om[["biols"]][[biol_no]][["biol"]])[dims[1],year,,season,,] * exp(-z1in[dims[1],year,,season,,]))
    # Rec - it's BH for Biol 1
    ssb_timestep <- timestep - om[["biols"]][[biol_no]][["srr_timelag"]] + 1
    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
    ssb_rec <- ssb1_in[,ssb_year,,ssb_season,,]
    rec_in <- om[["biols"]][[biol_no]][["srr_params"]]['a',] * ssb_rec / (om[["biols"]][[biol_no]][["srr_params"]]['b',] + ssb_rec)
    # Multiplicative residuals
    next_n[1,] <- rec_in * exp(om[["biols"]][[biol_no]][["srr_residuals"]][,next_year,,next_season,,])

    # Test Fs and Zs
    expect_that(f1@.Data, equals(f1in@.Data)) # Total F on Biol
    expect_that(pf11@.Data, equals(f1in@.Data)) # Partial F from FC
    expect_that(z1@.Data, equals(z1in@.Data)) # Partial F from FC
    # Test SSB
    expect_that(ssb1_out@.Data, equals(ssb1_in@.Data))
    # Test Catch, landings and discards in timestep only
    # Catch, landings, discards in timestep
    expect_that(catch.n1[,year,1,season,1,]@.Data, equals(catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]@.Data))
    expect_that(landings.n1@.Data, equals(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]@.Data))
    expect_that(discards.n1@.Data,equals(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]@.Data))
    # Only CLD in that timestep should have changed
    expect_that(catch.n(om[["fisheries"]][[fishery_no]][[catch_no]])[,-year]@.Data, equals(catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,-year]@.Data))
    expect_that(landings.n(om[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data, equals(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data))
    expect_that(discards.n(om[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data, equals(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data))
    # Abundances
    expect_that(next_n, equals(n(om_out[["biols"]][[biol_no]])[,next_year,,next_season,,]))
    # Other abundances unaffected
    expect_that(n(om[["biols"]][[biol_no]][["biol"]])[,-next_year,,,,]@.Data,equals(n(om_out[["biols"]][[biol_no]])[,-next_year,,,,]@.Data))

    #------------------
    # 1 biol -> 2 catch (FC 12 & 21 -> B2)
    #------------------
    biol_no <- 2
    # Total F on the biol
    f2 <- test_operatingModel_F_B(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
    # Partial F from FC
    pf12 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, biol_no)
    pf21 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 1, biol_no)
    # Total Z of biol
    z2 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
    # SSB of biol
    ssb2_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)

    # Get metrics from IP OM
    biomass_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
    cq_flq_12 <- as(catch.q(om[["fisheries"]][[1]][[2]]), "FLQuant")
    cq_flq_21 <- as(catch.q(om[["fisheries"]][[2]][[1]]), "FLQuant")
    qin_12 <- sweep(sweep(biomass_in, c(1,3,4,5), -cq_flq_12[2,], "^"), c(1,3,4,5), cq_flq_12[1], "*")
    qin_21 <- sweep(sweep(biomass_in, c(1,3,4,5), -cq_flq_21[2,], "^"), c(1,3,4,5), cq_flq_21[1], "*")
    pfin_12 <- sweep(catch.sel(om[["fisheries"]][[1]][[2]]), 2:6, qin_12 * effort(om[["fisheries"]][[1]]), "*")
    pfin_21 <- sweep(catch.sel(om[["fisheries"]][[2]][[1]]), 2:6, qin_21 * effort(om[["fisheries"]][[2]]), "*")
    f2in <- pfin_12 + pfin_21
    z2in <- f2in + m(om[["biols"]][[biol_no]][["biol"]])
    ssb2_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]) * fec(om[["biols"]][[biol_no]][["biol"]]) * exp(-((f2in+ m(om[["biols"]][[biol_no]][["biol"]]))* spwn(om[["biols"]][[biol_no]][["biol"]]))))
    # CLD
    catch.n12 <- (pfin_12 / z2in) * (1 - exp(-z2in)) * n(om[["biols"]][[biol_no]][["biol"]])
    landings.n12 <- (catch.n12[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[1]][[2]])[,year,1,season,1,]))
    discards.n12 <- (catch.n12[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[1]][[2]])[,year,1,season,1,]))
    catch.n21 <- (pfin_21 / z2in) * (1 - exp(-z2in)) * n(om[["biols"]][[biol_no]][["biol"]])
    landings.n21 <- (catch.n21[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[2]][[1]])[,year,1,season,1,]))
    discards.n21 <- (catch.n21[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[2]][[1]])[,year,1,season,1,]))
    # Biol abundances
    next_n <- n(om[["biols"]][[biol_no]][["biol"]])[,next_year,,next_season,,]
    next_n[] <- 0
    next_n[2:dims[1],] <- (n(om[["biols"]][[biol_no]][["biol"]])[,year,,season,,] * exp(-z2in[,year,,season,,]))[1:(dims[1]-1),]
    next_n[dims[1]] <- next_n[dims[1]] + (n(om[["biols"]][[biol_no]][["biol"]])[dims[1],year,,season,,] * exp(-z2in[dims[1],year,,season,,]))
    # Rec - it's Ricker for Biol 2
    ssb_timestep <- timestep - om[["biols"]][[biol_no]][["srr_timelag"]] + 1
    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
    ssb_rec <- ssb2_in[,ssb_year,,ssb_season,,]
    rec_in <- om[["biols"]][[biol_no]][["srr_params"]]['a',] * ssb_rec * exp(-om[["biols"]][[biol_no]][["srr_params"]]['b',] * ssb_rec)
    # Multiplicative residuals
    next_n[1,] <- rec_in * exp(om[["biols"]][[biol_no]][["srr_residuals"]][,next_year,,next_season,,])
    # Test Fs and Zs
    expect_that(f2@.Data, equals(f2in@.Data)) # Total F on Biol
    expect_that(pf12@.Data, equals(pfin_12@.Data)) # Partial F from FC
    expect_that(pf21@.Data, equals(pfin_21@.Data)) # Partial F from FC
    expect_that(z2@.Data, equals(z2in@.Data)) # Partial F from FC
    # Test SSB
    expect_that(ssb2_out@.Data, equals(ssb2_in@.Data))
    # Test Catch, landings and discards in timestep only
    # Catch, landings, discards in timestep
    expect_that(catch.n12[,year,1,season,1,]@.Data, equals(catch.n(om_out[["fisheries"]][[1]][[2]])[,year,1,season,1,]@.Data))
    expect_that(landings.n12@.Data, equals(landings.n(om_out[["fisheries"]][[1]][[2]])[,year,1,season,1,]@.Data))
    expect_that(discards.n12@.Data,equals(discards.n(om_out[["fisheries"]][[1]][[2]])[,year,1,season,1,]@.Data))
    expect_that(catch.n21[,year,1,season,1,]@.Data, equals(catch.n(om_out[["fisheries"]][[2]][[1]])[,year,1,season,1,]@.Data))
    expect_that(landings.n21@.Data, equals(landings.n(om_out[["fisheries"]][[2]][[1]])[,year,1,season,1,]@.Data))
    expect_that(discards.n21@.Data,equals(discards.n(om_out[["fisheries"]][[2]][[1]])[,year,1,season,1,]@.Data))
    # Only CLD in that timestep should have changed
    expect_that(catch.n(om[["fisheries"]][[1]][[2]])[,-year]@.Data, equals(catch.n(om_out[["fisheries"]][[1]][[2]])[,-year]@.Data))
    expect_that(landings.n(om[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data, equals(landings.n(om_out[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data))
    expect_that(discards.n(om[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data, equals(discards.n(om_out[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data))
    expect_that(catch.n(om[["fisheries"]][[2]][[1]])[,-year]@.Data, equals(catch.n(om_out[["fisheries"]][[2]][[1]])[,-year]@.Data))
    expect_that(landings.n(om[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data, equals(landings.n(om_out[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data))
    expect_that(discards.n(om[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data, equals(discards.n(om_out[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data))
    # Abundances
    expect_that(next_n, equals(n(om_out[["biols"]][[biol_no]])[,next_year,,next_season,,]))
    # Other abundances unaffected
    expect_that(n(om[["biols"]][[biol_no]][["biol"]])[,-next_year,,,,]@.Data,equals(n(om_out[["biols"]][[biol_no]])[,-next_year,,,,]@.Data))

    #------------------
    # 2 biol -> 1 catch (FC 22 -> B3 + B4)
    #------------------
    # Total F on the biol
    f3 <- test_operatingModel_F_B(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3)
    f4 <- test_operatingModel_F_B(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4)
    # Partial F from FC
    pf223 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 3)
    pf224 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 4)
    # Total Z of biol
    z3 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3)
    z4 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4)
    # SSB of biol
    ssb3_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3)
    ssb4_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4)

    # Get metrics from IP OM
    biomass_in3 <- quantSums(n(om[["biols"]][[3]][["biol"]]) * wt(om[["biols"]][[3]][["biol"]]))
    biomass_in4 <- quantSums(n(om[["biols"]][[4]][["biol"]]) * wt(om[["biols"]][[4]][["biol"]]))
    cq_flq_22 <- as(catch.q(om[["fisheries"]][[2]][[2]]), "FLQuant")
    qin_223 <- sweep(sweep(biomass_in3, c(1,3,4,5), -cq_flq_22[2,], "^"), c(1,3,4,5), cq_flq_22[1], "*")
    qin_224 <- sweep(sweep(biomass_in4, c(1,3,4,5), -cq_flq_22[2,], "^"), c(1,3,4,5), cq_flq_22[1], "*")
    fin_223 <- sweep(catch.sel(om[["fisheries"]][[2]][[2]]), 2:6, qin_223 * effort(om[["fisheries"]][[2]]), "*")
    fin_224 <- sweep(catch.sel(om[["fisheries"]][[2]][[2]]), 2:6, qin_224 * effort(om[["fisheries"]][[2]]), "*")
    z3in <- fin_223 + m(om[["biols"]][[3]][["biol"]])
    z4in <- fin_224 + m(om[["biols"]][[4]][["biol"]])
    ssb3_in <- quantSums(n(om[["biols"]][[3]][["biol"]]) * wt(om[["biols"]][[3]][["biol"]]) * fec(om[["biols"]][[3]][["biol"]]) * exp(-((fin_223 + m(om[["biols"]][[3]][["biol"]]))* spwn(om[["biols"]][[3]][["biol"]]))))
    ssb4_in <- quantSums(n(om[["biols"]][[4]][["biol"]]) * wt(om[["biols"]][[4]][["biol"]]) * fec(om[["biols"]][[4]][["biol"]]) * exp(-((fin_224 + m(om[["biols"]][[4]][["biol"]]))* spwn(om[["biols"]][[4]][["biol"]]))))
    # CLD
    catch.n223 <- (fin_223 / z3in) * (1 - exp(-z3in)) * n(om[["biols"]][[3]][["biol"]])
    catch.n224 <- (fin_224 / z4in) * (1 - exp(-z4in)) * n(om[["biols"]][[4]][["biol"]])
    landings.n223 <- (catch.n223[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
    landings.n224 <- (catch.n224[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
    discards.n223 <- (catch.n223[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
    discards.n224 <- (catch.n224[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
    catch.n22 <- catch.n223 + catch.n224
    landings.n22 <- landings.n223 + landings.n224
    discards.n22 <- discards.n223 + discards.n224

    # Biol abundances
    next_n3 <- n(om[["biols"]][[3]][["biol"]])[,next_year,,next_season,,]
    next_n3[] <- 0
    next_n3[2:dims[1],] <- (n(om[["biols"]][[3]][["biol"]])[,year,,season,,] * exp(-z3in[,year,,season,,]))[1:(dims[1]-1),]
    next_n3[dims[1]] <- next_n3[dims[1]] + (n(om[["biols"]][[3]][["biol"]])[dims[1],year,,season,,] * exp(-z3in[dims[1],year,,season,,]))
    # Rec - it's Bevholt for Biol 3
    ssb_timestep <- timestep - om[["biols"]][[3]][["srr_timelag"]] + 1
    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
    ssb_rec <- ssb3_in[,ssb_year,,ssb_season,,]
    rec_in <- om[["biols"]][[3]][["srr_params"]]['a',] * ssb_rec / (om[["biols"]][[3]][["srr_params"]]['b',] + ssb_rec)
    # Multiplicative residuals
    next_n3[1,] <- rec_in * exp(om[["biols"]][[3]][["srr_residuals"]][,next_year,,next_season,,])

    next_n4 <- n(om[["biols"]][[4]][["biol"]])[,next_year,,next_season,,]
    next_n4[] <- 0
    next_n4[2:dims[1],] <- (n(om[["biols"]][[4]][["biol"]])[,year,,season,,] * exp(-z4in[,year,,season,,]))[1:(dims[1]-1),]
    next_n4[dims[1]] <- next_n4[dims[1]] + (n(om[["biols"]][[4]][["biol"]])[dims[1],year,,season,,] * exp(-z4in[dims[1],year,,season,,]))
    # Rec - it's Ricker for Biol 4
    ssb_timestep <- timestep - om[["biols"]][[4]][["srr_timelag"]] + 1
    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
    ssb_rec <- ssb4_in[,ssb_year,,ssb_season,,]
    rec_in <- om[["biols"]][[4]][["srr_params"]]['a',] * ssb_rec * exp(-om[["biols"]][[4]][["srr_params"]]['b',] * ssb_rec)
    # Multiplicative residuals
    next_n4[1,] <- rec_in * exp(om[["biols"]][[4]][["srr_residuals"]][,next_year,,next_season,,])
    # Test Fs and Zs
    expect_that(f3@.Data, equals(fin_223@.Data)) # Total F on Biol
    expect_that(f4@.Data, equals(fin_224@.Data)) # Total F on Biol
    expect_that(pf223@.Data, equals(fin_223@.Data)) # Total F on Biol
    expect_that(pf224@.Data, equals(fin_224@.Data)) # Total F on Biol
    expect_that(z3@.Data, equals(z3in@.Data)) # Partial F from FC
    expect_that(z4@.Data, equals(z4in@.Data)) # Partial F from FC
    # Test SSB
    expect_that(ssb3_out@.Data, equals(ssb3_in@.Data))
    expect_that(ssb4_out@.Data, equals(ssb4_in@.Data))
    # Test Catch, landings and discards in timestep only
    # Catch, landings, discards in timestep
    expect_that(catch.n22[,year,1,season,1,]@.Data, equals(catch.n(om_out[["fisheries"]][[2]][[2]])[,year,1,season,1,]@.Data))
    expect_that(landings.n22@.Data, equals(landings.n(om_out[["fisheries"]][[2]][[2]])[,year,1,season,1,]@.Data))
    expect_that(discards.n22@.Data,equals(discards.n(om_out[["fisheries"]][[2]][[2]])[,year,1,season,1,]@.Data))
    # Only CLD in that timestep should have changed
    expect_that(catch.n(om[["fisheries"]][[2]][[2]])[,-year]@.Data, equals(catch.n(om_out[["fisheries"]][[2]][[2]])[,-year]@.Data))
    expect_that(landings.n(om[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data, equals(landings.n(om_out[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data))
    expect_that(discards.n(om[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data, equals(discards.n(om_out[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data))
    # Abundances
    expect_that(next_n3, equals(n(om_out[["biols"]][[3]])[,next_year,,next_season,,]))
    expect_that(next_n4, equals(n(om_out[["biols"]][[4]])[,next_year,,next_season,,]))
    # Other abundances unaffected
    expect_that(n(om[["biols"]][[3]][["biol"]])[,-next_year,,,,]@.Data,equals(n(om_out[["biols"]][[3]])[,-next_year,,,,]@.Data))
    expect_that(n(om[["biols"]][[4]][["biol"]])[,-next_year,,,,]@.Data,equals(n(om_out[["biols"]][[4]])[,-next_year,,,,]@.Data))

    #------------------
    # 1 biol -> 0 catch (B5)
    #------------------
    # Total F on the biol
    f5 <- test_operatingModel_F_B(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5)
    # Total Z of biol
    z5 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5)
    # SSB of biol
    ssb5_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5)
    # Get metrics from IP OM
    biomass_in5 <- quantSums(n(om[["biols"]][[5]][["biol"]]) * wt(om[["biols"]][[5]][["biol"]]))
    z5in <- m(om[["biols"]][[5]][["biol"]])
    ssb5_in <- quantSums(n(om[["biols"]][[5]][["biol"]]) * wt(om[["biols"]][[5]][["biol"]]) * fec(om[["biols"]][[5]][["biol"]]) * exp(-((m(om[["biols"]][[5]][["biol"]]))* spwn(om[["biols"]][[5]][["biol"]]))))

    # Biol abundances
    next_n5 <- n(om[["biols"]][[5]][["biol"]])[,next_year,,next_season,,]
    next_n5[] <- 0
    next_n5[2:dims[1],] <- (n(om[["biols"]][[5]][["biol"]])[,year,,season,,] * exp(-z5in[,year,,season,,]))[1:(dims[1]-1),]
    next_n5[dims[1]] <- next_n5[dims[1]] + (n(om[["biols"]][[5]][["biol"]])[dims[1],year,,season,,] * exp(-z5in[dims[1],year,,season,,]))
    # Rec - it's Ricker for Biol 5
    ssb_timestep <- timestep - om[["biols"]][[5]][["srr_timelag"]] + 1
    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
    ssb_rec <- ssb5_in[,ssb_year,,ssb_season,,]
    rec_in <- om[["biols"]][[5]][["srr_params"]]['a',] * ssb_rec * exp(-om[["biols"]][[5]][["srr_params"]]['b',] * ssb_rec)
    # Multiplicative residuals
    next_n5[1,] <- rec_in * exp(om[["biols"]][[5]][["srr_residuals"]][,next_year,,next_season,,])
    # Test Fs and Zs
    expect_that(all(f5@.Data==0), equals(TRUE)) # Total F on Biol
    expect_that(z5@.Data, equals(z5in@.Data)) # Partial F from FC
    # Test SSB
    expect_that(ssb5_out@.Data, equals(ssb5_in@.Data))
    # Abundances
    expect_that(next_n5, equals(n(om_out[["biols"]][[5]])[,next_year,,next_season,,]))
    # Other abundances unaffected
    expect_that(n(om[["biols"]][[5]][["biol"]])[,-next_year,,,,]@.Data,equals(n(om_out[["biols"]][[5]])[,-next_year,,,,]@.Data))
})

# Just checking partial Fs - including when they should be 0
test_that("operatingModel partial Fs",{
    om <- make_test_operatingModel1(5)
    # Partial F of F/C on a B
    # F/C 11 -> B
    fishery_no <- 1
    catch_no <- 1
    biol_no <- 1
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    biomass <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
    qin <- sweep(sweep(biomass, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
    fin <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
    expect_that(fout@.Data, equals(fin@.Data))
    biol_no <- 2
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    biol_no <- 3
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    biol_no <- 4
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    biol_no <- 5
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    # F/C 12 -> B
    fishery_no <- 1
    catch_no <- 2
    biol_no <- 1
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    biol_no <- 2
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    biomass <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
    qin <- sweep(sweep(biomass, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
    fin <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
    expect_that(fout@.Data, equals(fin@.Data))
    biol_no <- 3
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    biol_no <- 4
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    # F/C 21 -> B
    fishery_no <- 2
    catch_no <- 1
    biol_no <- 1
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    biol_no <- 2
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    biomass <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
    qin <- sweep(sweep(biomass, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
    fin <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
    expect_that(fout@.Data, equals(fin@.Data))
    biol_no <- 3
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    biol_no <- 4
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    # F/C 22 -> B
    fishery_no <- 2
    catch_no <- 2
    biol_no <- 1
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    biol_no <- 2
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    expect_that(all(fout==0.0), is_true())
    biol_no <- 3
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    biomass <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
    qin <- sweep(sweep(biomass, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
    fin <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
    expect_that(fout@.Data, equals(fin@.Data))
    biol_no <- 4
    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
    biomass <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
    qin <- sweep(sweep(biomass, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
    fin <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
    expect_that(fout@.Data, equals(fin@.Data))
})


#
## Biomass, SSB and other abundance based targets need to change F in the previous timestep
#test_that("operatingModel get_target_fmult_timestep",{
#    # Make an FLFishery with X FLFishery objects. Each FLFishery has an FLCatch that catches the FLBiol
#    # This is all a massive faff
#    # Have at least 5 years and 10 ages, random number of seasons
#    nyears <- 10
#    flq <- random_FLQuant_generator(fixed_dim=c(10,nyears,NA,NA,NA,NA), sd=1)
#    # Single FLBiol
#    flb <- random_FLBiol_generator(fixed_dims = dim(flq), sd = 1 )
#    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=5, min_catches = 1, max_catches = 3, sd=1)
#    # Each element of F is F from an FLCatch attacking the same FLBiol
#    f <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq), sd=1)
#    f <- lapply(f,abs)
#    f_spwn <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq), sd=1)
#    f_spwn <- lapply(f_spwn,abs)
#    # SRR bits
#    srr_model_name <- "ricker"
#    params_sr <- as.FLQuant(FLPar(a=10, b = 4))
#    residuals_sr <- flq[1,]
#    residuals_mult <- TRUE
#    srr_timelag <- 1
#
#    fc <- dummy_fwdControl_generator(years = 1:6, niters = dim(n(flb))[6])
#    fc@target@element$season <- round(runif(dim(fc@target@element)[1], min = 1, max = dim(flq)[4]))
#    fc@target@element[2,"quantity"] <- "catch"
#    fc@target@element[3,"quantity"] <- "ssb"
#    fc@target@element[4,"quantity"] <- "biomass"
#
#    target_no <- 1 # F
#    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    year <- fc@target@element[target_no,"year"]
#    season <- fc@target@element[target_no,"season"]
#    ctrl_timestep <- as.integer((year-1) * dim(flq)[4] + season)
#    expect_that(fmult_timestep, is_identical_to(ctrl_timestep))
#
#    target_no <- 2 # Catch
#    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    year <- fc@target@element[target_no,"year"]
#    season <- fc@target@element[target_no,"season"]
#    ctrl_timestep <- as.integer((year-1) * dim(flq)[4] + season)
#    expect_that(fmult_timestep, is_identical_to(ctrl_timestep))
#
#    target_no <- 3 # SSB
#    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    year <- fc@target@element[target_no,"year"]
#    season <- fc@target@element[target_no,"season"]
#    ctrl_timestep <- as.integer((year-1) * dim(flq)[4] + season)
#    expect_that(fmult_timestep, is_identical_to(ctrl_timestep-1L))
#
#    target_no <- 4 # Biomass
#    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    year <- fc@target@element[target_no,"year"]
#    season <- fc@target@element[target_no,"season"]
#    ctrl_timestep <- as.integer((year-1) * dim(flq)[4] + season)
#    expect_that(fmult_timestep, is_identical_to(ctrl_timestep-1L))
#})
#
#test_that("operatingModel target values and eval_target method", {
#    # Make an FLFishery with X FLFishery objects. Each FLFishery has an FLCatch that catches the FLBiol
#    # This is all a massive faff
#    # Have at least 5 years and 10 ages, random number of seasons
#    nyears <- 10
#    flq <- random_FLQuant_generator(fixed_dim=c(10,nyears,NA,NA,NA,NA), sd=1)
#    # Single FLBiol
#    flb <- random_FLBiol_generator(fixed_dims = dim(flq), sd = 1 )
#    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=5, min_catches = 1, max_catches = 3, sd=1)
#    # Each element of F is F from an FLCatch attacking the same FLBiol
#    f <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq), sd=1)
#    f <- lapply(f,abs)
#    f_spwn <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq), sd=1)
#    f_spwn <- lapply(f_spwn,abs)
#    # SRR bits
#    srr_model_name <- "ricker"
#    params_sr <- as.FLQuant(FLPar(a=10, b = 4))
#    residuals_sr <- flq[1,]
#    residuals_mult <- TRUE
#    srr_timelag <- 1
#    # Catch no has be to 1 at the moment - we only have 1 biol and we need to think about how to link a biol to a catch
#    catch_no <- 1 # round(runif(n=1,min=1,max=min(unlist(lapply(flfs, length))))) # which catch no of each fishery
#    fishery_no <- round(runif(1, min=1, max=length(flfs)))
#
#    #------------ Test age range indices --------------------
#    fc <- dummy_fwdControl_generator(years = 1:6, niters = dim(n(flb))[6])
#    fc@target@element[1,c("minAge","maxAge")] <- c(1L,5L)
#    fc@target@element[2,c("minAge","maxAge")] <- c(2L,3L)
#    fc@target@element[3,c("minAge","maxAge")] <- c(20L,30L)
#    fc@target@element[4,c("minAge","maxAge")] <- c(2L,30L)
#    fc@target@element[5,c("minAge","maxAge")] <- c(0L,30L)
#    fc@target@element[6,c("minAge","maxAge")] <- c(as.integer(NA),as.integer(NA))
#
#    age_indices <- test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 1)
#    expect_that(age_indices, equals(unname(unlist(fc@target@element[1,c("minAge","maxAge")])) - as.numeric(dimnames(n(flb))[[1]][1])))
#    age_indices <- test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 2)
#    expect_that(age_indices, equals(unname(unlist(fc@target@element[2,c("minAge","maxAge")])) - as.numeric(dimnames(n(flb))[[1]][1])))
#    # outside range - error
#    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 3), throws_error())
#    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 4), throws_error())
#    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 5), throws_error())
#    # range not set
#    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 6), throws_error())
#
#
#    #---------- Test target methods ---------------
#    # Get the target values from the C++ code
#    # Nothing to do with the target quantities in the control here - just testing internal methods
#    targets <- test_operating_model_targets(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, fishery_no, catch_no, 1)
#    # fbar of a single catch 
#    expect_that(targets[["fbar_catch"]], equals(apply(f[[fishery_no]][fc@target@element[1,"minAge"]:fc@target@element[1,"maxAge"],],2:6,mean)))
#    # a different age range
#    targets <- test_operating_model_targets(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, fishery_no, catch_no, 2)
#    # fbar of a single catch 
#    expect_that(targets[["fbar_catch"]], equals(apply(f[[fishery_no]][fc@target@element[2,"minAge"]:fc@target@element[2,"maxAge"],],2:6,mean)))
#
#    # total fbar of all fisheries
#    f2 <- lapply(f, function(x) 
#        apply(x[fc@target@element[2,"minAge"]:fc@target@element[2,"maxAge"],],2:6,mean)
#    )
#    f_total <- f2[[1]]
#    f_total[] <- 0
#    for (i in 1:length(flfs)){
#        f_total <- f_total + f2[[i]]
#    }
#    expect_that(targets[["fbar"]]@.Data, is_identical_to(f_total@.Data))
#
#    # catches - single
#    expect_that(targets[["catches_catch"]]@.Data , equals(catch(flfs[[fishery_no]][[catch_no]])@.Data))
#    # catches - total
#    catches_total <- catch(flfs[[fishery_no]][[catch_no]])
#    catches_total[] <- 0
#    for (i in 1:length(flfs)){
#        catches_total <- catches_total + catch(flfs[[i]][[catch_no]])
#    }
#    expect_that(targets[["catches"]]@.Data, equals(catches_total@.Data))
#
#    # landings - single
#    expect_that(targets[["landings_catch"]]@.Data , equals(landings(flfs[[fishery_no]][[catch_no]])@.Data))
#    # landings - total
#    landings_total <- landings(flfs[[fishery_no]][[catch_no]])
#    landings_total[] <- 0
#    for (i in 1:length(flfs)){
#        landings_total <- landings_total + landings(flfs[[i]][[catch_no]])
#    }
#    expect_that(targets[["landings"]]@.Data, equals(landings_total@.Data))
#
#    # discards - single
#    expect_that(targets[["discards_catch"]]@.Data , equals(discards(flfs[[fishery_no]][[catch_no]])@.Data))
#    # discards - total
#    discards_total <- discards(flfs[[fishery_no]][[catch_no]])
#    discards_total[] <- 0
#    for (i in 1:length(flfs)){
#        discards_total <- discards_total + discards(flfs[[i]][[catch_no]])
#    }
#    expect_that(targets[["discards"]]@.Data, equals(discards_total@.Data))
#
#    # SSB - f portion from all catches
#    f_portion <- f_spwn[[1]] * f[[1]]
#    for (i in 2:length(f)){
#        f_portion <- f_portion + (f_spwn[[i]] * f[[i]])
#    }
#    ssb_in <- quantSums(n(flb) * wt(flb) * fec(flb) * exp(-f_portion - m(flb) * spwn(flb)))
#    expect_that(ssb_in@.Data, equals(targets[["ssb"]]@.Data))
#
#    # Biomass
#    expect_that(targets[["biomass"]]@.Data, equals(quantSums(n(flb) * wt(flb))@.Data))
#
#    #-------- Test eval_target by target number----------
#    # Set up fc to test the output
#    # By defaulf fishery = NA
#    fishery_no <- round(runif(1, min=1,max=length(f)))
#    fc <- dummy_fwdControl_generator(years = 1:dim(flb@n)[2], niters = dim(n(flb))[6])
#    fc@target@element$minAge <- 2
#    fc@target@element$maxAge <- 5
#    f2 <- lapply(f, function(x) 
#        apply(x[fc@target@element[1,"minAge"]:fc@target@element[1,"maxAge"],],2:6,mean)
#    )
#    f_total <- f2[[1]]
#    f_total[] <- 0
#    for (i in 1:length(flfs)){
#        f_total <- f_total + f2[[i]]
#    }
#    # No fishery
#    fc@target@element[1,"quantity"] <- "f"
#    fc@target@element[2,"quantity"] <- "catch"
#    fc@target@element[3,"quantity"] <- "ssb"
#    fc@target@element[4,"quantity"] <- "biomass"
#
#    # Testing eval_target - is the correct quantity calculation called
#    min_iter <- 1
#    max_iter <- dim(fc@target@iters)[3]
#
#    # fbar
#    target_no <- 1 
#    fout <- test_operatingModel_eval_target(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    expect_that(fout@.Data, equals(f_total@.Data))
#    # catch
#    target_no <- 2 
#    cout <- test_operatingModel_eval_target(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    expect_that(cout@.Data, equals(catches_total@.Data))
#    # ssb 
#    target_no <- 3 
#    ssb_out <- test_operatingModel_eval_target(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    expect_that(ssb_out@.Data, equals(ssb_in@.Data))
#    # biomass
#    target_no <- 4 
#    biomass_out <- test_operatingModel_eval_target(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    biomass_in <- quantSums(n(flb) * wt(flb))
#    expect_that(biomass_out@.Data, equals(biomass_in@.Data))
#
#    #----------- Test calc_target_value  ----------
#    # Not a relative target - should just the values in the target_iter slot
#    target_no <- 1 # f target
#    fout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(fout, is_identical_to(unname(c(fc@target@iters[target_no,"value",]))))
#    target_no <- 2 # catch target
#    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(cout, is_identical_to(unname(c(fc@target@iters[target_no,"value",]))))
#    target_no <- 3 # ssb target
#    ssb_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(ssb_out, is_identical_to(unname(c(fc@target@iters[target_no,"value",]))))
#    target_no <- 4 # biomass target
#    biomass_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(biomass_out, is_identical_to(unname(c(fc@target@iters[target_no,"value",]))))
#
#    # Add a relative f target
#    fc@target@element[3,"quantity"] <- "f"
#    fc@target@element[3,"relYear"] <- 1
#    fc@target@element[3,"relSeason"] <- round(runif(1,min=1,max=dim(flq)[4]))
#    target_no <- 3
#    fout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(unname(c(f_total[1,1,1,fc@target@element[3,"relSeason"],1,]) * fc@target@iters[target_no,"value",]), equals(fout))
#    # Add a relative catch target
#    fc@target[4,"quantity"] <- "catch"
#    fc@target[4,"relYear"] <- 2
#    fc@target[4,"relSeason"] <- round(runif(1,min=1,max=dim(flq)[4]))
#    target_no <- 4
#    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(unname(c(catches_total[1,2,1,fc@target@element[4,"relSeason"],1,]) * fc@target@iters[target_no,"value",]), equals(cout))
#    # Add a relative ssb target
#    fc@target[5,"quantity"] <- "ssb"
#    fc@target[5,"relYear"] <- 3
#    fc@target[5,"relSeason"] <- round(runif(1,min=1,max=dim(flq)[4]))
#    target_no <- 5
#    ssb_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(unname(c(ssb_in[1,3,1,fc@target@element[5,"relSeason"],1,]) * fc@target@iters[target_no,"value",]), equals(ssb_out))
#    # Relative biomass target
#    fc@target[6,"quantity"] <- "biomass"
#    fc@target[6,"relYear"] <- 4
#    fc@target[6,"relSeason"] <- round(runif(1,min=1,max=dim(flq)[4]))
#    target_no <- 6
#    biomass_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(unname(c(biomass_in[1,4,1,fc@target@element[6,"relSeason"],1,]) * fc@target@iters[target_no,"value",]), equals(biomass_out))
#
#    # Min and max values
#    # Set a Min bound to start with
#    fc@target[7,"quantity"] <- "catch"
#    fc@target@iters[7,"value",] <- NA
#    fc@target@iters[7,"max",] <- NA
#    fc@target@iters[7,"min",] <- rnorm(dim(fc@target@iters)[3], mean = 1000)
#    # Set Catch in that year to be 0 so that catch should be 0 - but will be constrained by min catch
#    flfs_min <- flfs
#    for (i in 1:length(flfs_min)){
#        flfs_min[[i]][[1]]@landings.n[,fc@target@element[7,"year"],,fc@target@element[7,"season"],,] <- 0
#        flfs_min[[i]][[1]]@discards.n[,fc@target@element[7,"year"],,fc@target@element[7,"season"],,] <- 0
#    }
#    flfs_min@desc <- flfs@desc # eck
#    cout <- test_operatingModel_calc_target_value(flfs_min, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 7) 
#    expect_that(cout, is_identical_to(unname(fc@target@iters[7,"min",])))
#    # Try a Max value
#    fc@target[8,"quantity"] <- "catch"
#    fc@target@iters[8,"value",] <- NA
#    fc@target@iters[8,"min",] <- NA
#    fc@target@iters[8,"max",] <- rnorm(dim(fc@target@iters)[3], mean = 1000)
#    # Set Catch in that year to be larger than the catch in that year so that catch should be very big - but will be constrained by max catch
#    flfs_max <- flfs
#    for (i in 1:length(flfs_max)){
#        flfs_max[[i]][[1]]@landings.n[,fc@target@element[8,"year"],,fc@target@element[8,"season"],,] <- fc@target@iters[8,"max",] + 1
#        flfs_max[[i]][[1]]@discards.n[,fc@target@element[8,"year"],,fc@target@element[8,"season"],,] <- fc@target@iters[8,"max",] + 1
#    }
#    flfs_max@desc <- flfs@desc # eck
#    cout <- test_operatingModel_calc_target_value(flfs_max, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 8) 
#    expect_that(cout, is_identical_to(unname(fc@target@iters[8,"max",])))
#
#    # Try Min and Max values
#    # some iters are min, some are max, some are OK
#    catches_total <- catch(flfs[[1]][[1]])
#    catches_total[] <- 0
#    for (i in 1:length(flfs)){
#        catches_total <- catches_total + catch(flfs[[i]][[1]])
#    }
#    current_catches <- c(catches_total[,fc@target@element[7,"year"],1,fc@target@element[7,"season"],1,])
#    min_iters <- as.logical(round(runif(dim(flq)[6], min = 0, max = 1)))
#    max_iters <- !min_iters
#    max_catches <- current_catches
#    min_catches <- current_catches
#    min_catches[min_iters] <- min_catches[min_iters] * 1.01
#    max_catches[min_iters] <- max_catches[min_iters] * 1.1
#    min_catches[max_iters] <- min_catches[max_iters] * 0.9
#    max_catches[max_iters] <- max_catches[max_iters] * 0.99
#    fc@target@element[7,"quantity"] <- "catch"
#    fc@target@iters[7,"value",] <- NA
#    fc@target@iters[7,"min",] <- min_catches
#    fc@target@iters[7,"max",] <- max_catches
#    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 7) 
#    expect_that(cout[min_iters], is_identical_to(min_catches[min_iters]))
#    expect_that(cout[max_iters], is_identical_to(max_catches[max_iters]))
#    
#    # Try Min and Max relative values
#    # Catch cannot change by more than 5% in year 4
#    fc@target@element[8,"relYear"] <- 4L
#    fc@target@element[8,"relSeason"] <- 1L
#    fc@target@element[8,"quantity"] <- "catch"
#    fc@target@iters[8,"value",] <- NA
#    fc@target@iters[8,"min",] <- 0.95
#    fc@target@iters[8,"max",] <- 1.05
#    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 8) 
#    current_catches4 <- c(catches_total[,fc@target@element[4,"year"],1,fc@target@element[4,"season"],1,])
#    current_catches8 <- c(catches_total[,fc@target@element[8,"year"],1,fc@target@element[8,"season"],1,])
#    min_lim <- current_catches8 >= current_catches4
#    expect_that(all((cout[min_lim] / current_catches4[min_lim]) >= 0.95), is_true())
#    max_lim <- current_catches8 < current_catches4
#    expect_that(all((cout[max_lim] / current_catches4[max_lim]) <= 1.05), is_true())
#})
#
#
#test_that("Test projection with individual targets",{
#    # Test operating model.
#    # Need FLFisheries, FLBiol, SRR, F and Fspwn
#    data(ple4)
#    # make the biol
#    # srr
#    srr_model_name <- "bevholt"
#    srr <- as.FLSR(ple4, model=srr_model_name)
#    srr <- fmle(srr, control = list(trace=0))
#    srr_params <- as.FLQuant(params(srr))
#    srr_residuals <- exp(residuals(srr))
#    srr_residuals <- window(srr_residuals,end=2009)
#    srr_residuals[,"2009"] <- srr_residuals[,"2008"]
#    #srr_residuals[] <- 1 # turn off residuals
#    srr_residuals_mult <- TRUE
#    srr_timelag <- 1
#    niters <- 500
#    data(ple4)
#    ple4 <- propagate(ple4,niters)
#    biol <- as(ple4, 'FLBiol')
#    catch <- as(ple4, 'FLCatch')
#    catch@name <- "ple4 catch"
#    catch@desc <- "ple4 catch"
#    # Hack
#    fishery <- FLFishery(ple4=catch)
#    fishery@name <- "ple4 fishery"
#    fishery@desc <- "ple4 fishery"
#    fisheries <- FLFisheries(ple4=fishery)
#    fisheries@desc <- "ple4 fisheries"
#    # Fs
#    f <- list(ple4 = harvest(ple4))
#    f_spwn_flq <- harvest(ple4)
#    f_spwn_flq[] <- 0
#    f_spwn <- list(ple4 = f_spwn_flq)
#
#    # Add some noise on biol n and harvest
#    n(biol) <- n(biol) * rlnorm(prod(dim(n(biol))), sd = 0.1)
#    f[[1]] <- f[[1]] * rlnorm(prod(dim(f[[1]])), sd = 0.1)
#
#    # A simple target object - 1 year, 1 target
#    minAge <- 2
#    maxAge <- 6
#    # F target
#    target <- data.frame(year=2, quantity = 'f', value = 0.5, minAge=minAge, maxAge=maxAge, season=1L)
#    fwc <- fwdControl(target=target, iter=niters)
#    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1)
#    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
#    # equals?
#    expect_that(c(apply(out[["f"]][[1]][ac(minAge:maxAge),2] ,2:6,mean)), equals(unname(fwc@target@iters[1,"value",])))
#
#    # Special case - 0 F
#    target <- data.frame(year=2, quantity = 'f', value = 0, minAge=minAge, maxAge=maxAge, season=1L)
#    fwc <- fwdControl(target=target, iter=niters)
#    #fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1)
#    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
#    # equals?
#    expect_that(c(apply(out[["f"]][[1]][ac(minAge:maxAge),2] ,2:6,mean)), equals(unname(fwc@target@iters[1,"value",])))
#    #discards.ratio(out[["fisheries"]][[1]][[1]])[,2]
#    #discards.ratio(fisheries[[1]][[1]])[,2]
#
#    # Catch target
#    target <- data.frame(year=2, quantity = 'catch', value = 100000, minAge=minAge, maxAge=maxAge, season=1L)
#    fwc <- fwdControl(target=target, iter=niters)
#    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(100000), sd = 0.1)
#    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
#    expect_that(c(catch(out[["fisheries"]][[1]][[1]])[,2]), equals(unname(fwc@target@iters[1,"value",])))
#
#    # Biomass target
#    target <- data.frame(year=2, quantity = 'biomass', value = 300000, minAge=minAge, maxAge=maxAge, season=1L)
#    fwc <- fwdControl(target=target, iter=niters)
#    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1)
#    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
#    expect_that(c(quantSums(n(out[["biol"]]) * wt(out[["biol"]]))[,2]), equals(unname(fwc@target@iters[1,"value",])))
#
#    # SSB target
#    target <- data.frame(year=2, quantity = 'ssb', value = 300000, minAge=minAge, maxAge=maxAge, season=1L)
#    fwc <- fwdControl(target=target, iter=niters)
#    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1)
#    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
#    ssb_out <- quantSums(n(out[["biol"]]) * wt(out[["biol"]]) * fec(out[["biol"]]) * exp(-out[["f_spwn"]][[1]] * out[["f"]][[1]] - m(out[["biol"]]) * spwn(out[["biol"]])))
#    expect_that(c(ssb_out[,2]), equals(unname(fwc@target@iters[1,"value",])))
#})
#
#test_that("Test projection with really complicated target",{
#    # Test operating model.
#    # Need FLFisheries, FLBiol, SRR, F and Fspwn
#    data(ple4)
#    # make the biol
#    # srr
#    srr_model_name <- "bevholt"
#    srr <- as.FLSR(ple4, model=srr_model_name)
#    srr <- fmle(srr, control = list(trace=0))
#    srr_params <- as.FLQuant(params(srr))
#    srr_residuals <- exp(residuals(srr))
#    srr_residuals <- window(srr_residuals,end=2009)
#    srr_residuals[,"2009"] <- srr_residuals[,"2008"]
#    #srr_residuals[] <- 1 # turn off residuals
#    srr_residuals_mult <- TRUE
#    srr_timelag <- 1
#    niters <- 500
#    data(ple4)
#    ple4 <- propagate(ple4,niters)
#    biol <- as(ple4, 'FLBiol')
#    catch <- as(ple4, 'FLCatch')
#    catch@name <- "ple4 catch"
#    catch@desc <- "ple4 catch"
#    # Hack
#    fishery <- FLFishery(ple4=catch)
#    fishery@name <- "ple4 fishery"
#    fishery@desc <- "ple4 fishery"
#    fisheries <- FLFisheries(ple4=fishery)
#    fisheries@desc <- "ple4 fisheries"
#    # Fs
#    f <- list(ple4 = harvest(ple4))
#    f_spwn_flq <- harvest(ple4)
#    f_spwn_flq[] <- 0
#    f_spwn <- list(ple4 = f_spwn_flq)
#
#    # Add some noise on biol n and harvest
#    set.seed(1)
#    n(biol) <- n(biol) * rlnorm(prod(dim(n(biol))), sd = 0.1)
#    f[[1]] <- f[[1]] * rlnorm(prod(dim(f[[1]])), sd = 0.1)
#    minAge <- 2
#    maxAge <- 6
#
#    target <- data.frame(year = 2, quantity = 'f', value = 0.5, minAge = 2, maxAge = 6, season = 1L)
#    target <- rbind(target,
#        # Absolute values
#        data.frame(year = 3, quantity = 'catch', value = 100000, minAge = NA, maxAge = NA, season = 1L),
#        data.frame(year = 5, quantity = 'biomass', value = 300000, minAge = NA, maxAge = NA, season = 1L), # affects F in year 4
#        data.frame(year = 6, quantity = 'ssb', value = 300000, minAge = NA, maxAge = NA, season = 1L), # affects F in year 5
#        # Max targets
#        data.frame(year = 7, quantity = 'catch', value = 100000, minAge = NA, maxAge = NA, season = 1L), # Catch target
#        data.frame(year = 7, quantity = 'f', value = 1, minAge = 2, maxAge = 6, season = 1L), # but maximum F limits it - need to hack this line after constructor
#        # Min target
#        data.frame(year = 8, quantity = 'f', value = 0.2, minAge = 2, maxAge = 6, season = 1L), # F target
#        data.frame(year = 8, quantity = 'catch', value = 100000, minAge = NA, maxAge = NA, season = 1L), # but minimum catch limits it - need to hack this line after constructor
#        # Relative target
#        data.frame(year = 9, quantity = 'catch', value = 0.5, minAge = NA, maxAge = NA, season = 1L), # Need to hack this after constructor
#        # Relative max target
#        data.frame(year = 11, quantity = 'f', value = 0.5, minAge = 2, maxAge = 6, season = 1L), # F target
#        data.frame(year = 11, quantity = 'catch', value = 1.15, minAge = NA, maxAge = NA, season = 1L), # But catch cannot increase more than 15% from previous yearNeed to hack this after constructor
#        # Relative min target
#        data.frame(year = 13, quantity = 'f', value = 0.3, minAge = 2, maxAge = 6, season = 1L), # F target
#        data.frame(year = 13, quantity = 'catch', value = 0.85, minAge = NA, maxAge = NA, season = 1L) # But catch cannot increase more than 15% from previous yearNeed to hack this after constructor
#    )
#
#    fwc <- fwdControl(target=target, iter=niters)
#    # Hack min and max values of f target
#    fwc@target@element
#    fwc@target@element[6,c("value","max")] <- c(NA,0.3)
#    fwc@target@element[8,c("value","min")] <- c(NA,100000)
#    fwc@target@element[9,c("relYear","relSeason")] <- c(8L,1L)
#    fwc@target@element[11,c("value","max","relYear","relSeason")] <- c(NA, 1.15, 10L, 1L)
#    fwc@target@element[13,c("value","min","relYear","relSeason")] <- c(NA, 0.85, 12L, 1L)
#
#    # Fix iter values
#    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1) # f
#    fwc@target@iters[2,"value",] <- rlnorm(niters, mean = log(100000), sd = 0.1) # sd = 1 results in bad iters - good for testing
#    fwc@target@iters[3,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1) # biomass
#    fwc@target@iters[4,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1) # ssb
#    fwc@target@iters[5,"value",] <- rlnorm(niters, mean = log(100000), sd = 0.1) # catch for f max test
#    fwc@target@iters[6,"value",] <- NA # fbar has max only
#    fwc@target@iters[6,"max",] <- rlnorm(niters, mean = log(0.35), sd = 0.01)
#    fwc@target@iters[7,"value",] <- rlnorm(niters, mean = log(0.2), sd = 0.1) 
#    fwc@target@iters[8,"value",] <- NA # fbar has max only
#    fwc@target@iters[8,"min",] <- rlnorm(niters, mean = log(100000), sd = 0.1) 
#    fwc@target@iters[9,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1) 
#    fwc@target@iters[10,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1) 
#    fwc@target@iters[11,"value",] <- NA
#    fwc@target@iters[11,"max",] <- 1.15
#    fwc@target@iters[12,"value",] <- rlnorm(niters, mean = log(0.3), sd = 0.1) 
#    fwc@target@iters[13,"value",] <- NA
#    fwc@target@iters[13,"min",] <- 0.85 
#
#    # Call test run with (something like this will go in fwd() method)
#    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
#    ssb_out <- quantSums(n(out[["biol"]]) * wt(out[["biol"]]) * fec(out[["biol"]]) * exp(-out[["f_spwn"]][[1]] * out[["f"]][[1]] - m(out[["biol"]]) * spwn(out[["biol"]])))
#
#    # Check that discards_ratio has not been mucked about with
#    dr_orig <- discards.ratio(fisheries[[1]][[1]])
#    dr_out <- discards.ratio(out[["fisheries"]][[1]][[1]])
#    expect_that(dr_orig, equals(dr_out))
#
#    # Test output
#    # Absolute targets
#    expect_that(c(apply(out[["f"]][[1]][ac(minAge:maxAge),2] ,2:6,mean)), equals(unname(fwc@target@iters[1,"value",]))) # abs ssb
#    expect_that(c(catch(out[["fisheries"]][[1]][[1]])[,3]), equals(unname(fwc@target@iters[2,"value",]))) # abs catch
#    expect_that(c(quantSums(n(out[["biol"]]) * wt(out[["biol"]]))[,5]), equals(unname(fwc@target@iters[3,"value",]))) # abs biomass
#    expect_that(c(ssb_out[,6]), equals(unname(fwc@target@iters[4,"value",])))
#
#    # Max target - bit tricky for comparison - cannot use < > as differences are sometimes +-1e-16 
#    # so need tolerances
#    tol <- .Machine$double.eps ^ 0.5   
#    # Find which Catches have not been hit
#    fmax_iters <- abs((c(catch(out[["fisheries"]][[1]][[1]])[,7]) / unname(fwc@target@iters[5,"value",])) - 1) > tol
#    # Check that fmax has been hit for these iters
#    expect_that(all(abs((c(apply(out[["f"]][[1]][ac(minAge:maxAge),7] ,2:6,mean))[fmax_iters] / unname(fwc@target@iters[6,"max",])[fmax_iters]) -1) < tol), is_true())
#    # And that F is less than Fmax for the iters where catches have been hit
#    expect_that(all(c(apply(out[["f"]][[1]][ac(minAge:maxAge),7] ,2:6,mean))[!fmax_iters] < unname(fwc@target@iters[6,"max",])[!fmax_iters]), is_true())
#
#    # Min test
#    # Iters where F has not been hit
#    cmin_iters <- abs((c(apply(out[["f"]][[1]][ac(minAge:maxAge),8] ,2:6,mean)) / unname(fwc@target@iters[7,"value",]))-1) > tol
#    # cmin has been hit for these iters
#    expect_that(all(abs((c(catch(out[["fisheries"]][[1]][[1]])[,8])[cmin_iters] / unname(fwc@target@iters[8,"min",])[cmin_iters])-1) < tol), is_true())
#    # And that Catch is greater than Cmin for the other iters (where original f target was hit)
#    expect_that(all(c(catch(out[["fisheries"]][[1]][[1]])[,8])[!cmin_iters] > unname(fwc@target@iters[8,"min",])[!cmin_iters]), is_true())
#
#    # Relative test
#    expect_that(c(catch(out[["fisheries"]][[1]][[1]])[,8]) * unname(fwc@target@iters[9,"value",]), equals(c(catch(out[["fisheries"]][[1]][[1]])[,9])))
#
#    # Relative max test
#    # Which iters have been limited by cmax, i.e. f target not hit
#    cmax_iters <- abs((c(apply(out[["f"]][[1]][ac(minAge:maxAge),11] ,2:6,mean)) / unname(fwc@target@iters[10,"value",]))-1) > tol
#    # Check that these iters have hit the limit
#    # Catch should be at limit
#    expect_that(all( abs((c(catch(out[["fisheries"]][[1]][[1]])[,11])[cmax_iters] / (c(catch(out[["fisheries"]][[1]][[1]])[,10])[cmax_iters] * unname(fwc@target@iters[11,"max",])[cmax_iters]))-1) < tol), is_true())
#    # Unlimited iters should have catches less than max
#    expect_that(all(c(catch(out[["fisheries"]][[1]][[1]])[,11])[!cmax_iters] < (c(catch(out[["fisheries"]][[1]][[1]])[,10])[!cmax_iters] * unname(fwc@target@iters[11,"max",])[!cmax_iters])), is_true())
#
#    # Relative min test
#    # Which iters have been limited by cmax, i.e. f target not hit
#    cmax_iters <- abs(( c(apply(out[["f"]][[1]][ac(minAge:maxAge),13] ,2:6,mean)) / unname(fwc@target@iters[12,"value",]))-1) > tol
#    # These iters should be at catch limit
#    expect_that(all( abs((c(catch(out[["fisheries"]][[1]][[1]])[,13])[cmax_iters] / (c(catch(out[["fisheries"]][[1]][[1]])[,12])[cmax_iters] * unname(fwc@target@iters[13,"min",])[cmax_iters]))-1) < tol), is_true())
#    # Other iters should have catches greater than catch limit
#    expect_that(all(c(catch(out[["fisheries"]][[1]][[1]])[,13])[!cmax_iters] > (c(catch(out[["fisheries"]][[1]][[1]])[,12])[!cmax_iters] * unname(fwc@target@iters[13,"min",])[!cmax_iters])), is_true())
#
#
#})
#
#test_that("Test Jose bug with 0 landings and F bounds",{
#    # This used to stuff up as discards.ratio was recalculated
#    # With 0 landings leading to NA
#    data(ple4)
#    # make the biol
#    # srr
#    srr_model_name <- "bevholt"
#    srr <- as.FLSR(ple4, model=srr_model_name)
#    srr <- fmle(srr, control = list(trace=0))
#    srr_params <- as.FLQuant(params(srr))
#    srr_residuals <- exp(residuals(srr))
#    srr_residuals <- window(srr_residuals,end=2009)
#    srr_residuals[,"2009"] <- srr_residuals[,"2008"]
#    srr_residuals[] <- 1 # turn off residuals
#    srr_residuals_mult <- TRUE
#    srr_timelag <- 1
#    # catch -> fishery -> fisheries
#    #niters <- 500
#    #niters <- 10
#    niters <- 1 
#    #niters <- 1000
#    data(ple4)
#    ple4 <- propagate(ple4,niters)
#    biol <- as(ple4, 'FLBiol')
#    catch <- as(ple4, 'FLCatch')
#    catch@name <- "ple4 catch"
#    catch@desc <- "ple4 catch"
#    # Hack
#    fishery <- FLFishery(ple4=catch)
#    fishery@name <- "ple4 fishery"
#    fishery@desc <- "ple4 fishery"
#    fisheries <- FLFisheries(ple4=fishery)
#    fisheries@desc <- "ple4 fisheries"
#    # Fs
#    f <- list(ple4 = harvest(ple4))
#    f_spwn_flq <- harvest(ple4)
#    f_spwn_flq[] <- 0
#    f_spwn <- list(ple4 = f_spwn_flq)
#
#    minAge <- 2
#    maxAge <- 6
#
#    # 0 landings target, with min and max f
#    target <- data.frame(year=c(2,2,2), quantity = c('landings','f','f'), value = c(0.0,0.0,0.0), minAge=c(NA,minAge,minAge), maxAge=c(NA,maxAge,maxAge), season=1L)
#    fwc <- fwdControl(target=target, iter=niters)
#    # hack target
#    fwc@target@element[2,c("value","min")] <- c(NA, 0)
#    fwc@target@iters[2,c("value","min"),] <- c(NA, 0)
#    fwc@target@element[3,c("value","max")] <- c(NA, 1.5)
#    fwc@target@iters[3,c("value","max"),] <- c(NA, 1.5)
#
#    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
#
#    # Catch, landings and discards should be 0
#    expect_that(c(landings(out[["fisheries"]][[1]][[1]])[,2]), equals(fwc@target@iters[1,"value",]))
#    expect_that(c(catch(out[["fisheries"]][[1]][[1]])[,2]), equals(fwc@target@iters[1,"value",]))
#    expect_that(c(discards(out[["fisheries"]][[1]][[1]])[,2]), equals(fwc@target@iters[1,"value",]))
#    # F should be 0
#    expect_that(c(apply(out[["f"]][[1]][ac(minAge:maxAge),2],2:6,mean)), equals(0))
#})
