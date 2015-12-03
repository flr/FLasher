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
    # Empty constructor - just check it doesn't fail
    test_operatingModel_empty_constructor()
    # Main constructor test
    # Set up parameters for full test 
    flq <- random_FLQuant_generator()
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- FLBiols(lapply(flbs, function(x) return(x[["biol"]])))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Test as and wrap
    out <- test_operatingModel_full_constructor(flfs, flbs, fc)
    expect_identical(out[["biols"]], flbs_in)
    test_FLFisheries_equal(out[["fisheries"]], flfs)
    test_fwdControl_equal(out[["ctrl"]], fc)

    # biol - dimension check
    # Add tests for dimension checks
    # Residuals, fisheries, biols and who is fishing on what
})

# Test F method with random Biols and Fisheries
# No check if FC catches B
test_that("operatingModel get_f method for FCB with random OM objects",{
    flq <- random_FLQuant_generator(min_dims = c(2,2,2,2,2,2))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- FLBiols(lapply(flbs, function(x) return(x[["biol"]])))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    # fwdControl and FCB needed for constructor but not actually used to test F
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    attr(fc@target, "FCB") <- array(0, dim = c(10,3))
    # Any FLC FLB combination
    fishery_no <- round(runif(1,min=1, max=length(flfs)))
    catch_no <- round(runif(1,min=1, max=length(flfs[[fishery_no]])))
    biol_no <- round(runif(1,min=1, max=length(flbs)))
    cq_flq <- as(catch.q(flfs[[fishery_no]][[catch_no]]), "FLQuant")
    biomass <- quantSums(n(flbs[[biol_no]][["biol"]]) * wt(flbs[[biol_no]][["biol"]]))
    qin <- sweep(sweep(biomass, 6, -cq_flq[2,], "^"), 6, cq_flq[1], "*")
    fin <- sweep(catch.sel(flfs[[fishery_no]][[catch_no]]), 2:6, qin * effort(flfs[[fishery_no]]), "*")
    # Full FLQ
    fout <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)
    expect_equal(dim(fout), dim(fin))
    expect_equal(c(fout), c(fin))
    # Subset FLQ
    dim_max <- dim(flq)
    dim_min <- round(runif(6, min=1, max = dim_max))
    fout <- test_operatingModel_get_f_FCB_subset(flfs, flbs, fc, fishery_no, catch_no, biol_no, dim_min, dim_max)
    fin_sub <- fin[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2],dim_min[3]:dim_max[3],dim_min[4]:dim_max[4],dim_min[5]:dim_max[5],dim_min[6]:dim_max[6]]
    expect_equal(dim(fout), dim(fin_sub))
    expect_equal(c(fout), c(fin_sub))
    # With years in the Catch Q pars too
    flp <- FLPar(abs(rnorm(2 * dim(flq)[6] * dim(flq)[2])), dimnames = list(params = c("alpha","beta"), year=1:dim(flq)[2], iter = 1:dim(flq)[6]))
    catch.q(flfs[[fishery_no]][[catch_no]]) <- flp # desc removes! Why?
    cq_flq <- as(catch.q(flfs[[fishery_no]][[catch_no]]), "FLQuant")
    biomass <- quantSums(n(flbs[[biol_no]][["biol"]]) * wt(flbs[[biol_no]][["biol"]]))
    qin <- sweep(sweep(biomass, c(2,6), -cq_flq[2,], "^"), c(2,6), cq_flq[1], "*")
    fin <- sweep(catch.sel(flfs[[fishery_no]][[catch_no]]), 2:6, qin * effort(flfs[[fishery_no]]), "*")
    # Full FLQ
    fout <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)
    expect_equal(dim(fout), dim(fin))
    expect_equal(c(fout), c(fin))
    # Subset FLQ
    dim_max <- dim(flq)
    dim_min <- round(runif(6, min=1, max = dim_max))
    fout <- test_operatingModel_get_f_FCB_subset(flfs, flbs, fc, fishery_no, catch_no, biol_no, dim_min, dim_max)
    fin_sub <- fin[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2],dim_min[3]:dim_max[3],dim_min[4]:dim_max[4],dim_min[5]:dim_max[5],dim_min[6]:dim_max[6]]
    expect_equal(dim(fout), dim(fin_sub))
    expect_equal(c(fout), c(fin_sub))
})

test_that("get_f for biols with example operatingModel1",{
    # Get the total Fs on Biols
    # Uses the FCB attribute
    om <- make_test_operatingModel1(20)
    flfs <- om[["fisheries"]]
    flbs_in <- FLBiols(lapply(om[["biols"]], function(x) return(x[["biol"]])))
    nbiols <- length(om[["biols"]])
    for (i in 1:nbiols){
        dim <- dim(n(om[["biols"]][[i]][["biol"]]))
        indices_max <- round(runif(6,1,dim))
        indices_min <- round(runif(6,1,indices_max))
        fout <- test_operatingModel_get_f_B_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], i, indices_min,  indices_max)
        fout_all <- test_operatingModel_get_f_B(om[["fisheries"]], om[["biols"]], om[["fwc"]], i)
        FC <- om[["fwc"]]@target@FCB[om[["fwc"]]@target@FCB[,"B"] == i,,drop=FALSE]
        if (nrow(FC) > 0){
            # Loop over each FC
            biomass <- quantSums(n(flbs_in[[i]]) * wt(flbs_in[[i]]))
            fin <- FLQuant(0, dim=dim)
            for (j in 1:nrow(FC)){
                cq_flq <- as(catch.q(flfs[[FC[j,"F"]]][[FC[j,"C"]]]), "FLQuant")
                qin <- (c(cq_flq[1,])) * biomass ^ (-c(cq_flq[2,]))
                fin <- fin + sweep(catch.sel(flfs[[FC[j,"F"]]][[FC[j,"C"]]]), 2:6, qin * effort(flfs[[FC[j,"F"]]]), "*")
            }
            test_FLQuant_equal(fin, fout_all)
            test_FLQuant_equal(fin[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2],indices_min[3]:indices_max[3],indices_min[4]:indices_max[4],indices_min[5]:indices_max[5],indices_min[6]:indices_max[6]], fout)
        }
    }
})

test_that("operatingModel SRP methods",{
    # test f_prop_spwn
    # Random OM
    flq <- random_FLQuant_generator()
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- FLBiols(lapply(flbs, function(x) return(x[["biol"]])))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Random fishery and biol
    biol_no <- round(runif(1, min=1, max=length(flbs)))
    fishery_no <- round(runif(1, min=1, max=length(flfs)))
    # Get full FLQ
    indices_min <- rep(1,6)
    indices_max <- dim(n(flbs_in[[biol_no]]))

    #hperiod <- flfs[[fishery_no]]@hperiod
    #spwn(flbs_in[[biol_no]]) # should just be dim 1
    #fprop <- flq[1,]

    #fprop[hperiod[1,] > spwn(flbs_in[[biol_no]])[1,]] <- 0 # Set fprop to 0, all fishing after
    #fprop[hperiod[2,] < spwn(flbs_in[[biol_no]])[1,]] <- 1 # Set fprop to 1, all fishing before
    ## Others that are mixed
    ##straddle <- (hperiod[1,] < spwn(flbs_in[[biol_no]])[1,] & hperiod[2,] > spwn(flbs_in[[biol_no]])[1,])
    #straddle <- hperiod[1,] < spwn(flbs_in[[biol_no]])[1,] 

    #fprop_straddle <- (c(spwn(flbs_in[[biol_no]])[1,])[c(straddle)] - c(hperiod[1,])[c(straddle)]) / (c(hperiod[2,])[c(straddle)] - c(hperiod[1,])[c(straddle)])
    #fprop[hperiod[1,] < spwn(flbs_in[[biol_no]])[1,] & hperiod[2,] > spwn(flbs_in[[biol_no]])[1,]] <- fprop_straddle



    ## Cannot test as no hperiod[2,] in C++
    #prop_out <- test_operatingModel_f_prop_spwn_FLQ_subset(flfs, flbs, fc, fishery_no, biol_no, indices_min[-1], indices_max[-1])







    #om_bits <- make_test_operatingModel1(niters = 10)

    #hperiod <- lapply(om_bits[["fisheries"]], function(x) x@hperiod)

    #FLQuantAD test_operatingModel_f_prop_spwn_FLQ_subset(FLFisheriesAD flfs, Rcpp::List flbs_list, const fwdControl ctrl, const int fishery_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){



    #for (biol_no in 1:length(om_bits[["biols"]])){
    #    biol <- om_bits[["biols"]][[biol_no]][["biol"]]
    #    dims <- dim(n(biol))
    #    indices_min <- round(runif(5, min=1, max=ceiling(dims[-1] / 2)))
    #    indices_max <- round(runif(5, min=indices_min, max=dims[-1]))
    #    # F prop spwn
    #    fcb <- om_bits[["fwc"]]@target@FCB
    #    fc <- fcb[fcb[,"B"] == biol_no,,drop=FALSE]

    #    for (fc_count in 1:nrow(fc)){
    #        #hperiod <- 

    #    }

    #    }




    #    srp_out <- test_operatingModel_SRP_FLQ_subset(om_bits[["fisheries"]], om_bits[["biols"]], om_bits[["fwc"]], biol_no, indices_min, indices_max)
    #    # Without fishing atm

    #    # Get Fprop
    #    # Get F prop: F * Fprop

    #    srp_in <- quantSums(n(biol) * mat(biol) * wt(biol) * exp(-m(biol)*spwn(biol)))[, indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5]] 
    #    expect_equal(srp_in@.Data, srp_out@.Data)
    #}


})

test_that("operatingModel project_biol", {
    # Without Rec for the time being
    # Seasonal FLQ
    # Assume get_f method works (it does...)
    flq <- random_FLQuant_generator(fixed_dims = c(5,6,1,4,1,10))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 2, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- FLBiols(lapply(flbs, function(x) return(x[["biol"]])))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Fix FCB
    FCB <- array(NA, dim=c(3,3))
    FCB[1,] <- c(1,1,1)
    FCB[2,] <- c(1,2,2)
    FCB[3,] <- c(2,1,2)
    attr(fc@target, "FCB") <- FCB
    # Mid year 
    season <- round(runif(1,min=2,max=4))
    year <- round(runif(1,min=1,dim(flq)[2]))
    timestep <- test_year_season_to_timestep(flq, year, season)
    om_out <- test_operatingModel_project_biols(flfs, flbs, fc, timestep)
    for (biol_count in 1:2){
        fout <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_count)[,year,1,season-1,1,]
        old_n <- n(flbs_in[[biol_count]])[,year,1,season-1,1,]
        new_n_in <- n(om_out[["biols"]][[biol_count]])[,year,1,season,1,]
        survivors <- old_n * exp(- (fout + m(flbs_in[[biol_count]])[,year,1,season-1,1,]))
        # Ignore recruitment for now
        test_FLQuant_equal(survivors[-1,], new_n_in[-1,])
    }
    # Start of year - plusgroup
    season <- 1
    year <- round(runif(1,min=2,dim(flq)[2]))
    timestep <- test_year_season_to_timestep(flq, year, season)
    om_out <- test_operatingModel_project_biols(flfs, flbs, fc, timestep)
    for (biol_count in 1:2){
        fout <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_count)[,year-1,1,4,1,]
        old_n <- n(flbs_in[[biol_count]])[,year-1,1,4,1,]
        survivors <- old_n * exp(- (fout + m(flbs_in[[biol_count]])[,year-1,1,4,1,]))
        new_n_in <- n(om_out[["biols"]][[biol_count]])[,year,1,season,1,]
        # Ignore recruitment for now
        # Not plusgroup
        test_FLQuant_equal(survivors[1:(dim(flq)[1]-2),], new_n_in[2:(dim(flq)[1]-1),])
        # plusgroup
        test_FLQuant_equal(survivors[dim(flq)[1],] + survivors[dim(flq)[1]-1,], new_n_in[dim(flq)[1],])
    }
})

test_that("operatingModel project_fisheries", {
    # Seasonal FLQ
    # Assume get_f method works (it does...)
    flq <- random_FLQuant_generator(fixed_dims = c(5,6,1,4,1,10))
    flbs <- random_fwdBiols_list_generator(min_biols = 4, max_biols = 4, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- FLBiols(lapply(flbs, function(x) return(x[["biol"]])))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Fix FCB
    FCB <- array(NA, dim=c(5,3))
    FCB[1,] <- c(1,1,1)
    FCB[2,] <- c(1,2,2)
    FCB[3,] <- c(2,1,2)
    FCB[4,] <- c(2,2,3)
    FCB[5,] <- c(2,2,4)
    attr(fc@target, "FCB") <- FCB
    # Random timestep
    season <- round(runif(1,min=1,max=4))
    year <- round(runif(1,min=1,dim(flq)[2]))
    timestep <- test_year_season_to_timestep(flq, year, season)
    om_out <- test_operatingModel_project_fisheries(flfs, flbs, fc, timestep)
    # FC 11
    fishery_no <- 1
    catch_no <- 1
    biol_no <- 1
    lout <- landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dout <- discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    cout <- catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    # Check that timestep is correct
    fin <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,1,season,1,]
    zin <- fin + m(flbs_in[[biol_no]])[,year,1,season,1,]
    cin <- (fin / zin) * (1 - exp(-zin)) * n(flbs_in[[biol_no]])[,year,1,season,1]
    dr <- (discards.n(flfs[[fishery_no]][[catch_no]]) / (discards.n(flfs[[fishery_no]][[catch_no]]) + landings.n(flfs[[fishery_no]][[catch_no]])))[,year,,season,]
    # Catch, Landings, Discards numbers
    test_FLQuant_equal(cin, cout)
    test_FLQuant_equal(cin*(1-dr), lout)
    test_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq))) %in% get_FLQuant_elements(flq, c(1,year,1,season,1,1), c(dim(flq)[1], year, 1, season, 1, dim(flq)[6])))
    expect_equal(c(landings.n(flfs[[fishery_no]][[catch_no]]))[elem], c(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    expect_equal(c(discards.n(flfs[[fishery_no]][[catch_no]]))[elem], c(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    # FC 12
    fishery_no <- 1
    catch_no <- 2
    biol_no <- 2
    lout <- landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dout <- discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    cout <- catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    fin <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,1,season,1,]
    total_fin <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,1,season,1,]
    zin <- total_fin + m(flbs_in[[biol_no]])[,year,1,season,1,]
    cin <- (fin / zin) * (1 - exp(-zin)) * n(flbs_in[[biol_no]])[,year,1,season,1]
    dr <- (discards.n(flfs[[fishery_no]][[catch_no]]) / (discards.n(flfs[[fishery_no]][[catch_no]]) + landings.n(flfs[[fishery_no]][[catch_no]])))[,year,,season,]
    # Catch, Landings, Discards numbers
    test_FLQuant_equal(cin, cout)
    test_FLQuant_equal(cin*(1-dr), lout)
    test_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq))) %in% get_FLQuant_elements(flq, c(1,year,1,season,1,1), c(dim(flq)[1], year, 1, season, 1, dim(flq)[6])))
    expect_equal(c(landings.n(flfs[[fishery_no]][[catch_no]]))[elem], c(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    expect_equal(c(discards.n(flfs[[fishery_no]][[catch_no]]))[elem], c(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    # FC 21
    fishery_no <- 2
    catch_no <- 1
    biol_no <- 2
    lout <- landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dout <- discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    cout <- catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    fin <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,1,season,1,]
    total_fin <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,1,season,1,]
    zin <- total_fin + m(flbs_in[[biol_no]])[,year,1,season,1,]
    cin <- (fin / zin) * (1 - exp(-zin)) * n(flbs_in[[biol_no]])[,year,1,season,1]
    dr <- (discards.n(flfs[[fishery_no]][[catch_no]]) / (discards.n(flfs[[fishery_no]][[catch_no]]) + landings.n(flfs[[fishery_no]][[catch_no]])))[,year,,season,]
    # Catch, Landings, Discards numbers
    test_FLQuant_equal(cin, cout)
    test_FLQuant_equal(cin*(1-dr), lout)
    test_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq))) %in% get_FLQuant_elements(flq, c(1,year,1,season,1,1), c(dim(flq)[1], year, 1, season, 1, dim(flq)[6])))
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
    fin3 <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,1,season,1,]
    total_fin3 <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,1,season,1,]
    zin3 <- total_fin3 + m(flbs_in[[biol_no]])[,year,1,season,1,]
    cin3 <- (fin3 / zin3) * (1 - exp(-zin3)) * n(flbs_in[[biol_no]])[,year,1,season,1]
    biol_no <- 4
    fin4 <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,1,season,1,]
    total_fin4 <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,1,season,1,]
    zin4 <- total_fin4 + m(flbs_in[[biol_no]])[,year,1,season,1,]
    cin4 <- (fin4 / zin4) * (1 - exp(-zin4)) * n(flbs_in[[biol_no]])[,year,1,season,1]
    cin <- cin3 + cin4
    # Catch, Landings, Discards numbers
    test_FLQuant_equal(cin, cout)
    test_FLQuant_equal(cin*(1-dr), lout)
    test_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq))) %in% get_FLQuant_elements(flq, c(1,year,1,season,1,1), c(dim(flq)[1], year, 1, season, 1, dim(flq)[6])))
    expect_equal(c(landings.n(flfs[[fishery_no]][[catch_no]]))[elem], c(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    expect_equal(c(discards.n(flfs[[fishery_no]][[catch_no]]))[elem], c(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
})

test_that("operatingModel landings, catch and discards methods",{
    # These methods evaluate the current state of the OM
    # i.e. just pull values out, no calculation
    om <- make_test_operatingModel1(10)
    dim_max <- dim(n(om[["biols"]][[1]][["biol"]]))
    dim_min <- round(runif(6, min=1, max=dim_max))
    year <- round(runif(1,min=dim_min[2],max=dim_max[2]))
    season <- round(runif(1,min=dim_min[4],max=dim_max[4]))
    timestep <- (year-1) * dim(n(om[["biols"]][[1]][["biol"]]))[4] + season

    # 1 biol -> 1 catch
    biol_no <- 1
    landings1_out <- test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    landings1_in <- landings(om[["fisheries"]][[1]][[1]])
    test_FLQuant_equal(landings1_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], landings1_out)
    discards1_out <- test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    discards1_in <- discards(om[["fisheries"]][[1]][[1]])
    test_FLQuant_equal(discards1_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], discards1_out)
    catch1_out <- test_operatingModel_catches_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    catch1_in <- catch(om[["fisheries"]][[1]][[1]])
    test_FLQuant_equal(catch1_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], catch1_out)

    # 1 biol -> 2 catch
    biol_no <- 2
    landings2_out <- test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    landings12_in <- landings(om[["fisheries"]][[1]][[2]])
    landings21_in <- landings(om[["fisheries"]][[2]][[1]])
    test_FLQuant_equal((landings12_in+landings21_in)[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], landings2_out)
    discards2_out <- test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    discards12_in <- discards(om[["fisheries"]][[1]][[2]])
    discards21_in <- discards(om[["fisheries"]][[2]][[1]])
    test_FLQuant_equal((discards12_in+discards21_in)[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], discards2_out)
    catch2_out <- test_operatingModel_catches_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    catch12_in <- catch(om[["fisheries"]][[1]][[2]])
    catch21_in <- catch(om[["fisheries"]][[2]][[1]])
    test_FLQuant_equal((catch12_in+catch21_in)[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], catch2_out)

    # 2 biol -> 1 catch
    # Not yet implemented for a single catch so fails
    biol_no <- 3
    expect_error(test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))
    expect_error(test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))
    expect_error(test_operatingModel_catch_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))
    biol_no <- 4
    expect_error(test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))
    expect_error(test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))
    expect_error(test_operatingModel_catch_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))

})

test_that("operatingModel eval_om", {
    niters <- 10 
    om <- make_test_operatingModel1(niters)
    dim_max <- dim(n(om[["biols"]][[1]][["biol"]]))
    dim_min <- round(runif(6, min=1, max=dim_max))

    # Catch
    # 1, 1, 1 Cannot ask for biol and a catch
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", 1, 1, 1, dim_min[-1], dim_max[-1]))
    # 1, 2, NA
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", 1, 2, as.integer(NA), dim_min[-1], dim_max[-1])
    cin <- catch(om[["fisheries"]][[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]] 
    test_FLQuant_equal(cout, cin)
    # NA, NA, 2
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", as.integer(NA), as.integer(NA), 2, dim_min[-1], dim_max[-1])
    cin <- test_operatingModel_catches_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(cout, cin)
    # NA, NA, 3 error
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", as.integer(NA), as.integer(NA), 3, dim_min[-1], dim_max[-1]))

    # Landings
    # 1, 1, 1 Cannot ask for biol and a catch
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", 1, 1, 1, dim_min[-1], dim_max[-1]))
    # 1, 2, NA
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", 1, 2, as.integer(NA), dim_min[-1], dim_max[-1])
    cin <- landings(om[["fisheries"]][[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]] 
    test_FLQuant_equal(cout, cin)
    # NA, NA, 2
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", as.integer(NA), as.integer(NA), 2, dim_min[-1], dim_max[-1])
    cin <- test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(cout, cin)
    # NA, NA, 3 error
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", as.integer(NA), as.integer(NA), 3, dim_min[-1], dim_max[-1]))

    # Discards
    # 1, 1, 1 Cannot ask for biol and a catch
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", 1, 1, 1, dim_min[-1], dim_max[-1]))
    # 1, 2, NA
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", 1, 2, as.integer(NA), dim_min[-1], dim_max[-1])
    cin <- discards(om[["fisheries"]][[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]] 
    test_FLQuant_equal(cout, cin)
    # NA, NA, 2
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", as.integer(NA), as.integer(NA), 2, dim_min[-1], dim_max[-1])
    cin <- test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(cout, cin)
    # NA, NA, 3 error
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", as.integer(NA), as.integer(NA), 3, dim_min[-1], dim_max[-1]))

})

test_that("get_target_value_hat", {
    niters <- 10 
    flq <- random_FLQuant_generator(fixed_dims = c(5,20,1,4,1,niters))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 2, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- FLBiols(lapply(flbs, function(x) return(x[["biol"]])))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
    # Fix FCB
    FCB <- array(NA, dim=c(3,3))
    FCB[1,] <- c(1,1,1)
    FCB[2,] <- c(1,2,2)
    FCB[3,] <- c(2,1,2)
    # Make an fwdControl to test
    # Two sim catch targets
    years <- rep(round(runif(4, min=1,max=dim(flq)[2])),each=2)
    seasons <- rep(round(runif(4, min=1,max=dim(flq)[4])),each=2)
    timesteps <- (years-1) * dim(flq)[4] + seasons;
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], timestep = timesteps[1:2],
                        quantity = c("catch","catch"), target = 1, value = 10,
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2),
                        relFishery = NA, relCatch = NA, relBiol = NA,
                        relYear = NA, relSeason = NA)
    trgt2 <- data.frame(year = years[3:4], season = seasons[3:4], timestep = timesteps[3:4],
                        quantity = c("landings","discards"), target = 2, value = 10,
                        fishery = c(NA,1), catch = c(NA,2), biol = c(1,NA),
                        relFishery = NA, relCatch = NA, relBiol = NA,
                        relYear = NA, relSeason = NA)
    rel_years <- rep(round(runif(4, min=1,max=dim(flq)[2])),each=2)
    rel_seasons <- rep(round(runif(4, min=1,max=dim(flq)[4])),each=2)
    rel_trgt1 <- data.frame(year = years[5:6], season = seasons[5:6], timestep = timesteps[5:6],
                        quantity = c("catch","catch"), target = 3, value = 10,
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2),
                        relFishery = c(1,NA), relCatch = c(1,NA), relBiol = c(NA,2),
                        relYear = rel_years[5:6], relSeason = rel_seasons[5:6])
    # Discards relative to different catch and fishery
    rel_trgt2 <- data.frame(year = years[7:8], season = seasons[7:8], timestep = timesteps[7:8],
                        quantity = c("landings","discards"), target = 4, value = 10,
                        fishery = c(NA,1), catch = c(NA,2), biol = c(1,NA),
                        relFishery = c(NA,2), relCatch = c(NA,1), relBiol = c(1,NA),
                        relYear = rel_years[7:8], relSeason = rel_seasons[7:8])
    fwc <- fwdControl(rbind(trgt1, trgt2, rel_trgt1, rel_trgt2))
    attr(fwc@target, "FCB") <- FCB
    # Target 1 - 1 sim target at a time
    val_hat1 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 1, 1)
    val_in1 <- c(catch(flfs[[1]][[1]])[,years[1],,seasons[1]])
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 1, 2)
    val_in2 <- c(catch(flfs[[1]][[2]])[,years[2],,seasons[2]]) + c(catch(flfs[[2]][[1]])[,years[2],,seasons[2]])
    expect_equal(val_hat2, val_in2)
    # Both sim targets
    val_hat <- test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_in1,val_in2))
    # Target 2 - 1 sim target at a time
    val_hat1 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 2, 1)
    val_in1 <- c(landings(flfs[[1]][[1]])[,years[3],,seasons[3]])
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 2, 2)
    val_in2 <- c(discards(flfs[[1]][[2]])[,years[4],,seasons[4]])
    expect_equal(val_hat2, val_in2)
    # Both sim targets
    val_hat <- test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 2)
    expect_equal(val_hat, c(val_in1,val_in2))
    # Target 3 - Relative - 1 sim target at a time
    val_hat1 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 3, 1)
    val_in1 <- c(catch(flfs[[1]][[1]])[,years[5],,seasons[5]]) / c(catch(flfs[[1]][[1]])[,rel_years[5],,rel_seasons[5]])
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 3, 2)
    val_in2 <- (c(catch(flfs[[1]][[2]])[,years[6],,seasons[6]]) + c(catch(flfs[[2]][[1]])[,years[6],,seasons[6]])) / (c(catch(flfs[[1]][[2]])[,rel_years[6],,rel_seasons[6]]) + c(catch(flfs[[2]][[1]])[,rel_years[6],,rel_seasons[6]]))
    expect_equal(val_hat2, val_in2)
    # Both sim targets
    val_hat <- test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 3)
    expect_equal(val_hat, c(val_in1,val_in2))
    # Target 4 - Relative - 1 sim target at a time
    val_hat1 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 4, 1)
    val_in1 <- c(landings(flfs[[1]][[1]])[,years[7],,seasons[7]]) / c(landings(flfs[[1]][[1]])[,rel_years[7],,rel_seasons[7]])
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 4, 2)
    val_in2 <- c(discards(flfs[[1]][[2]])[,years[8],,seasons[8]]) / c(discards(flfs[[2]][[1]])[,rel_years[8],,rel_seasons[8]])
    expect_equal(val_hat2, val_in2)
    # Both sim targets
    val_hat <- test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 4)
    expect_equal(val_hat, c(val_in1,val_in2))
    # Relative fails due to not being set properly
    rel_trgt3 <- data.frame(year = 1:8, season = 1, timestep = 1:8,
                        quantity = "catch", target = 1:8,
                        fishery = 1, catch = 1, biol = NA,
                        relFishery = c(NA,1,1,1,1,NA,NA,NA), relCatch = c(1,NA,1,1,1,NA,NA,NA), relBiol = c(NA,NA,NA,NA,NA,1,1,NA),
                        relYear = c(1,1,NA,NA,1,1,NA,1), relSeason = c(1,1,NA,1,NA,NA,1,1))
    fwc <- fwdControl(rel_trgt3)
    attr(fwc@target, "FCB") <- FCB
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 1))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 2)) 
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 3))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 4))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 5))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 6))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 7))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 8))
})

test_that("get_target_value", {
    niters <- 10 
    flq <- random_FLQuant_generator(fixed_dims = c(5,20,1,4,1,niters))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 2, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- FLBiols(lapply(flbs, function(x) return(x[["biol"]])))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
    # Fix FCB
    FCB <- array(NA, dim=c(3,3))
    FCB[1,] <- c(1,1,1)
    FCB[2,] <- c(1,2,2)
    FCB[3,] <- c(2,1,2)
    # Make an fwdControl to test
    # Two sim catch targets
    years <- rep(round(runif(4, min=1,max=dim(flq)[2])),each=2)
    seasons <- rep(round(runif(4, min=1,max=dim(flq)[4])),each=2)
    timesteps <- (years-1) * dim(flq)[4] + seasons;
    value <- abs(rnorm(4))
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], timestep = timesteps[1:2],
                        quantity = c("catch","catch"), target = 1, value = value[1:2],
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2),
                        relFishery = NA, relCatch = NA, relBiol = NA,
                        relYear = NA, relSeason = NA)
    trgt2 <- data.frame(year = years[3:4], season = seasons[3:4], timestep = timesteps[3:4],
                        quantity = c("landings","discards"), target = 2, value = value[3:4],
                        fishery = c(NA,1), catch = c(NA,2), biol = c(1,NA),
                        relFishery = NA, relCatch = NA, relBiol = NA,
                        relYear = NA, relSeason = NA)
    # 1 iter - should blow up
    fwc <- fwdControl(rbind(trgt1, trgt2))
    attr(fwc@target, "FCB") <- FCB
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    val_in1 <- rep(fwc@target@iters[1,"value",],niters)
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    val_in2 <- rep(fwc@target@iters[2,"value",],niters)
    expect_equal(val_hat2, val_in2)
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_in1, val_in2))
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 2, 1)
    val_in1 <- rep(fwc@target@iters[3,"value",],niters)
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 2, 2)
    val_in2 <- rep(fwc@target@iters[4,"value",],niters)
    expect_equal(val_hat2, val_in2)
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 2)
    expect_equal(val_hat, c(val_in1, val_in2))
    # Same number of iters
    fwc <- fwdControl(rbind(trgt1, trgt2), iters=niters)
    fwc@target@iters[,"value",] <- abs(rnorm(4*niters))
    attr(fwc@target, "FCB") <- FCB
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    val_in1 <- fwc@target@iters[1,"value",]
    expect_equal(val_hat1, unname(val_in1))
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    val_in2 <- fwc@target@iters[2,"value",]
    expect_equal(val_hat2, unname(val_in2)
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, unname(c(val_in1, val_in2)))
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 2, 1)
    val_in1 <- fwc@target@iters[3,"value",]
    expect_equal(val_hat1, unname(val_in1))
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 2, 2)
    val_in2 <- fwc@target@iters[4,"value",]
    expect_equal(val_hat2, unname(val_in2))
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 2)
    expect_equal(val_hat, unname(c(val_in1, val_in2)))
    # Should fail
    fwc <- fwdControl(rbind(trgt1, trgt2), iters=niters-1)
    fwc@target@iters[,"value",] <- abs(rnorm(4*(niters-1)))
    attr(fwc@target, "FCB") <- FCB
    expect_error(test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1))

    # Min / Max tests
    limit_iters <- sample(1:niters, 5)
    not_limit_iters <- (1:niters)[!((1:niters) %in% limit_iters)]
    catch1 <- c(catch(flfs[[1]][[1]])[,years[1],,seasons[[1]]])
    max_catch1 <- catch1
    max_catch1[limit_iters] <- max_catch1[limit_iters] * 0.9
    max_catch1[not_limit_iters] <- max_catch1[not_limit_iters] * 1.1
    catch2 <- c(catch(flfs[[1]][[2]])[,years[1],,seasons[[1]]] + catch(flfs[[2]][[1]])[,years[1],,seasons[[1]]])
    max_catch2 <- catch2
    max_catch2[limit_iters] <- max_catch2[limit_iters] * 0.9
    max_catch2[not_limit_iters] <- max_catch2[not_limit_iters] * 1.1
    # Max
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], timestep = timesteps[1:2],
                        quantity = c("catch","catch"), target = 1, max = catch1[1],
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2),
                        relFishery = NA, relCatch = NA, relBiol = NA,
                        relYear = NA, relSeason = NA)
    fwc <- fwdControl(trgt1, niters)
    fwc@target@iters[1,"max",] <- max_catch1
    fwc@target@iters[2,"max",] <- max_catch2
    attr(fwc@target, "FCB") <- FCB
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    # only the limit_iters should equal values in control
    expect_equal(val_hat1[limit_iters], unname(fwc@target@iters[1,"max",limit_iters]))
    # Others same as in OM
    expect_equal(val_hat1[not_limit_iters], catch1[not_limit_iters])
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    expect_equal(val_hat2[limit_iters], unname(fwc@target@iters[2,"max",limit_iters]))
    expect_equal(val_hat2[not_limit_iters], catch2[not_limit_iters])
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_hat1, val_hat2))
    # Min
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], timestep = timesteps[1:2],
                        quantity = c("catch","catch"), target = 1, min = catch1[1],
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2),
                        relFishery = NA, relCatch = NA, relBiol = NA,
                        relYear = NA, relSeason = NA)
    fwc <- fwdControl(trgt1, niters)
    fwc@target@iters[1,"min",] <- max_catch1
    fwc@target@iters[2,"min",] <- max_catch2
    attr(fwc@target, "FCB") <- FCB
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    expect_equal(val_hat1[not_limit_iters], unname(fwc@target@iters[1,"min",not_limit_iters]))
    # Others same as in OM
    expect_equal(val_hat1[limit_iters], catch1[limit_iters])
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    expect_equal(val_hat2[not_limit_iters], unname(fwc@target@iters[2,"min",not_limit_iters]))
    expect_equal(val_hat2[limit_iters], catch2[limit_iters])
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_hat1, val_hat2))

    # Min and Max
    min_limit_iters <- sample(1:niters, 3)
    max_limit_iters <- sample((1:niters)[!((1:niters) %in% min_limit_iters)], 3)
    not_min_limit_iters <- (1:niters)[!((1:niters) %in% min_limit_iters)]
    not_max_limit_iters <- (1:niters)[!((1:niters) %in% max_limit_iters)]

    catch1 <- c(catch(flfs[[1]][[1]])[,years[1],,seasons[[1]]])
    max_catch1 <- catch1
    max_catch1[max_limit_iters] <- max_catch1[max_limit_iters] * 0.9
    max_catch1[not_max_limit_iters] <- max_catch1[not_max_limit_iters] * 1.1

    min_catch1 <- catch1
    min_catch1[not_min_limit_iters] <- min_catch1[not_min_limit_iters] * 0.9
    min_catch1[min_limit_iters] <- min_catch1[min_limit_iters] * 1.1

    catch2 <- c(catch(flfs[[1]][[2]])[,years[1],,seasons[[1]]] + catch(flfs[[2]][[1]])[,years[1],,seasons[[1]]])
    max_catch2 <- catch2
    max_catch2[max_limit_iters] <- max_catch2[max_limit_iters] * 0.9
    max_catch2[not_max_limit_iters] <- max_catch2[not_max_limit_iters] * 1.1
    min_catch2 <- catch2
    min_catch2[not_min_limit_iters] <- min_catch2[not_min_limit_iters] * 0.9
    min_catch2[min_limit_iters] <- min_catch2[min_limit_iters] * 1.1


    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], timestep = timesteps[1:2],
                        quantity = c("catch","catch"), target = 1, min = catch1[1], 
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2),
                        relFishery = NA, relCatch = NA, relBiol = NA,
                        relYear = NA, relSeason = NA)
    fwc <- fwdControl(trgt1, niters)
    # Constructor busted so hack
    fwc@target@element$max = catch1[1]

    fwc@target@iters[1,"max",] <- max_catch1
    fwc@target@iters[1,"min",] <- min_catch1
    fwc@target@iters[2,"max",] <- max_catch2
    fwc@target@iters[2,"min",] <- min_catch2
    attr(fwc@target, "FCB") <- FCB
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)


# Something going wrong with min / max together

    # only the limit_iters should equal values in control
    expect_equal(val_hat1[max_limit_iters], unname(fwc@target@iters[1,"max",max_limit_iters]))

    expect_equal(val_hat1[min_limit_iters], unname(fwc@target@iters[1,"min",min_limit_iters]))

    # Others same as in OM
    expect_equal(val_hat1[not_limit_iters], catch1[not_limit_iters])



    # Min and Max with 1 iter in control



    # Min and Max Rel






})

#test_that("operatingModel Q methods",{
#    flq <- random_FLQuant_generator()
#    flq <- random_FLQuant_generator(fixed_dims = c(10,52,1,1,1,20))
#    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
#    # Pull out just FLBiols for testing
#    flbs_in <- FLBiols(lapply(flbs, function(x) return(x[["biol"]])))
#    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
#    fc <- dummy_fwdControl_generator(years = 1, niters = dim(flq)[6])
#    fishery_no <- round(runif(1,min=1, max=length(flfs)))
#    catch_no <- round(runif(1,min=1, max=length(flfs[[fishery_no]])))
#    biol_no <- round(runif(1,min=1, max=length(flbs)))
#    cq_flq <- as(catch.q(flfs[[fishery_no]][[catch_no]]), "FLQuant")
#    biomass <- quantSums(n(flbs[[biol_no]][["biol"]]) * wt(flbs[[biol_no]][["biol"]]))
#    qin <- sweep(sweep(biomass, 6, -cq_flq[2,], "^"), 6, cq_flq[1], "*")
#    # FLQuantAD subset
#    dims_max <- dim(n(flbs[[1]][["biol"]]))
#    dims_min <- round(runif(6, min=1,max=dims_max))
#    expect_that(test_operatingModel_catch_q_subset(flfs, flbs, fc, fishery_no, catch_no, biol_no, dims_min, dims_max), throws_error()) # indices too long
#    qout <- test_operatingModel_catch_q_subset(flfs, flbs, fc, fishery_no, catch_no, biol_no, dims_min[-1], dims_max[-1])
#    qin_subset <- qin[, dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]]
#    # Dimnames not fixed so check contents and dim
#    expect_that(c(qout), equals(c(qin_subset)))
#    expect_that(dim(qout), equals(dim(qin_subset)))
#    # FLQuantAD
#    qout <- test_operatingModel_catch_q_FLQuantAD(flfs, flbs, fc, fishery_no, catch_no, biol_no)
#    expect_that(c(qout), equals(c(qin)))
#    expect_that(dim(qout), equals(dim(qin)))
#    # Single value
#    indices <- round(runif(5, min = 1, max = dim(flq)[-1]))
#    qout <- test_operatingModel_catch_q_adouble(flfs, flbs, fc, fishery_no, catch_no, biol_no, indices)
#    expect_that(qout, equals(c(qin[1, indices[1],indices[2],indices[3],indices[4],indices[5]])))
#})
#
#test_that("age_range_indices", {
#    # Ages 1 to 10
#    flq <- random_FLQuant_generator(fixed_dims = 10)
#    dimnames(flq)$age = as.character(1:10)
#    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
#    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
#    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
#    fc@target@element[1,c("minAge", "maxAge", "target")] <- c(1, 10, 1)
#    range <- test_operatingModel_get_target_age_range_indices(flfs, flbs, fc, 1, 1, 1)
#    expect_that(range, equals(c(0,9)))
#    # Start ages 3 
#    dimnames(flq)$age = as.character(3:12)
#    flb <- FLBiol(n = flq, desc = "something", name = "something")
#    flbs[[1]][["biol"]] <- flb
#    fc@target@element[1,c("minAge", "maxAge", "target")] <- c(5, 10, 1)
#    range <- test_operatingModel_get_target_age_range_indices(flfs, flbs, fc, 1, 1, 1)
#    expect_that(range, equals(c(2,7)))
#    # Outside min age
#    fc@target@element[1,c("minAge", "maxAge", "target")] <- c(2, 10, 1)
#    expect_that(test_operatingModel_get_target_age_range_indices(flfs, flbs, fc, 1, 1, 1), throws_error())
#    # Outside max
#    fc@target@element[1,c("minAge", "maxAge", "target")] <- c(3, 15, 1)
#    expect_that(test_operatingModel_get_target_age_range_indices(flfs, flbs, fc, 1, 1, 1), throws_error())
#})
#


#test_that("operatingModel annual project fisheries and biols",{
#    # Project fisheries and biols seperately - otherwise need to update the R test OM with the updated biols
#    # A faff so we have two seperate OMs, one with updated fisheries, the other with updated biols
#    om <- make_test_operatingModel1(20)
#
#    # Random timestep
#    dims <- dim(n(om[["biols"]][[1]][["biol"]]))
#    year <- round(runif(1,min=1,max=dims[2]))
#    season <- round(runif(1,min=1,max=dims[4]))
#    timestep <- (year-1)*dims[4] + season
#    #yr <- floor((timestep - 1) / dims[4]) + 1
#    #sn <- (timestep-1) %% dims[4] + 1
#    prev_year <- floor((timestep - 2) / dims[4]) + 1
#    prev_season <- (timestep-2) %% dims[4] + 1
#
#    #om_out <- test_operatingModel_project_biols_then_fisheries(om[["fisheries"]], om[["biols"]], om[["fwc"]], timestep)
#    om_out_b <- test_operatingModel_project_biols(om[["fisheries"]], om[["biols"]], om[["fwc"]], timestep)
#    om_out_f <- test_operatingModel_project_fisheries(om[["fisheries"]], om[["biols"]], om[["fwc"]], timestep)
#
#    dim_max <- dims
#    dim_min <- round(runif(6, min=1, max=dim_max))
#
#    #------------------
#    # 1 biol -> 1 catch
#    #------------------
#    biol_no <- 1
#    fishery_no <- 1
#    catch_no <- 1
#    # Total F on the biol
#    f1 <- test_operatingModel_total_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    f1_sub <- test_operatingModel_total_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
#    fbar1 <- test_operatingModel_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
#    expect_that(apply(f1_sub, 2:6, mean), equals(fbar1))
#    # Partial F from FC
#    pf11 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    pf11_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    fbar11 <- test_operatingModel_partial_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(apply(pf11_sub, 2:6, mean), equals(fbar11))
#    # Total Z of biol
#    z1 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    # SSB of biol
#    ssb1_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    ssb1_out_subset <- test_operatingModel_SSB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
#    # Get metrics from IP OM
#    biomass_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
#    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
#    qin <- sweep(sweep(biomass_in, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
#    f1in <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
#    z1in <- f1in + m(om[["biols"]][[biol_no]][["biol"]])
#    ssb1_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]) * fec(om[["biols"]][[biol_no]][["biol"]]) * exp(-((f1in+ m(om[["biols"]][[biol_no]][["biol"]]))* spwn(om[["biols"]][[biol_no]][["biol"]]))))
#    catch.n1 <- (f1in / z1in) * (1 - exp(-z1in)) * n(om[["biols"]][[biol_no]][["biol"]])
#    landings.n1 <- (catch.n1[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]))
#    discards.n1 <- (catch.n1[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]))
#    # Update biol abundances
#    next_n <- n(om[["biols"]][[biol_no]][["biol"]])[,year,,season,,]
#    next_n[] <- 0
#    next_n[2:dims[1],] <- (n(om[["biols"]][[biol_no]][["biol"]])[,prev_year,,prev_season,,] * exp(-z1in[,prev_year,,prev_season,,]))[1:(dims[1]-1),]
#    next_n[dims[1]] <- next_n[dims[1]] + (n(om[["biols"]][[biol_no]][["biol"]])[dims[1],prev_year,,prev_season,,] * exp(-z1in[dims[1],prev_year,,prev_season,,]))
#    # Rec - it's BH for Biol 1
#    ssb_timestep <- timestep - om[["biols"]][[biol_no]][["srr_timelag"]]
#    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
#    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
#    ssb_rec <- ssb1_in[,ssb_year,,ssb_season,,]
#    rec_in <- om[["biols"]][[biol_no]][["srr_params"]]['a',] * ssb_rec / (om[["biols"]][[biol_no]][["srr_params"]]['b',] + ssb_rec)
#    # Multiplicative residuals
#    next_n[1,] <- rec_in * exp(om[["biols"]][[biol_no]][["srr_residuals"]][,year,,season,,])
#    # Test Fs and Zs
#    expect_that(unname(f1@.Data), equals(unname(f1in@.Data))) # Total F on Biol
#    expect_that(unname(f1_sub@.Data), equals(unname(f1in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))) # Total F on Biol
#    expect_that(pf11@.Data, equals(f1in@.Data)) # Partial F from FC
#    expect_that(pf11_sub@.Data, equals(f1in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    expect_that(unname(z1@.Data), equals(unname(z1in@.Data))) # Partial F from FC
#    # Test SSB
#    expect_that(unname(ssb1_out@.Data), equals(unname(ssb1_in@.Data)))
#    expect_that(unname(ssb1_out_subset@.Data), equals(unname(ssb1_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data)))
#    # Test Catch, landings and discards in timestep only
#    # Catch, landings, discards in timestep
#    expect_that(catch.n1[,year,1,season,1,]@.Data, equals(catch.n(om_out_f[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]@.Data))
#    expect_that(landings.n1@.Data, equals(landings.n(om_out_f[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]@.Data))
#    expect_that(discards.n1@.Data,equals(discards.n(om_out_f[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]@.Data))
#    # Only CLD in that timestep should have changed
#    expect_that(catch.n(om[["fisheries"]][[fishery_no]][[catch_no]])[,-year]@.Data, equals(catch.n(om_out_f[["fisheries"]][[fishery_no]][[catch_no]])[,-year]@.Data))
#    expect_that(landings.n(om[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data, equals(landings.n(om_out_f[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data))
#    expect_that(discards.n(om[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data, equals(discards.n(om_out_f[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data))
#    # Abundances
#    expect_that(next_n, equals(n(om_out_b[["biols"]][[biol_no]])[,year,,season,,]))
#    # Other abundances unaffected
#    expect_that(n(om[["biols"]][[biol_no]][["biol"]])[,-year,,,,]@.Data,equals(n(om_out_b[["biols"]][[biol_no]])[,-year,,,,]@.Data))
#
#    #------------------
#    # 1 biol -> 2 catch (FC 12 & 21 -> B2)
#    #------------------
#    biol_no <- 2
#    # Total F on the biol
#    f2 <- test_operatingModel_total_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    f2_sub <- test_operatingModel_total_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
#    fbar2 <- test_operatingModel_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
#    expect_that(apply(f2_sub, 2:6, mean), equals(fbar2))
#    # Partial F from FC
#    pf12 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, biol_no)
#    pf21 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 1, biol_no)
#    pf12_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, biol_no, dim_min, dim_max)
#    pf21_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 1, biol_no, dim_min, dim_max)
#    fbar12 <- test_operatingModel_partial_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, biol_no, dim_min, dim_max)
#    fbar21 <- test_operatingModel_partial_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 1,  biol_no, dim_min, dim_max)
#    expect_that(apply(pf12_sub, 2:6, mean), equals(fbar12))
#    expect_that(apply(pf21_sub, 2:6, mean), equals(fbar21))
#    # Total Z of biol
#    z2 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    # SSB of biol
#    ssb2_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    ssb2_out_subset <- test_operatingModel_SSB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
#    # Get metrics from IP OM
#    biomass_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
#    cq_flq_12 <- as(catch.q(om[["fisheries"]][[1]][[2]]), "FLQuant")
#    cq_flq_21 <- as(catch.q(om[["fisheries"]][[2]][[1]]), "FLQuant")
#    qin_12 <- sweep(sweep(biomass_in, c(1,3,4,5), -cq_flq_12[2,], "^"), c(1,3,4,5), cq_flq_12[1], "*")
#    qin_21 <- sweep(sweep(biomass_in, c(1,3,4,5), -cq_flq_21[2,], "^"), c(1,3,4,5), cq_flq_21[1], "*")
#    pfin_12 <- sweep(catch.sel(om[["fisheries"]][[1]][[2]]), 2:6, qin_12 * effort(om[["fisheries"]][[1]]), "*")
#    pfin_21 <- sweep(catch.sel(om[["fisheries"]][[2]][[1]]), 2:6, qin_21 * effort(om[["fisheries"]][[2]]), "*")
#    f2in <- pfin_12 + pfin_21
#    z2in <- f2in + m(om[["biols"]][[biol_no]][["biol"]])
#    ssb2_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]) * fec(om[["biols"]][[biol_no]][["biol"]]) * exp(-((f2in+ m(om[["biols"]][[biol_no]][["biol"]]))* spwn(om[["biols"]][[biol_no]][["biol"]]))))
#    # CLD
#    catch.n12 <- (pfin_12 / z2in) * (1 - exp(-z2in)) * n(om[["biols"]][[biol_no]][["biol"]])
#    landings.n12 <- (catch.n12[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[1]][[2]])[,year,1,season,1,]))
#    discards.n12 <- (catch.n12[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[1]][[2]])[,year,1,season,1,]))
#    catch.n21 <- (pfin_21 / z2in) * (1 - exp(-z2in)) * n(om[["biols"]][[biol_no]][["biol"]])
#    landings.n21 <- (catch.n21[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[2]][[1]])[,year,1,season,1,]))
#    discards.n21 <- (catch.n21[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[2]][[1]])[,year,1,season,1,]))
#    # Biol abundances
#    next_n <- n(om[["biols"]][[biol_no]][["biol"]])[,year,,season,,]
#    next_n[] <- 0
#    next_n[2:dims[1],] <- (n(om[["biols"]][[biol_no]][["biol"]])[,prev_year,,prev_season,,] * exp(-z2in[,prev_year,,prev_season,,]))[1:(dims[1]-1),]
#    next_n[dims[1]] <- next_n[dims[1]] + (n(om[["biols"]][[biol_no]][["biol"]])[dims[1],prev_year,,prev_season,,] * exp(-z2in[dims[1],prev_year,,prev_season,,]))
#    # Rec - it's Ricker for Biol 2
#    ssb_timestep <- timestep - om[["biols"]][[biol_no]][["srr_timelag"]]
#    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
#    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
#    ssb_rec <- ssb2_in[,ssb_year,,ssb_season,,]
#    rec_in <- om[["biols"]][[biol_no]][["srr_params"]]['a',] * ssb_rec * exp(-om[["biols"]][[biol_no]][["srr_params"]]['b',] * ssb_rec)
#    # Multiplicative residuals
#    next_n[1,] <- rec_in * exp(om[["biols"]][[biol_no]][["srr_residuals"]][,year,,season,,])
#    # Test Fs and Zs
#    expect_that(unname(f2@.Data), equals(unname(f2in@.Data))) # Total F on Biol
#    expect_that(unname(f2_sub@.Data), equals(unname(f2in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))) # Total F on Biol
#    expect_that(pf12@.Data, equals(pfin_12@.Data)) # Partial F from FC
#    expect_that(pf21@.Data, equals(pfin_21@.Data)) # Partial F from FC
#    expect_that(pf12_sub@.Data, equals(pfin_12[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    expect_that(pf21_sub@.Data, equals(pfin_21[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    expect_that(unname(z2@.Data), equals(unname(z2in@.Data))) # Partial F from FC
#    # Test SSB
#    expect_that(unname(ssb2_out@.Data), equals(unname(ssb2_in@.Data)))
#    expect_that(unname(ssb2_out_subset@.Data), equals(unname(ssb2_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data)))
#    # Test Catch, landings and discards in timestep only
#    # Catch, landings, discards in timestep
#    expect_that(catch.n12[,year,1,season,1,]@.Data, equals(catch.n(om_out_f[["fisheries"]][[1]][[2]])[,year,1,season,1,]@.Data))
#    expect_that(landings.n12@.Data, equals(landings.n(om_out_f[["fisheries"]][[1]][[2]])[,year,1,season,1,]@.Data))
#    expect_that(discards.n12@.Data,equals(discards.n(om_out_f[["fisheries"]][[1]][[2]])[,year,1,season,1,]@.Data))
#    expect_that(catch.n21[,year,1,season,1,]@.Data, equals(catch.n(om_out_f[["fisheries"]][[2]][[1]])[,year,1,season,1,]@.Data))
#    expect_that(landings.n21@.Data, equals(landings.n(om_out_f[["fisheries"]][[2]][[1]])[,year,1,season,1,]@.Data))
#    expect_that(discards.n21@.Data,equals(discards.n(om_out_f[["fisheries"]][[2]][[1]])[,year,1,season,1,]@.Data))
#    # Only CLD in that timestep should have changed
#    expect_that(catch.n(om[["fisheries"]][[1]][[2]])[,-year]@.Data, equals(catch.n(om_out_f[["fisheries"]][[1]][[2]])[,-year]@.Data))
#    expect_that(landings.n(om[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data, equals(landings.n(om_out_f[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data))
#    expect_that(discards.n(om[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data, equals(discards.n(om_out_f[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data))
#    expect_that(catch.n(om[["fisheries"]][[2]][[1]])[,-year]@.Data, equals(catch.n(om_out_f[["fisheries"]][[2]][[1]])[,-year]@.Data))
#    expect_that(landings.n(om[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data, equals(landings.n(om_out_f[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data))
#    expect_that(discards.n(om[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data, equals(discards.n(om_out_f[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data))
#    # Abundances
#    expect_that(next_n, equals(n(om_out_b[["biols"]][[biol_no]])[,year,,season,,]))
#    # Other abundances unaffected
#    expect_that(n(om[["biols"]][[biol_no]][["biol"]])[,-year,,,,]@.Data,equals(n(om_out_b[["biols"]][[biol_no]])[,-year,,,,]@.Data))
#
#    #------------------
#    # 2 biol -> 1 catch (FC 22 -> B3 + B4)
#    #------------------
#    # Total F on the biol
#    f3 <- test_operatingModel_total_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3)
#    f3_sub <- test_operatingModel_total_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3, dim_min, dim_max)
#    f4 <- test_operatingModel_total_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4)
#    f4_sub <- test_operatingModel_total_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4, dim_min, dim_max)
#    fbar3_sub <- test_operatingModel_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3, dim_min, dim_max)
#    fbar4_sub <- test_operatingModel_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4, dim_min, dim_max)
#    expect_that(apply(f3_sub, 2:6, mean), equals(fbar3_sub))
#    expect_that(apply(f3_sub, 2:6, mean), equals(fbar3_sub))
#    # Partial F from FC
#    pf223 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 3)
#    pf224 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 4)
#    pf223_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 3, dim_min, dim_max)
#    pf224_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 4, dim_min, dim_max)
#    fbar223_sub <- test_operatingModel_partial_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 3, dim_min, dim_max)
#    fbar224_sub <- test_operatingModel_partial_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 4, dim_min, dim_max)
#    expect_that(apply(pf223_sub, 2:6, mean), equals(fbar223_sub))
#    expect_that(apply(pf224_sub, 2:6, mean), equals(fbar224_sub))
#    # Total Z of biol
#    z3 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3)
#    z4 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4)
#    # SSB of biol
#    ssb3_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3)
#    ssb3_out_subset <- test_operatingModel_SSB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3, dim_min[-1], dim_max[-1])
#    ssb4_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4)
#    ssb4_out_subset <- test_operatingModel_SSB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4, dim_min[-1], dim_max[-1])
#
#    # Get metrics from IP OM
#    biomass_in3 <- quantSums(n(om[["biols"]][[3]][["biol"]]) * wt(om[["biols"]][[3]][["biol"]]))
#    biomass_in4 <- quantSums(n(om[["biols"]][[4]][["biol"]]) * wt(om[["biols"]][[4]][["biol"]]))
#    cq_flq_22 <- as(catch.q(om[["fisheries"]][[2]][[2]]), "FLQuant")
#    qin_223 <- sweep(sweep(biomass_in3, c(1,3,4,5), -cq_flq_22[2,], "^"), c(1,3,4,5), cq_flq_22[1], "*")
#    qin_224 <- sweep(sweep(biomass_in4, c(1,3,4,5), -cq_flq_22[2,], "^"), c(1,3,4,5), cq_flq_22[1], "*")
#    fin_223 <- sweep(catch.sel(om[["fisheries"]][[2]][[2]]), 2:6, qin_223 * effort(om[["fisheries"]][[2]]), "*")
#    fin_224 <- sweep(catch.sel(om[["fisheries"]][[2]][[2]]), 2:6, qin_224 * effort(om[["fisheries"]][[2]]), "*")
#    z3in <- fin_223 + m(om[["biols"]][[3]][["biol"]])
#    z4in <- fin_224 + m(om[["biols"]][[4]][["biol"]])
#    ssb3_in <- quantSums(n(om[["biols"]][[3]][["biol"]]) * wt(om[["biols"]][[3]][["biol"]]) * fec(om[["biols"]][[3]][["biol"]]) * exp(-((fin_223 + m(om[["biols"]][[3]][["biol"]]))* spwn(om[["biols"]][[3]][["biol"]]))))
#    ssb4_in <- quantSums(n(om[["biols"]][[4]][["biol"]]) * wt(om[["biols"]][[4]][["biol"]]) * fec(om[["biols"]][[4]][["biol"]]) * exp(-((fin_224 + m(om[["biols"]][[4]][["biol"]]))* spwn(om[["biols"]][[4]][["biol"]]))))
#    # CLD
#    catch.n223 <- (fin_223 / z3in) * (1 - exp(-z3in)) * n(om[["biols"]][[3]][["biol"]])
#    catch.n224 <- (fin_224 / z4in) * (1 - exp(-z4in)) * n(om[["biols"]][[4]][["biol"]])
#    landings.n223 <- (catch.n223[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
#    landings.n224 <- (catch.n224[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
#    discards.n223 <- (catch.n223[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
#    discards.n224 <- (catch.n224[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
#    catch.n22 <- catch.n223 + catch.n224
#    landings.n22 <- landings.n223 + landings.n224
#    discards.n22 <- discards.n223 + discards.n224
#
#    # Biol abundances
#    next_n3 <- n(om[["biols"]][[3]][["biol"]])[,year,,season,,]
#    next_n3[] <- 0
#    next_n3[2:dims[1],] <- (n(om[["biols"]][[3]][["biol"]])[,prev_year,,prev_season,,] * exp(-z3in[,prev_year,,prev_season,,]))[1:(dims[1]-1),]
#    next_n3[dims[1]] <- next_n3[dims[1]] + (n(om[["biols"]][[3]][["biol"]])[dims[1],prev_year,,prev_season,,] * exp(-z3in[dims[1],prev_year,,prev_season,,]))
#    # Rec - it's Bevholt for Biol 3
#    ssb_timestep <- timestep - om[["biols"]][[3]][["srr_timelag"]]
#    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
#    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
#    ssb_rec <- ssb3_in[,ssb_year,,ssb_season,,]
#    rec_in <- om[["biols"]][[3]][["srr_params"]]['a',] * ssb_rec / (om[["biols"]][[3]][["srr_params"]]['b',] + ssb_rec)
#    # Multiplicative residuals
#    next_n3[1,] <- rec_in * exp(om[["biols"]][[3]][["srr_residuals"]][,year,,season,,])
#    next_n4 <- n(om[["biols"]][[4]][["biol"]])[,year,,season,,]
#    next_n4[] <- 0
#    next_n4[2:dims[1],] <- (n(om[["biols"]][[4]][["biol"]])[,prev_year,,prev_season,,] * exp(-z4in[,prev_year,,prev_season,,]))[1:(dims[1]-1),]
#    next_n4[dims[1]] <- next_n4[dims[1]] + (n(om[["biols"]][[4]][["biol"]])[dims[1],prev_year,,prev_season,,] * exp(-z4in[dims[1],prev_year,,prev_season,,]))
#    # Rec - it's Ricker for Biol 4
#    ssb_timestep <- timestep - om[["biols"]][[4]][["srr_timelag"]]
#    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
#    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
#    ssb_rec <- ssb4_in[,ssb_year,,ssb_season,,]
#    rec_in <- om[["biols"]][[4]][["srr_params"]]['a',] * ssb_rec * exp(-om[["biols"]][[4]][["srr_params"]]['b',] * ssb_rec)
#    # Multiplicative residuals
#    next_n4[1,] <- rec_in * exp(om[["biols"]][[4]][["srr_residuals"]][,year,,season,,])
#    # Test Fs and Zs
#    expect_that(unname(f3@.Data), equals(unname(fin_223@.Data))) # Total F on Biol
#    expect_that(unname(f4@.Data), equals(unname(fin_224@.Data))) # Total F on Biol
#    expect_that(unname(f3_sub@.Data), equals(unname(fin_223[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))) # Total F on Biol
#    expect_that(unname(f4_sub@.Data), equals(unname(fin_224[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))) # Total F on Biol
#    expect_that(pf223@.Data, equals(fin_223@.Data)) # Total F on Biol
#    expect_that(pf224@.Data, equals(fin_224@.Data)) # Total F on Biol
#    expect_that(pf223_sub@.Data, equals(fin_223[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    expect_that(pf224_sub@.Data, equals(fin_224[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    expect_that(unname(z3@.Data), equals(unname(z3in@.Data))) # Partial F from FC
#    expect_that(unname(z4@.Data), equals(unname(z4in@.Data))) # Partial F from FC
#    # Test SSB
#    expect_that(unname(ssb3_out@.Data), equals(unname(ssb3_in@.Data)))
#    expect_that(unname(ssb3_out_subset@.Data), equals(unname(ssb3_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data)))
#    expect_that(unname(ssb4_out@.Data), equals(unname(ssb4_in@.Data)))
#    expect_that(unname(ssb4_out_subset@.Data), equals(unname(ssb4_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data)))
#    # Test Catch, landings and discards in timestep only
#    # Catch, landings, discards in timestep
#    expect_that(catch.n22[,year,1,season,1,]@.Data, equals(catch.n(om_out_f[["fisheries"]][[2]][[2]])[,year,1,season,1,]@.Data))
#    expect_that(landings.n22@.Data, equals(landings.n(om_out_f[["fisheries"]][[2]][[2]])[,year,1,season,1,]@.Data))
#    expect_that(discards.n22@.Data,equals(discards.n(om_out_f[["fisheries"]][[2]][[2]])[,year,1,season,1,]@.Data))
#    # Only CLD in that timestep should have changed
#    expect_that(catch.n(om[["fisheries"]][[2]][[2]])[,-year]@.Data, equals(catch.n(om_out_f[["fisheries"]][[2]][[2]])[,-year]@.Data))
#    expect_that(landings.n(om[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data, equals(landings.n(om_out_f[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data))
#    expect_that(discards.n(om[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data, equals(discards.n(om_out_f[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data))
#    # Abundances
#    expect_that(next_n3, equals(n(om_out_b[["biols"]][[3]])[,year,,season,,]))
#    expect_that(next_n4, equals(n(om_out_b[["biols"]][[4]])[,year,,season,,]))
#    # Other abundances unaffected
#    expect_that(n(om[["biols"]][[3]][["biol"]])[,-year,,,,]@.Data,equals(n(om_out_b[["biols"]][[3]])[,-year,,,,]@.Data))
#    expect_that(n(om[["biols"]][[4]][["biol"]])[,-year,,,,]@.Data,equals(n(om_out_b[["biols"]][[4]])[,-year,,,,]@.Data))
#
#    #------------------
#    # 1 biol -> 0 catch (B5)
#    #------------------
#    # Total F on the biol
#    f5 <- test_operatingModel_total_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5)
#    f5_sub <- test_operatingModel_total_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5, dim_min, dim_max)
#    # Total Z of biol
#    z5 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5)
#    # SSB of biol
#    ssb5_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5)
#    ssb5_out_subset <- test_operatingModel_SSB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5, dim_min[-1], dim_max[-1])
#    # Get metrics from IP OM
#    biomass_in5 <- quantSums(n(om[["biols"]][[5]][["biol"]]) * wt(om[["biols"]][[5]][["biol"]]))
#    z5in <- m(om[["biols"]][[5]][["biol"]])
#    ssb5_in <- quantSums(n(om[["biols"]][[5]][["biol"]]) * wt(om[["biols"]][[5]][["biol"]]) * fec(om[["biols"]][[5]][["biol"]]) * exp(-((m(om[["biols"]][[5]][["biol"]]))* spwn(om[["biols"]][[5]][["biol"]]))))
#
#    # Biol abundances
#    next_n5 <- n(om[["biols"]][[5]][["biol"]])[,year,,season,,]
#    next_n5[] <- 0
#    next_n5[2:dims[1],] <- (n(om[["biols"]][[5]][["biol"]])[,prev_year,,prev_season,,] * exp(-z5in[,prev_year,,prev_season,,]))[1:(dims[1]-1),]
#    next_n5[dims[1]] <- next_n5[dims[1]] + (n(om[["biols"]][[5]][["biol"]])[dims[1],prev_year,,prev_season,,] * exp(-z5in[dims[1],prev_year,,prev_season,,]))
#    # Rec - it's Ricker for Biol 5
#    ssb_timestep <- timestep - om[["biols"]][[5]][["srr_timelag"]]
#    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
#    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
#    ssb_rec <- ssb5_in[,ssb_year,,ssb_season,,]
#    rec_in <- om[["biols"]][[5]][["srr_params"]]['a',] * ssb_rec * exp(-om[["biols"]][[5]][["srr_params"]]['b',] * ssb_rec)
#    # Multiplicative residuals
#    next_n5[1,] <- rec_in * exp(om[["biols"]][[5]][["srr_residuals"]][,year,,season,,])
#    # Test Fs and Zs
#    expect_that(all(unname(f5@.Data)==0), equals(TRUE)) # Total F on Biol
#    expect_that(all(unname(f5_sub@.Data)==0), equals(TRUE)) # Total F on Biol
#    expect_that(unname(z5@.Data), equals(unname(z5in@.Data))) # Partial F from FC
#    # Test SSB
#    expect_that(unname(ssb5_out@.Data), equals(unname(ssb5_in@.Data)))
#    expect_that(unname(ssb5_out_subset@.Data), equals(unname(ssb5_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data)))
#    # Abundances
#    expect_that(next_n5, equals(n(om_out_b[["biols"]][[5]])[,year,,season,,]))
#    # Other abundances unaffected
#    expect_that(n(om[["biols"]][[5]][["biol"]])[,-year,,,,]@.Data,equals(n(om_out_b[["biols"]][[5]])[,-year,,,,]@.Data))
})

## F, partial F, Z, SSB, C, L, D
## Using the annual test operating model
## Epic!
#test_that("operatingModel annual project",{
#    om <- make_test_operatingModel1(20)
#
#    # Random timestep
#    dims <- dim(n(om[["biols"]][[1]][["biol"]]))
#    year <- round(runif(1,min=1,max=dims[2]))
#    season <- round(runif(1,min=1,max=dims[4]))
#    timestep <- (year-1)*dims[4] + season
#    next_year <-  floor((timestep) / dims[4]) + 1; 
#    next_season <- timestep %% dims[4] + 1;
#    om_out <- test_operatingModel_project_timestep(om[["fisheries"]], om[["biols"]], om[["fwc"]], timestep)
#    dim_max <- dims
#    dim_min <- round(runif(6, min=1, max=dim_max))
#
#    #------------------
#    # 1 biol -> 1 catch
#    #------------------
#    biol_no <- 1
#    fishery_no <- 1
#    catch_no <- 1
#    # Total F on the biol
#    f1 <- test_operatingModel_total_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    f1_sub <- test_operatingModel_total_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
#    fbar1 <- test_operatingModel_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
#    expect_that(apply(f1_sub, 2:6, mean), equals(fbar1))
#    # Partial F from FC
#    pf11 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    pf11_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    fbar11 <- test_operatingModel_partial_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(apply(pf11_sub, 2:6, mean), equals(fbar11))
#    # Total Z of biol
#    z1 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    # SSB of biol
#    ssb1_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    ssb1_out_subset <- test_operatingModel_SSB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
#
#    # Get metrics from IP OM
#    biomass_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
#    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
#    qin <- sweep(sweep(biomass_in, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
#    f1in <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
#    z1in <- f1in + m(om[["biols"]][[biol_no]][["biol"]])
#    ssb1_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]) * fec(om[["biols"]][[biol_no]][["biol"]]) * exp(-((f1in+ m(om[["biols"]][[biol_no]][["biol"]]))* spwn(om[["biols"]][[biol_no]][["biol"]]))))
#    catch.n1 <- (f1in / z1in) * (1 - exp(-z1in)) * n(om[["biols"]][[biol_no]][["biol"]])
#    landings.n1 <- (catch.n1[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]))
#    discards.n1 <- (catch.n1[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]))
#    # Biol abundances
#    next_n <- n(om[["biols"]][[biol_no]][["biol"]])[,next_year,,next_season,,]
#    next_n[] <- 0
#    next_n[2:dims[1],] <- (n(om[["biols"]][[biol_no]][["biol"]])[,year,,season,,] * exp(-z1in[,year,,season,,]))[1:(dims[1]-1),]
#    next_n[dims[1]] <- next_n[dims[1]] + (n(om[["biols"]][[biol_no]][["biol"]])[dims[1],year,,season,,] * exp(-z1in[dims[1],year,,season,,]))
#    # Rec - it's BH for Biol 1
#    ssb_timestep <- timestep - om[["biols"]][[biol_no]][["srr_timelag"]] + 1
#    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
#    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
#    ssb_rec <- ssb1_in[,ssb_year,,ssb_season,,]
#    rec_in <- om[["biols"]][[biol_no]][["srr_params"]]['a',] * ssb_rec / (om[["biols"]][[biol_no]][["srr_params"]]['b',] + ssb_rec)
#    # Multiplicative residuals
#    next_n[1,] <- rec_in * exp(om[["biols"]][[biol_no]][["srr_residuals"]][,next_year,,next_season,,])
#
#    # Test Fs and Zs
#    expect_that(unname(f1@.Data), equals(unname(f1in@.Data))) # Total F on Biol
#    expect_that(unname(f1_sub@.Data), equals(unname(f1in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))) # Total F on Biol
#    expect_that(pf11@.Data, equals(f1in@.Data)) # Partial F from FC
#    expect_that(pf11_sub@.Data, equals(f1in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    expect_that(unname(z1@.Data), equals(unname(z1in@.Data))) # Partial F from FC
#    # Test SSB
#    expect_that(unname(ssb1_out@.Data), equals(unname(ssb1_in@.Data)))
#    expect_that(unname(ssb1_out_subset@.Data), equals(unname(ssb1_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data)))
#
#    # Test Catch, landings and discards in timestep only
#    # Catch, landings, discards in timestep
#    expect_that(catch.n1[,year,1,season,1,]@.Data, equals(catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]@.Data))
#    expect_that(landings.n1@.Data, equals(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]@.Data))
#    expect_that(discards.n1@.Data,equals(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,1,season,1,]@.Data))
#    # Only CLD in that timestep should have changed
#    expect_that(catch.n(om[["fisheries"]][[fishery_no]][[catch_no]])[,-year]@.Data, equals(catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,-year]@.Data))
#    expect_that(landings.n(om[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data, equals(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data))
#    expect_that(discards.n(om[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data, equals(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,-year,,,,]@.Data))
#    # Abundances
#    expect_that(next_n, equals(n(om_out[["biols"]][[biol_no]])[,next_year,,next_season,,]))
#    # Other abundances unaffected
#    expect_that(n(om[["biols"]][[biol_no]][["biol"]])[,-next_year,,,,]@.Data,equals(n(om_out[["biols"]][[biol_no]])[,-next_year,,,,]@.Data))
#
#    #------------------
#    # 1 biol -> 2 catch (FC 12 & 21 -> B2)
#    #------------------
#    biol_no <- 2
#    # Total F on the biol
#    f2 <- test_operatingModel_total_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    f2_sub <- test_operatingModel_total_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
#    fbar2 <- test_operatingModel_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
#    expect_that(apply(f2_sub, 2:6, mean), equals(fbar2))
#    # Partial F from FC
#    pf12 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, biol_no)
#    pf21 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 1, biol_no)
#    pf12_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, biol_no, dim_min, dim_max)
#    pf21_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 1, biol_no, dim_min, dim_max)
#    fbar12 <- test_operatingModel_partial_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, biol_no, dim_min, dim_max)
#    fbar21 <- test_operatingModel_partial_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 1,  biol_no, dim_min, dim_max)
#    expect_that(apply(pf12_sub, 2:6, mean), equals(fbar12))
#    expect_that(apply(pf21_sub, 2:6, mean), equals(fbar21))
#    # Total Z of biol
#    z2 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    # SSB of biol
#    ssb2_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no)
#    ssb2_out_subset <- test_operatingModel_SSB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
#    # Get metrics from IP OM
#    biomass_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
#    cq_flq_12 <- as(catch.q(om[["fisheries"]][[1]][[2]]), "FLQuant")
#    cq_flq_21 <- as(catch.q(om[["fisheries"]][[2]][[1]]), "FLQuant")
#    qin_12 <- sweep(sweep(biomass_in, c(1,3,4,5), -cq_flq_12[2,], "^"), c(1,3,4,5), cq_flq_12[1], "*")
#    qin_21 <- sweep(sweep(biomass_in, c(1,3,4,5), -cq_flq_21[2,], "^"), c(1,3,4,5), cq_flq_21[1], "*")
#    pfin_12 <- sweep(catch.sel(om[["fisheries"]][[1]][[2]]), 2:6, qin_12 * effort(om[["fisheries"]][[1]]), "*")
#    pfin_21 <- sweep(catch.sel(om[["fisheries"]][[2]][[1]]), 2:6, qin_21 * effort(om[["fisheries"]][[2]]), "*")
#    f2in <- pfin_12 + pfin_21
#    z2in <- f2in + m(om[["biols"]][[biol_no]][["biol"]])
#    ssb2_in <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]) * fec(om[["biols"]][[biol_no]][["biol"]]) * exp(-((f2in+ m(om[["biols"]][[biol_no]][["biol"]]))* spwn(om[["biols"]][[biol_no]][["biol"]]))))
#    # CLD
#    catch.n12 <- (pfin_12 / z2in) * (1 - exp(-z2in)) * n(om[["biols"]][[biol_no]][["biol"]])
#    landings.n12 <- (catch.n12[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[1]][[2]])[,year,1,season,1,]))
#    discards.n12 <- (catch.n12[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[1]][[2]])[,year,1,season,1,]))
#    catch.n21 <- (pfin_21 / z2in) * (1 - exp(-z2in)) * n(om[["biols"]][[biol_no]][["biol"]])
#    landings.n21 <- (catch.n21[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[2]][[1]])[,year,1,season,1,]))
#    discards.n21 <- (catch.n21[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[2]][[1]])[,year,1,season,1,]))
#    # Biol abundances
#    next_n <- n(om[["biols"]][[biol_no]][["biol"]])[,next_year,,next_season,,]
#    next_n[] <- 0
#    next_n[2:dims[1],] <- (n(om[["biols"]][[biol_no]][["biol"]])[,year,,season,,] * exp(-z2in[,year,,season,,]))[1:(dims[1]-1),]
#    next_n[dims[1]] <- next_n[dims[1]] + (n(om[["biols"]][[biol_no]][["biol"]])[dims[1],year,,season,,] * exp(-z2in[dims[1],year,,season,,]))
#    # Rec - it's Ricker for Biol 2
#    ssb_timestep <- timestep - om[["biols"]][[biol_no]][["srr_timelag"]] + 1
#    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
#    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
#    ssb_rec <- ssb2_in[,ssb_year,,ssb_season,,]
#    rec_in <- om[["biols"]][[biol_no]][["srr_params"]]['a',] * ssb_rec * exp(-om[["biols"]][[biol_no]][["srr_params"]]['b',] * ssb_rec)
#    # Multiplicative residuals
#    next_n[1,] <- rec_in * exp(om[["biols"]][[biol_no]][["srr_residuals"]][,next_year,,next_season,,])
#    # Test Fs and Zs
#    expect_that(unname(f2@.Data), equals(unname(f2in@.Data))) # Total F on Biol
#    expect_that(unname(f2_sub@.Data), equals(unname(f2in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))) # Total F on Biol
#    expect_that(pf12@.Data, equals(pfin_12@.Data)) # Partial F from FC
#    expect_that(pf21@.Data, equals(pfin_21@.Data)) # Partial F from FC
#    expect_that(pf12_sub@.Data, equals(pfin_12[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    expect_that(pf21_sub@.Data, equals(pfin_21[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    expect_that(unname(z2@.Data), equals(unname(z2in@.Data))) # Partial F from FC
#    # Test SSB
#    expect_that(unname(ssb2_out@.Data), equals(unname(ssb2_in@.Data)))
#    expect_that(unname(ssb2_out_subset@.Data), equals(unname(ssb2_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data)))
#    # Test Catch, landings and discards in timestep only
#    # Catch, landings, discards in timestep
#    expect_that(catch.n12[,year,1,season,1,]@.Data, equals(catch.n(om_out[["fisheries"]][[1]][[2]])[,year,1,season,1,]@.Data))
#    expect_that(landings.n12@.Data, equals(landings.n(om_out[["fisheries"]][[1]][[2]])[,year,1,season,1,]@.Data))
#    expect_that(discards.n12@.Data,equals(discards.n(om_out[["fisheries"]][[1]][[2]])[,year,1,season,1,]@.Data))
#    expect_that(catch.n21[,year,1,season,1,]@.Data, equals(catch.n(om_out[["fisheries"]][[2]][[1]])[,year,1,season,1,]@.Data))
#    expect_that(landings.n21@.Data, equals(landings.n(om_out[["fisheries"]][[2]][[1]])[,year,1,season,1,]@.Data))
#    expect_that(discards.n21@.Data,equals(discards.n(om_out[["fisheries"]][[2]][[1]])[,year,1,season,1,]@.Data))
#    # Only CLD in that timestep should have changed
#    expect_that(catch.n(om[["fisheries"]][[1]][[2]])[,-year]@.Data, equals(catch.n(om_out[["fisheries"]][[1]][[2]])[,-year]@.Data))
#    expect_that(landings.n(om[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data, equals(landings.n(om_out[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data))
#    expect_that(discards.n(om[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data, equals(discards.n(om_out[["fisheries"]][[1]][[2]])[,-year,,,,]@.Data))
#    expect_that(catch.n(om[["fisheries"]][[2]][[1]])[,-year]@.Data, equals(catch.n(om_out[["fisheries"]][[2]][[1]])[,-year]@.Data))
#    expect_that(landings.n(om[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data, equals(landings.n(om_out[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data))
#    expect_that(discards.n(om[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data, equals(discards.n(om_out[["fisheries"]][[2]][[1]])[,-year,,,,]@.Data))
#    # Abundances
#    expect_that(next_n, equals(n(om_out[["biols"]][[biol_no]])[,next_year,,next_season,,]))
#    # Other abundances unaffected
#    expect_that(n(om[["biols"]][[biol_no]][["biol"]])[,-next_year,,,,]@.Data,equals(n(om_out[["biols"]][[biol_no]])[,-next_year,,,,]@.Data))
#
#    #------------------
#    # 2 biol -> 1 catch (FC 22 -> B3 + B4)
#    #------------------
#    # Total F on the biol
#    f3 <- test_operatingModel_total_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3)
#    f3_sub <- test_operatingModel_total_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3, dim_min, dim_max)
#    f4 <- test_operatingModel_total_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4)
#    f4_sub <- test_operatingModel_total_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4, dim_min, dim_max)
#    fbar3_sub <- test_operatingModel_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3, dim_min, dim_max)
#    fbar4_sub <- test_operatingModel_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4, dim_min, dim_max)
#    expect_that(apply(f3_sub, 2:6, mean), equals(fbar3_sub))
#    expect_that(apply(f3_sub, 2:6, mean), equals(fbar3_sub))
#    # Partial F from FC
#    pf223 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 3)
#    pf224 <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 4)
#    pf223_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 3, dim_min, dim_max)
#    pf224_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 4, dim_min, dim_max)
#    fbar223_sub <- test_operatingModel_partial_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 3, dim_min, dim_max)
#    fbar224_sub <- test_operatingModel_partial_fbar_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, 4, dim_min, dim_max)
#    expect_that(apply(pf223_sub, 2:6, mean), equals(fbar223))
#    expect_that(apply(pf224_sub, 2:6, mean), equals(fbar224))
#    # Total Z of biol
#    z3 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3)
#    z4 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4)
#    # SSB of biol
#    ssb3_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3)
#    ssb3_out_subset <- test_operatingModel_SSB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3, dim_min[-1], dim_max[-1])
#    ssb4_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4)
#    ssb4_out_subset <- test_operatingModel_SSB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4, dim_min[-1], dim_max[-1])
#
#    # Get metrics from IP OM
#    biomass_in3 <- quantSums(n(om[["biols"]][[3]][["biol"]]) * wt(om[["biols"]][[3]][["biol"]]))
#    biomass_in4 <- quantSums(n(om[["biols"]][[4]][["biol"]]) * wt(om[["biols"]][[4]][["biol"]]))
#    cq_flq_22 <- as(catch.q(om[["fisheries"]][[2]][[2]]), "FLQuant")
#    qin_223 <- sweep(sweep(biomass_in3, c(1,3,4,5), -cq_flq_22[2,], "^"), c(1,3,4,5), cq_flq_22[1], "*")
#    qin_224 <- sweep(sweep(biomass_in4, c(1,3,4,5), -cq_flq_22[2,], "^"), c(1,3,4,5), cq_flq_22[1], "*")
#    fin_223 <- sweep(catch.sel(om[["fisheries"]][[2]][[2]]), 2:6, qin_223 * effort(om[["fisheries"]][[2]]), "*")
#    fin_224 <- sweep(catch.sel(om[["fisheries"]][[2]][[2]]), 2:6, qin_224 * effort(om[["fisheries"]][[2]]), "*")
#    z3in <- fin_223 + m(om[["biols"]][[3]][["biol"]])
#    z4in <- fin_224 + m(om[["biols"]][[4]][["biol"]])
#    ssb3_in <- quantSums(n(om[["biols"]][[3]][["biol"]]) * wt(om[["biols"]][[3]][["biol"]]) * fec(om[["biols"]][[3]][["biol"]]) * exp(-((fin_223 + m(om[["biols"]][[3]][["biol"]]))* spwn(om[["biols"]][[3]][["biol"]]))))
#    ssb4_in <- quantSums(n(om[["biols"]][[4]][["biol"]]) * wt(om[["biols"]][[4]][["biol"]]) * fec(om[["biols"]][[4]][["biol"]]) * exp(-((fin_224 + m(om[["biols"]][[4]][["biol"]]))* spwn(om[["biols"]][[4]][["biol"]]))))
#    # CLD
#    catch.n223 <- (fin_223 / z3in) * (1 - exp(-z3in)) * n(om[["biols"]][[3]][["biol"]])
#    catch.n224 <- (fin_224 / z4in) * (1 - exp(-z4in)) * n(om[["biols"]][[4]][["biol"]])
#    landings.n223 <- (catch.n223[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
#    landings.n224 <- (catch.n224[,year,1,season,1,] * (1 - discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
#    discards.n223 <- (catch.n223[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
#    discards.n224 <- (catch.n224[,year,1,season,1,] * (discards.ratio(om[["fisheries"]][[2]][[2]])[,year,1,season,1,]))
#    catch.n22 <- catch.n223 + catch.n224
#    landings.n22 <- landings.n223 + landings.n224
#    discards.n22 <- discards.n223 + discards.n224
#
#    # Biol abundances
#    next_n3 <- n(om[["biols"]][[3]][["biol"]])[,next_year,,next_season,,]
#    next_n3[] <- 0
#    next_n3[2:dims[1],] <- (n(om[["biols"]][[3]][["biol"]])[,year,,season,,] * exp(-z3in[,year,,season,,]))[1:(dims[1]-1),]
#    next_n3[dims[1]] <- next_n3[dims[1]] + (n(om[["biols"]][[3]][["biol"]])[dims[1],year,,season,,] * exp(-z3in[dims[1],year,,season,,]))
#    # Rec - it's Bevholt for Biol 3
#    ssb_timestep <- timestep - om[["biols"]][[3]][["srr_timelag"]] + 1
#    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
#    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
#    ssb_rec <- ssb3_in[,ssb_year,,ssb_season,,]
#    rec_in <- om[["biols"]][[3]][["srr_params"]]['a',] * ssb_rec / (om[["biols"]][[3]][["srr_params"]]['b',] + ssb_rec)
#    # Multiplicative residuals
#    next_n3[1,] <- rec_in * exp(om[["biols"]][[3]][["srr_residuals"]][,next_year,,next_season,,])
#
#    next_n4 <- n(om[["biols"]][[4]][["biol"]])[,next_year,,next_season,,]
#    next_n4[] <- 0
#    next_n4[2:dims[1],] <- (n(om[["biols"]][[4]][["biol"]])[,year,,season,,] * exp(-z4in[,year,,season,,]))[1:(dims[1]-1),]
#    next_n4[dims[1]] <- next_n4[dims[1]] + (n(om[["biols"]][[4]][["biol"]])[dims[1],year,,season,,] * exp(-z4in[dims[1],year,,season,,]))
#    # Rec - it's Ricker for Biol 4
#    ssb_timestep <- timestep - om[["biols"]][[4]][["srr_timelag"]] + 1
#    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
#    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
#    ssb_rec <- ssb4_in[,ssb_year,,ssb_season,,]
#    rec_in <- om[["biols"]][[4]][["srr_params"]]['a',] * ssb_rec * exp(-om[["biols"]][[4]][["srr_params"]]['b',] * ssb_rec)
#    # Multiplicative residuals
#    next_n4[1,] <- rec_in * exp(om[["biols"]][[4]][["srr_residuals"]][,next_year,,next_season,,])
#    # Test Fs and Zs
#    expect_that(unname(f3@.Data), equals(unname(fin_223@.Data))) # Total F on Biol
#    expect_that(unname(f4@.Data), equals(unname(fin_224@.Data))) # Total F on Biol
#    expect_that(unname(f3_sub@.Data), equals(unname(fin_223[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))) # Total F on Biol
#    expect_that(unname(f4_sub@.Data), equals(unname(fin_224[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))) # Total F on Biol
#    expect_that(pf223@.Data, equals(fin_223@.Data)) # Total F on Biol
#    expect_that(pf224@.Data, equals(fin_224@.Data)) # Total F on Biol
#    expect_that(pf223_sub@.Data, equals(fin_223[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    expect_that(pf224_sub@.Data, equals(fin_224[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    expect_that(unname(z3@.Data), equals(unname(z3in@.Data))) # Partial F from FC
#    expect_that(unname(z4@.Data), equals(unname(z4in@.Data))) # Partial F from FC
#    # Test SSB
#    expect_that(unname(ssb3_out@.Data), equals(unname(ssb3_in@.Data)))
#    expect_that(unname(ssb3_out_subset@.Data), equals(unname(ssb3_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data)))
#    expect_that(unname(ssb4_out@.Data), equals(unname(ssb4_in@.Data)))
#    expect_that(unname(ssb4_out_subset@.Data), equals(unname(ssb4_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data)))
#    # Test Catch, landings and discards in timestep only
#    # Catch, landings, discards in timestep
#    expect_that(catch.n22[,year,1,season,1,]@.Data, equals(catch.n(om_out[["fisheries"]][[2]][[2]])[,year,1,season,1,]@.Data))
#    expect_that(landings.n22@.Data, equals(landings.n(om_out[["fisheries"]][[2]][[2]])[,year,1,season,1,]@.Data))
#    expect_that(discards.n22@.Data,equals(discards.n(om_out[["fisheries"]][[2]][[2]])[,year,1,season,1,]@.Data))
#    # Only CLD in that timestep should have changed
#    expect_that(catch.n(om[["fisheries"]][[2]][[2]])[,-year]@.Data, equals(catch.n(om_out[["fisheries"]][[2]][[2]])[,-year]@.Data))
#    expect_that(landings.n(om[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data, equals(landings.n(om_out[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data))
#    expect_that(discards.n(om[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data, equals(discards.n(om_out[["fisheries"]][[2]][[2]])[,-year,,,,]@.Data))
#    # Abundances
#    expect_that(next_n3, equals(n(om_out[["biols"]][[3]])[,next_year,,next_season,,]))
#    expect_that(next_n4, equals(n(om_out[["biols"]][[4]])[,next_year,,next_season,,]))
#    # Other abundances unaffected
#    expect_that(n(om[["biols"]][[3]][["biol"]])[,-next_year,,,,]@.Data,equals(n(om_out[["biols"]][[3]])[,-next_year,,,,]@.Data))
#    expect_that(n(om[["biols"]][[4]][["biol"]])[,-next_year,,,,]@.Data,equals(n(om_out[["biols"]][[4]])[,-next_year,,,,]@.Data))
#
#    #------------------
#    # 1 biol -> 0 catch (B5)
#    #------------------
#    # Total F on the biol
#    f5 <- test_operatingModel_total_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5)
#    f5_sub <- test_operatingModel_total_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5, dim_min, dim_max)
#    # Total Z of biol
#    z5 <- test_operatingModel_Z(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5)
#    # SSB of biol
#    ssb5_out <- test_operatingModel_SSB_FLQ(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5)
#    ssb5_out_subset <- test_operatingModel_SSB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 5, dim_min[-1], dim_max[-1])
#    # Get metrics from IP OM
#    biomass_in5 <- quantSums(n(om[["biols"]][[5]][["biol"]]) * wt(om[["biols"]][[5]][["biol"]]))
#    z5in <- m(om[["biols"]][[5]][["biol"]])
#    ssb5_in <- quantSums(n(om[["biols"]][[5]][["biol"]]) * wt(om[["biols"]][[5]][["biol"]]) * fec(om[["biols"]][[5]][["biol"]]) * exp(-((m(om[["biols"]][[5]][["biol"]]))* spwn(om[["biols"]][[5]][["biol"]]))))
#
#    # Biol abundances
#    next_n5 <- n(om[["biols"]][[5]][["biol"]])[,next_year,,next_season,,]
#    next_n5[] <- 0
#    next_n5[2:dims[1],] <- (n(om[["biols"]][[5]][["biol"]])[,year,,season,,] * exp(-z5in[,year,,season,,]))[1:(dims[1]-1),]
#    next_n5[dims[1]] <- next_n5[dims[1]] + (n(om[["biols"]][[5]][["biol"]])[dims[1],year,,season,,] * exp(-z5in[dims[1],year,,season,,]))
#    # Rec - it's Ricker for Biol 5
#    ssb_timestep <- timestep - om[["biols"]][[5]][["srr_timelag"]] + 1
#    ssb_year <-  floor((ssb_timestep-1) / dims[4]) + 1; 
#    ssb_season <- (ssb_timestep-1) %% dims[4] + 1;
#    ssb_rec <- ssb5_in[,ssb_year,,ssb_season,,]
#    rec_in <- om[["biols"]][[5]][["srr_params"]]['a',] * ssb_rec * exp(-om[["biols"]][[5]][["srr_params"]]['b',] * ssb_rec)
#    # Multiplicative residuals
#    next_n5[1,] <- rec_in * exp(om[["biols"]][[5]][["srr_residuals"]][,next_year,,next_season,,])
#    # Test Fs and Zs
#    expect_that(all(unname(f5@.Data)==0), equals(TRUE)) # Total F on Biol
#    expect_that(all(unname(f5_sub@.Data)==0), equals(TRUE)) # Total F on Biol
#    expect_that(unname(z5@.Data), equals(unname(z5in@.Data))) # Partial F from FC
#    # Test SSB
#    expect_that(unname(ssb5_out@.Data), equals(unname(ssb5_in@.Data)))
#    expect_that(unname(ssb5_out_subset@.Data), equals(unname(ssb5_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data)))
#    # Abundances
#    expect_that(next_n5, equals(n(om_out[["biols"]][[5]])[,next_year,,next_season,,]))
#    # Other abundances unaffected
#    expect_that(n(om[["biols"]][[5]][["biol"]])[,-next_year,,,,]@.Data,equals(n(om_out[["biols"]][[5]])[,-next_year,,,,]@.Data))
#})
#
## Just checking partial Fs - including when they should be 0
## Could remove these as just repeating the tests above
#test_that("operatingModel partial Fs",{
#    om <- make_test_operatingModel1(5)
#    # Partial F of F/C on a B
#    # F/C 11 -> B
#    fishery_no <- 1
#    catch_no <- 1
#    biol_no <- 1
#    dims <- dim(n(om[["biols"]][[1]][["biol"]]))
#    dim_max <- dims
#    dim_min <- round(runif(6, min=1, max=dim_max))
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    biomass <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
#    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
#    qin <- sweep(sweep(biomass, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
#    fin <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
#    expect_that(fout@.Data, equals(fin@.Data))
#    expect_that(fout_sub@.Data, equals(fin[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    biol_no <- 2
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(all(fout==0.0), is_true())
#    expect_that(all(fout_sub==0.0), is_true())
#    biol_no <- 3
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(all(fout==0.0), is_true())
#    expect_that(all(fout_sub==0.0), is_true())
#    biol_no <- 4
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    expect_that(all(fout==0.0), is_true())
#    biol_no <- 5
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    expect_that(all(fout==0.0), is_true())
#
#    # F/C 12 -> B
#    fishery_no <- 1
#    catch_no <- 2
#    biol_no <- 1
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(all(fout==0.0), is_true())
#    expect_that(all(fout_sub==0.0), is_true())
#    biol_no <- 2
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    biomass <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
#    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
#    qin <- sweep(sweep(biomass, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
#    fin <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
#    expect_that(fout@.Data, equals(fin@.Data))
#    expect_that(fout_sub@.Data, equals(fin[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    biol_no <- 3
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(all(fout==0.0), is_true())
#    expect_that(all(fout_sub==0.0), is_true())
#    biol_no <- 4
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(all(fout==0.0), is_true())
#    expect_that(all(fout_sub==0.0), is_true())
#
#    # F/C 21 -> B
#    fishery_no <- 2
#    catch_no <- 1
#    biol_no <- 1
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(all(fout==0.0), is_true())
#    expect_that(all(fout_sub==0.0), is_true())
#    biol_no <- 2
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    biomass <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
#    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
#    qin <- sweep(sweep(biomass, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
#    fin <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
#    expect_that(fout@.Data, equals(fin@.Data))
#    expect_that(fout_sub@.Data, equals(fin[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    biol_no <- 3
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(all(fout==0.0), is_true())
#    expect_that(all(fout_sub==0.0), is_true())
#    biol_no <- 4
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(all(fout==0.0), is_true())
#    expect_that(all(fout_sub==0.0), is_true())
#
#    # F/C 22 -> B
#    fishery_no <- 2
#    catch_no <- 2
#    biol_no <- 1
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(all(fout==0.0), is_true())
#    expect_that(all(fout_sub==0.0), is_true())
#    biol_no <- 2
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    expect_that(all(fout==0.0), is_true())
#    expect_that(all(fout_sub==0.0), is_true())
#    biol_no <- 3
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    biomass <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
#    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
#    qin <- sweep(sweep(biomass, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
#    fin <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
#    expect_that(fout@.Data, equals(fin@.Data))
#    expect_that(fout_sub@.Data, equals(fin[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    biol_no <- 4
#    fout <- test_operatingModel_partial_f(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no)
#    fout_sub <- test_operatingModel_partial_f_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, dim_min, dim_max)
#    biomass <- quantSums(n(om[["biols"]][[biol_no]][["biol"]]) * wt(om[["biols"]][[biol_no]][["biol"]]))
#    cq_flq <- as(catch.q(om[["fisheries"]][[fishery_no]][[catch_no]]), "FLQuant")
#    qin <- sweep(sweep(biomass, c(1,3,4,5), -cq_flq[2,], "^"), c(1,3,4,5), cq_flq[1], "*")
#    fin <- sweep(catch.sel(om[["fisheries"]][[fishery_no]][[catch_no]]), 2:6, qin * effort(om[["fisheries"]][[fishery_no]]), "*")
#    expect_that(fout_sub@.Data, equals(fin[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]@.Data))
#    expect_that(fout@.Data, equals(fin@.Data))
#})


# OLD. DELETE
#test_that("operatingModel eval_target", {
    # catch targets
#    target <- data.frame(year = 4,
#                     season = 1,
#                     value = 1000,
#                     quantity = 'catch',
#                     fishery = c(1,2,NA,1,1,NA),
#                     catch =   c(1,1,NA,1,NA,1),
#                     biol = c(NA,NA,1,1,1,1)
#                     )
#    target_iters <- array(NA, dim=c(nrow(target),3,niters), dimnames=list(target_no=1:nrow(target), c("min","value","max"), iter=1:niters))
#    target_iters[1:nrow(target), "value",] <- abs(rnorm(niters * nrow(target), mean = target$value[1:nrow(target)], sd = 0.1))
#    fwc <- fwdControl(target=target, iters=target_iters)
#    # Add timestep column to control object - necessary for abundance timesteps
#    fwc@target@element$timestep <- fwc@target@element$year
#    # Add target column to control object - make them all sim targets for testing
#    fwc@target@element$target <- 1
#    # Add FCB array - will be constructed on R side before calling fwd()
#    FCB <- array(c(1,2,1,1,1,1), dim=c(2,3))
#    colnames(FCB) <- c("F","C","B")
#    attr(fwc@target, "FCB") <- FCB
#    om[["fwc"]] <- fwc
#    # Short cuts
#    years <- om[["fwc"]]@target@element$year
#     
#    indices_min <- c(years[1],1,1,1,1)
#    indices_max <- c(years[1],1,1,1,niters)
#
#    # Catch
#    # Check eval_target
#    # Sim target 1 - no biol
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 1, indices_min, indices_max)
#    catch_in <- catch(om[["fisheries"]][[1]][[1]])[,years[1]]
#    expect_that(out@.Data, equals(catch_in@.Data))
#    # Sim target 2 - no biol
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, indices_min, indices_max)
#    catch_in <- catch(om[["fisheries"]][[2]][[1]])[,years[1]]
#    expect_that(out@.Data, equals(catch_in@.Data))
#    # Sim target 3 - catch from a biol
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 3, indices_min, indices_max)
#    catch_in <- catch(om[["fisheries"]][[1]][[1]])[,years[1]] + catch(om[["fisheries"]][[2]][[1]])[,years[1]]
#    expect_that(c(out@.Data), equals(c(catch_in@.Data)))
#    # Sim target 4 - catch from a biol, fishery, catch - should fail
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 4, indices_min, indices_max), throws_error())
#    # Sim target 5-6 - either only catch or fishery specifed - should fail
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 5, indices_min, indices_max), throws_error())
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 6, indices_min, indices_max), throws_error())
#
#    # Landings
#    om[["fwc"]]@target@element$quantity <- 'landings'
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 1, indices_min, indices_max)
#    landings_in <- landings(om[["fisheries"]][[1]][[1]])[,years[1]]
#    expect_that(out@.Data, equals(landings_in@.Data))
#    # Sim target 2 - no biol
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, indices_min, indices_max)
#    landings_in <- landings(om[["fisheries"]][[2]][[1]])[,years[1]]
#    expect_that(out@.Data, equals(landings_in@.Data))
#    # Sim target 3 - landings from a biol
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 3, indices_min, indices_max)
#    landings_in <- landings(om[["fisheries"]][[1]][[1]])[,years[1]] + landings(om[["fisheries"]][[2]][[1]])[,years[1]]
#    expect_that(c(out@.Data), equals(c(landings_in@.Data)))
#    # Sim target 4 - catch from a biol, fishery, catch - should fail
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 4, indices_min, indices_max), throws_error())
#    # Sim target 5-6 - either only catch or fishery specifed - should fail
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 5, indices_min, indices_max), throws_error())
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 6, indices_min, indices_max), throws_error())
#
#    # Discards
#    om[["fwc"]]@target@element$quantity <- 'discards'
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 1, indices_min, indices_max)
#    discards_in <- discards(om[["fisheries"]][[1]][[1]])[,years[1]]
#    expect_that(out@.Data, equals(discards_in@.Data))
#    # Sim target 2 - no biol
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, indices_min, indices_max)
#    discards_in <- discards(om[["fisheries"]][[2]][[1]])[,years[1]]
#    expect_that(out@.Data, equals(discards_in@.Data))
#    # Sim target 3 - discards from a biol
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 3, indices_min, indices_max)
#    discards_in <- discards(om[["fisheries"]][[1]][[1]])[,years[1]] + discards(om[["fisheries"]][[2]][[1]])[,years[1]]
#    expect_that(c(out@.Data), equals(c(discards_in@.Data)))
#    # Sim target 4 - catch from a biol, fishery, catch - should fail
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 4, indices_min, indices_max), throws_error())
#    # Sim target 5-6 - either only catch or fishery specifed - should fail
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 5, indices_min, indices_max), throws_error())
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 6, indices_min, indices_max), throws_error())
#
#    # Fbar
#    target <- data.frame(year = 4,
#                     season = 1,
#                     value = 1,
#                     quantity = 'f',
#                     fishery = c(1,2,NA,1,1,NA),
#                     catch =   c(1,1,NA,1,NA,1),
#                     biol =    c(1,1,1,NA,1,1),
#                     minAge = 2,
#                     maxAge = 5
#                     )
#    target_iters <- array(NA, dim=c(nrow(target),3,niters), dimnames=list(target_no=1:nrow(target), c("min","value","max"), iter=1:niters))
#    target_iters[1:nrow(target), "value",] <- abs(rnorm(niters * nrow(target), mean = target$value[1:nrow(target)], sd = 0.1))
#    fwc <- fwdControl(target=target, iters=target_iters)
#    # Add timestep column to control object - necessary for abundance timesteps
#    fwc@target@element$timestep <- fwc@target@element$year
#    # Add target column to control object - make them all sim targets for testing
#    fwc@target@element$target <- 1
#    # Add FCB array - will be constructed on R side before calling fwd()
#    FCB <- array(c(1,2,1,1,1,1), dim=c(2,3))
#    colnames(FCB) <- c("F","C","B")
#    attr(fwc@target, "FCB") <- FCB
#    om[["fwc"]] <- fwc
#    # Short cuts
#    # F = Q * effort * sel
#    # F = alpha * B ^ -beta * effort * sel
#    q1 <- c(catch.q(om[["fisheries"]][[1]][[1]])['alpha',]) * (tsb(om[["biols"]][[1]][["biol"]]) ^ -c(catch.q(om[["fisheries"]][[1]][[1]])['beta',]))
#    q2 <- c(catch.q(om[["fisheries"]][[2]][[1]])['alpha',]) * (tsb(om[["biols"]][[1]][["biol"]]) ^ -c(catch.q(om[["fisheries"]][[2]][[1]])['beta',]))
#    fin1 <- sweep(catch.sel(om[["fisheries"]][[1]][[1]]), 2:6, q1 * effort(om[["fisheries"]][[1]]), "*")[,4]
#    fin2 <- sweep(catch.sel(om[["fisheries"]][[2]][[1]]), 2:6, q2 * effort(om[["fisheries"]][[2]]), "*")[,4]
#    fin <- fin1 + fin2
#    fbar_in1 <- apply(fin1[as.character(fwc@target@element[1,"minAge"]:fwc@target@element[1,"maxAge"]),], 2:6, mean)
#    fbar_in2 <- apply(fin2[as.character(fwc@target@element[1,"minAge"]:fwc@target@element[1,"maxAge"]),], 2:6, mean)
#    fbar_in <- apply(fin[as.character(fwc@target@element[1,"minAge"]:fwc@target@element[1,"maxAge"]),], 2:6, mean)
#
#    # Sim target 1 - F of a fishery and catch on a biol
#    fbar_out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 1, indices_min, indices_max)
#    expect_that(c(fbar_out), equals(c(fbar_in1)))
#    # Sim target 2 - F of a fishery and catch on a biol
#    fbar_out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, indices_min, indices_max)
#    expect_that(c(fbar_out), equals(c(fbar_in2)))
#    # Sim target 3 - F on a biol
#    fbar_out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 3, indices_min, indices_max)
#    expect_that(c(fbar_out), equals(c(fbar_in)))
#    # Sim target 4 - 6 - F on a catch and fishery only, or mismatch fishery and catch - all fail
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 4, indices_min, indices_max), throws_error())
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 5, indices_min, indices_max), throws_error())
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 6, indices_min, indices_max), throws_error())
#
#    # Abundance targets
#    target <- data.frame(year = 4,
#                     season = 1,
#                     value = 1,
#                     quantity = 'biomass',
#                     fishery = c(1,2,NA,1),
#                     catch =   c(1,1,NA,1),
#                     biol =    c(1,1,1,NA)
#                     )
#    target_iters <- array(NA, dim=c(nrow(target),3,niters), dimnames=list(target_no=1:nrow(target), c("min","value","max"), iter=1:niters))
#    target_iters[1:nrow(target), "value",] <- abs(rnorm(niters * nrow(target), mean = target$value[1:nrow(target)], sd = 0.1))
#    fwc <- fwdControl(target=target, iters=target_iters)
#    # Add timestep column to control object - necessary for abundance timesteps
#    fwc@target@element$timestep <- fwc@target@element$year
#    # Add target column to control object - make them all sim targets for testing
#    fwc@target@element$target <- 1
#    # Add FCB array - will be constructed on R side before calling fwd()
#    FCB <- array(c(1,2,1,1,1,1), dim=c(2,3))
#    colnames(FCB) <- c("F","C","B")
#    attr(fwc@target, "FCB") <- FCB
#    om[["fwc"]] <- fwc
#
#    biomass_in <- tsb(om[["biols"]][[1]][["biol"]])[,4]
#    # Sim target 1 & 2 - Biomass of a biol, fishery and catch also specified but ignored
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 1, indices_min, indices_max)
#    expect_that(c(out), equals(c(biomass_in)))
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, indices_min, indices_max)
#    expect_that(c(out), equals(c(biomass_in)))
#    # Sim target 3 - Just biol specified 
#    out <- test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 3, indices_min, indices_max)
#    expect_that(c(out), equals(c(biomass_in)))
#    # Sim target 4 - No biol specified 
#    expect_that(test_operatingModel_eval_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 4, indices_min, indices_max), throws_error())
#
#
#})
#
#
## Evaluate the control object
#test_that("operatingModel get_target_value methods", {
#    # Get actual values (inc relative)
#    # Get min / max
#    # Get relative min / max
#    # rel min and max on same line
#    niters <- 20 
#    om <- make_test_operatingModel2(niters)
#    # Set up some min max landings values for testing
#    min_discards <- apply(discards(om[["fisheries"]][[1]][[1]]), 2, min) * 1.01
#    max_biomass <- apply(quantSums(n(om[["biols"]][[1]][["biol"]]) * wt(om[["biols"]][[1]][["biol"]])), 2, max) * 0.99
#    rel_min_catch <- min(c(catch(om[["fisheries"]][[1]][[1]])[,8] / catch(om[["fisheries"]][[1]][[1]])[,7])) * 1.01
#    rel_max_catch <- max(c(catch(om[["fisheries"]][[1]][[1]])[,8] / catch(om[["fisheries"]][[1]][[1]])[,7])) * 0.99 
#    rel_max_landings <- max(c(landings(om[["fisheries"]][[2]][[1]])[,9] / landings(om[["fisheries"]][[1]][[1]])[,9])) * 0.99
#
#    target <- data.frame(year = c(4,5,6,7,8,9,8),
#                     season = rep(1,7),
#                     value = c(1000, 0.9, NA, NA, NA, NA, NA),
#                     min = c(NA, NA, min_discards[,6], NA, rel_min_catch, NA, rel_min_catch),
#                     max = c(NA, NA, NA, max_biomass[,7], NA, rel_max_landings, rel_max_catch),
#                     quantity = c('catch', 'landings', 'discards', 'biomass', 'catch', 'landings', 'catch'),
#                     fishery = c(1,2,1,NA,1,2,1),
#                     catch = c(1,1,1,NA,1,1,1),
#                     biol = c(NA,NA,NA,1,NA,NA,NA),
#                     relYear = c(NA,4,NA,NA,7,9,7), 
#                     relSeason = c(NA,1,NA,NA,1,1,1),
#                     relFishery = c(NA,2,NA,NA,1,1,1), 
#                     relCatch = c(NA,1,NA,NA,1,1,1),
#                     relBiol = c(NA,NA,NA,NA,NA,NA,NA)
#                     )
#    target_iters <- array(NA, dim=c(nrow(target),3,niters), dimnames=list(target_no=1:nrow(target), c("min","value","max"), iter=1:niters))
#    target_iters[1:2, "value",] <- abs(rnorm(niters * 2, mean = target$value[1:2], sd = 1))
#    target_iters[c(3,5), "min",] <- abs(rnorm(niters * 2, mean = target$min[c(3,5)], sd = 0))
#    target_iters[c(4,6), "max",] <- abs(rnorm(niters * 2, mean = target$max[c(4,6)], sd = 0))
#    target_iters[7, "max",] <- abs(rnorm(niters, mean = target$max[7], sd = 0))
#    target_iters[7, "min",] <- abs(rnorm(niters, mean = target$min[7], sd = 0))
#
#    fwc <- fwdControl(target=target, iters=target_iters)
#    # Add timestep column to control object - necessary for abundance timesteps
#    fwc@target@element$timestep <- fwc@target@element$year
#    # Add target column to control object
#    fwc@target@element$target <- c(1,1,2,2,3,3,4)
#    # Add FCB array - will be constructed on R side before calling fwd()
#    FCB <- array(c(1,2,1,1,1,1), dim=c(2,3))
#    colnames(FCB) <- c("F","C","B")
#    attr(fwc@target, "FCB") <- FCB
#    om[["fwc"]] <- fwc
#
#    # Handy subsets
#    years <- om[["fwc"]]@target@element$year
#
#    # Target 1 - abs catch and rel landings
#    val_out <- test_operatingModel_get_target_value(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1)
#    # First sim target just from the control object
#    expect_that(unname(om[["fwc"]]@target@iters[1,"value",]), equals(val_out[1:niters]))
#    # Second sim target is relative but still from control object
#    expect_that(unname(om[["fwc"]]@target@iters[2,"value",]), equals(val_out[(niters+1):length(val_out)]))
#
#    # Target 2 - min discards and max biomass
#    val_out <- test_operatingModel_get_target_value(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2)
#    # sim target 1
#    # All val_out should be greater than min_discards
#    sub_val <- val_out[1:niters]
#    discards_in <- c(discards(om[["fisheries"]][[1]][[1]])[,years[3]])
#    min_vals <- (discards_in < c(min_discards[,6]))
#    expect_that(sub_val[min_vals], equals(rep(c(min_discards[,6]), sum(min_vals))))
#    expect_that(sub_val[!min_vals], equals(discards_in[!min_vals]))
#    # biomass target 
#    sub_val <- val_out[(niters+1):(2*niters)]
#    biomass_in <- c(quantSums(n(om[["biols"]][[1]][["biol"]]) * wt(om[["biols"]][[1]][["biol"]]))[,years[4]])
#    max_vals <- (biomass_in > c(max_biomass[,years[4]]))
#    expect_that(sub_val[max_vals], equals(rep(c(max_biomass[,years[4]]), sum(max_vals))))
#    expect_that(sub_val[!max_vals], equals(biomass_in[!max_vals]))
#
#    # Target 3 - relative min catch, relative max landings
#    val_out <- test_operatingModel_get_target_value(om[["fisheries"]], om[["biols"]], om[["fwc"]], 3)
#    # rel min catch
#    sub_val <- val_out[1:niters]
#    rel_catch_in <- c(catch(om[["fisheries"]][[1]][[1]])[,8] / catch(om[["fisheries"]][[1]][[1]])[,7])
#    min_vals <- (rel_catch_in < rel_min_catch)
#    expect_that(sub_val[min_vals], equals(rep(rel_min_catch, sum(min_vals))))
#    expect_that(sub_val[!min_vals], equals(rel_catch_in[!min_vals]))
#    # rel max landings - relative within same timestep!
#    sub_val <- val_out[(niters+1):(2*niters)]
#    rel_landings_in <- c(landings(om[["fisheries"]][[2]][[1]])[,9] / landings(om[["fisheries"]][[1]][[1]])[,9])
#    max_vals <- (rel_landings_in > rel_max_landings)
#    expect_that(sub_val[max_vals], equals(rep(rel_max_landings, sum(max_vals))))
#    expect_that(sub_val[!max_vals], equals(rel_landings_in[!max_vals]))
#
#    # Target 4 - relative min and max on the same line
#    val_out <- test_operatingModel_get_target_value(om[["fisheries"]], om[["biols"]], om[["fwc"]], 4)
#    rel_catch_in <- c(catch(om[["fisheries"]][[1]][[1]])[,8] / catch(om[["fisheries"]][[1]][[1]])[,7])
#    min_vals <- (rel_catch_in < rel_min_catch)
#    expect_that(val_out[min_vals], equals(rep(rel_min_catch, sum(min_vals))))
#    max_vals <- (rel_catch_in > rel_max_catch)
#    expect_that(val_out[max_vals], equals(rep(rel_max_catch, sum(max_vals))))
#    expect_that( val_out[(!max_vals) & (!min_vals)] , equals(rel_catch_in[(!max_vals) & (!min_vals)]))
#
#
#})
#
#
#
#
#
##
##test_that("Test projection with individual targets",{
##    # Test operating model.
##    # Need FLFisheries, FLBiol, SRR, F and Fspwn
##    data(ple4)
##    # make the biol
##    # srr
##    srr_model_name <- "bevholt"
##    srr <- as.FLSR(ple4, model=srr_model_name)
##    srr <- fmle(srr, control = list(trace=0))
##    srr_params <- as.FLQuant(params(srr))
##    srr_residuals <- exp(residuals(srr))
##    srr_residuals <- window(srr_residuals,end=2009)
##    srr_residuals[,"2009"] <- srr_residuals[,"2008"]
##    #srr_residuals[] <- 1 # turn off residuals
##    srr_residuals_mult <- TRUE
##    srr_timelag <- 1
##    niters <- 500
##    data(ple4)
##    ple4 <- propagate(ple4,niters)
##    biol <- as(ple4, 'FLBiol')
##    catch <- as(ple4, 'FLCatch')
##    catch@name <- "ple4 catch"
##    catch@desc <- "ple4 catch"
##    # Hack
##    fishery <- FLFishery(ple4=catch)
##    fishery@name <- "ple4 fishery"
##    fishery@desc <- "ple4 fishery"
##    fisheries <- FLFisheries(ple4=fishery)
##    fisheries@desc <- "ple4 fisheries"
##    # Fs
##    f <- list(ple4 = harvest(ple4))
##    f_spwn_flq <- harvest(ple4)
##    f_spwn_flq[] <- 0
##    f_spwn <- list(ple4 = f_spwn_flq)
##
##    # Add some noise on biol n and harvest
##    n(biol) <- n(biol) * rlnorm(prod(dim(n(biol))), sd = 0.1)
##    f[[1]] <- f[[1]] * rlnorm(prod(dim(f[[1]])), sd = 0.1)
##
##    # A simple target object - 1 year, 1 target
##    minAge <- 2
##    maxAge <- 6
##    # F target
##    target <- data.frame(year=2, quantity = 'f', value = 0.5, minAge=minAge, maxAge=maxAge, season=1L)
##    fwc <- fwdControl(target=target, iter=niters)
##    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1)
##    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
##    # equals?
##    expect_that(c(apply(out[["f"]][[1]][ac(minAge:maxAge),2] ,2:6,mean)), equals(unname(fwc@target@iters[1,"value",])))
##
##    # Special case - 0 F
##    target <- data.frame(year=2, quantity = 'f', value = 0, minAge=minAge, maxAge=maxAge, season=1L)
##    fwc <- fwdControl(target=target, iter=niters)
##    #fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1)
##    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
##    # equals?
##    expect_that(c(apply(out[["f"]][[1]][ac(minAge:maxAge),2] ,2:6,mean)), equals(unname(fwc@target@iters[1,"value",])))
##    #discards.ratio(out[["fisheries"]][[1]][[1]])[,2]
##    #discards.ratio(fisheries[[1]][[1]])[,2]
##
##    # Catch target
##    target <- data.frame(year=2, quantity = 'catch', value = 100000, minAge=minAge, maxAge=maxAge, season=1L)
##    fwc <- fwdControl(target=target, iter=niters)
##    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(100000), sd = 0.1)
##    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
##    expect_that(c(catch(out[["fisheries"]][[1]][[1]])[,2]), equals(unname(fwc@target@iters[1,"value",])))
##
##    # Biomass target
##    target <- data.frame(year=2, quantity = 'biomass', value = 300000, minAge=minAge, maxAge=maxAge, season=1L)
##    fwc <- fwdControl(target=target, iter=niters)
##    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1)
##    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
##    expect_that(c(quantSums(n(out[["biol"]]) * wt(out[["biol"]]))[,2]), equals(unname(fwc@target@iters[1,"value",])))
##
##    # SSB target
##    target <- data.frame(year=2, quantity = 'ssb', value = 300000, minAge=minAge, maxAge=maxAge, season=1L)
##    fwc <- fwdControl(target=target, iter=niters)
##    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1)
##    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
##    ssb_out <- quantSums(n(out[["biol"]]) * wt(out[["biol"]]) * fec(out[["biol"]]) * exp(-out[["f_spwn"]][[1]] * out[["f"]][[1]] - m(out[["biol"]]) * spwn(out[["biol"]])))
##    expect_that(c(ssb_out[,2]), equals(unname(fwc@target@iters[1,"value",])))
##})
##
##test_that("Test projection with really complicated target",{
##    # Test operating model.
##    # Need FLFisheries, FLBiol, SRR, F and Fspwn
##    data(ple4)
##    # make the biol
##    # srr
##    srr_model_name <- "bevholt"
##    srr <- as.FLSR(ple4, model=srr_model_name)
##    srr <- fmle(srr, control = list(trace=0))
##    srr_params <- as.FLQuant(params(srr))
##    srr_residuals <- exp(residuals(srr))
##    srr_residuals <- window(srr_residuals,end=2009)
##    srr_residuals[,"2009"] <- srr_residuals[,"2008"]
##    #srr_residuals[] <- 1 # turn off residuals
##    srr_residuals_mult <- TRUE
##    srr_timelag <- 1
##    niters <- 500
##    data(ple4)
##    ple4 <- propagate(ple4,niters)
##    biol <- as(ple4, 'FLBiol')
##    catch <- as(ple4, 'FLCatch')
##    catch@name <- "ple4 catch"
##    catch@desc <- "ple4 catch"
##    # Hack
##    fishery <- FLFishery(ple4=catch)
##    fishery@name <- "ple4 fishery"
##    fishery@desc <- "ple4 fishery"
##    fisheries <- FLFisheries(ple4=fishery)
##    fisheries@desc <- "ple4 fisheries"
##    # Fs
##    f <- list(ple4 = harvest(ple4))
##    f_spwn_flq <- harvest(ple4)
##    f_spwn_flq[] <- 0
##    f_spwn <- list(ple4 = f_spwn_flq)
##
##    # Add some noise on biol n and harvest
##    set.seed(1)
##    n(biol) <- n(biol) * rlnorm(prod(dim(n(biol))), sd = 0.1)
##    f[[1]] <- f[[1]] * rlnorm(prod(dim(f[[1]])), sd = 0.1)
##    minAge <- 2
##    maxAge <- 6
##
##    target <- data.frame(year = 2, quantity = 'f', value = 0.5, minAge = 2, maxAge = 6, season = 1L)
##    target <- rbind(target,
##        # Absolute values
##        data.frame(year = 3, quantity = 'catch', value = 100000, minAge = NA, maxAge = NA, season = 1L),
##        data.frame(year = 5, quantity = 'biomass', value = 300000, minAge = NA, maxAge = NA, season = 1L), # affects F in year 4
##        data.frame(year = 6, quantity = 'ssb', value = 300000, minAge = NA, maxAge = NA, season = 1L), # affects F in year 5
##        # Max targets
##        data.frame(year = 7, quantity = 'catch', value = 100000, minAge = NA, maxAge = NA, season = 1L), # Catch target
##        data.frame(year = 7, quantity = 'f', value = 1, minAge = 2, maxAge = 6, season = 1L), # but maximum F limits it - need to hack this line after constructor
##        # Min target
##        data.frame(year = 8, quantity = 'f', value = 0.2, minAge = 2, maxAge = 6, season = 1L), # F target
##        data.frame(year = 8, quantity = 'catch', value = 100000, minAge = NA, maxAge = NA, season = 1L), # but minimum catch limits it - need to hack this line after constructor
##        # Relative target
##        data.frame(year = 9, quantity = 'catch', value = 0.5, minAge = NA, maxAge = NA, season = 1L), # Need to hack this after constructor
##        # Relative max target
##        data.frame(year = 11, quantity = 'f', value = 0.5, minAge = 2, maxAge = 6, season = 1L), # F target
##        data.frame(year = 11, quantity = 'catch', value = 1.15, minAge = NA, maxAge = NA, season = 1L), # But catch cannot increase more than 15% from previous yearNeed to hack this after constructor
##        # Relative min target
##        data.frame(year = 13, quantity = 'f', value = 0.3, minAge = 2, maxAge = 6, season = 1L), # F target
##        data.frame(year = 13, quantity = 'catch', value = 0.85, minAge = NA, maxAge = NA, season = 1L) # But catch cannot increase more than 15% from previous yearNeed to hack this after constructor
##    )
##
##    fwc <- fwdControl(target=target, iter=niters)
##    # Hack min and max values of f target
##    fwc@target@element
##    fwc@target@element[6,c("value","max")] <- c(NA,0.3)
##    fwc@target@element[8,c("value","min")] <- c(NA,100000)
##    fwc@target@element[9,c("relYear","relSeason")] <- c(8L,1L)
##    fwc@target@element[11,c("value","max","relYear","relSeason")] <- c(NA, 1.15, 10L, 1L)
##    fwc@target@element[13,c("value","min","relYear","relSeason")] <- c(NA, 0.85, 12L, 1L)
##
##    # Fix iter values
##    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1) # f
##    fwc@target@iters[2,"value",] <- rlnorm(niters, mean = log(100000), sd = 0.1) # sd = 1 results in bad iters - good for testing
##    fwc@target@iters[3,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1) # biomass
##    fwc@target@iters[4,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1) # ssb
##    fwc@target@iters[5,"value",] <- rlnorm(niters, mean = log(100000), sd = 0.1) # catch for f max test
##    fwc@target@iters[6,"value",] <- NA # fbar has max only
##    fwc@target@iters[6,"max",] <- rlnorm(niters, mean = log(0.35), sd = 0.01)
##    fwc@target@iters[7,"value",] <- rlnorm(niters, mean = log(0.2), sd = 0.1) 
##    fwc@target@iters[8,"value",] <- NA # fbar has max only
##    fwc@target@iters[8,"min",] <- rlnorm(niters, mean = log(100000), sd = 0.1) 
##    fwc@target@iters[9,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1) 
##    fwc@target@iters[10,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1) 
##    fwc@target@iters[11,"value",] <- NA
##    fwc@target@iters[11,"max",] <- 1.15
##    fwc@target@iters[12,"value",] <- rlnorm(niters, mean = log(0.3), sd = 0.1) 
##    fwc@target@iters[13,"value",] <- NA
##    fwc@target@iters[13,"min",] <- 0.85 
##
##    # Call test run with (something like this will go in fwd() method)
##    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
##    ssb_out <- quantSums(n(out[["biol"]]) * wt(out[["biol"]]) * fec(out[["biol"]]) * exp(-out[["f_spwn"]][[1]] * out[["f"]][[1]] - m(out[["biol"]]) * spwn(out[["biol"]])))
##
##    # Check that discards_ratio has not been mucked about with
##    dr_orig <- discards.ratio(fisheries[[1]][[1]])
##    dr_out <- discards.ratio(out[["fisheries"]][[1]][[1]])
##    expect_that(dr_orig, equals(dr_out))
##
##    # Test output
##    # Absolute targets
##    expect_that(c(apply(out[["f"]][[1]][ac(minAge:maxAge),2] ,2:6,mean)), equals(unname(fwc@target@iters[1,"value",]))) # abs ssb
##    expect_that(c(catch(out[["fisheries"]][[1]][[1]])[,3]), equals(unname(fwc@target@iters[2,"value",]))) # abs catch
##    expect_that(c(quantSums(n(out[["biol"]]) * wt(out[["biol"]]))[,5]), equals(unname(fwc@target@iters[3,"value",]))) # abs biomass
##    expect_that(c(ssb_out[,6]), equals(unname(fwc@target@iters[4,"value",])))
##
##    # Max target - bit tricky for comparison - cannot use < > as differences are sometimes +-1e-16 
##    # so need tolerances
##    tol <- .Machine$double.eps ^ 0.5   
##    # Find which Catches have not been hit
##    fmax_iters <- abs((c(catch(out[["fisheries"]][[1]][[1]])[,7]) / unname(fwc@target@iters[5,"value",])) - 1) > tol
##    # Check that fmax has been hit for these iters
##    expect_that(all(abs((c(apply(out[["f"]][[1]][ac(minAge:maxAge),7] ,2:6,mean))[fmax_iters] / unname(fwc@target@iters[6,"max",])[fmax_iters]) -1) < tol), is_true())
##    # And that F is less than Fmax for the iters where catches have been hit
##    expect_that(all(c(apply(out[["f"]][[1]][ac(minAge:maxAge),7] ,2:6,mean))[!fmax_iters] < unname(fwc@target@iters[6,"max",])[!fmax_iters]), is_true())
##
##    # Min test
##    # Iters where F has not been hit
##    cmin_iters <- abs((c(apply(out[["f"]][[1]][ac(minAge:maxAge),8] ,2:6,mean)) / unname(fwc@target@iters[7,"value",]))-1) > tol
##    # cmin has been hit for these iters
##    expect_that(all(abs((c(catch(out[["fisheries"]][[1]][[1]])[,8])[cmin_iters] / unname(fwc@target@iters[8,"min",])[cmin_iters])-1) < tol), is_true())
##    # And that Catch is greater than Cmin for the other iters (where original f target was hit)
##    expect_that(all(c(catch(out[["fisheries"]][[1]][[1]])[,8])[!cmin_iters] > unname(fwc@target@iters[8,"min",])[!cmin_iters]), is_true())
##
##    # Relative test
##    expect_that(c(catch(out[["fisheries"]][[1]][[1]])[,8]) * unname(fwc@target@iters[9,"value",]), equals(c(catch(out[["fisheries"]][[1]][[1]])[,9])))
##
##    # Relative max test
##    # Which iters have been limited by cmax, i.e. f target not hit
##    cmax_iters <- abs((c(apply(out[["f"]][[1]][ac(minAge:maxAge),11] ,2:6,mean)) / unname(fwc@target@iters[10,"value",]))-1) > tol
##    # Check that these iters have hit the limit
##    # Catch should be at limit
##    expect_that(all( abs((c(catch(out[["fisheries"]][[1]][[1]])[,11])[cmax_iters] / (c(catch(out[["fisheries"]][[1]][[1]])[,10])[cmax_iters] * unname(fwc@target@iters[11,"max",])[cmax_iters]))-1) < tol), is_true())
##    # Unlimited iters should have catches less than max
##    expect_that(all(c(catch(out[["fisheries"]][[1]][[1]])[,11])[!cmax_iters] < (c(catch(out[["fisheries"]][[1]][[1]])[,10])[!cmax_iters] * unname(fwc@target@iters[11,"max",])[!cmax_iters])), is_true())
##
##    # Relative min test
##    # Which iters have been limited by cmax, i.e. f target not hit
##    cmax_iters <- abs(( c(apply(out[["f"]][[1]][ac(minAge:maxAge),13] ,2:6,mean)) / unname(fwc@target@iters[12,"value",]))-1) > tol
##    # These iters should be at catch limit
##    expect_that(all( abs((c(catch(out[["fisheries"]][[1]][[1]])[,13])[cmax_iters] / (c(catch(out[["fisheries"]][[1]][[1]])[,12])[cmax_iters] * unname(fwc@target@iters[13,"min",])[cmax_iters]))-1) < tol), is_true())
##    # Other iters should have catches greater than catch limit
##    expect_that(all(c(catch(out[["fisheries"]][[1]][[1]])[,13])[!cmax_iters] > (c(catch(out[["fisheries"]][[1]][[1]])[,12])[!cmax_iters] * unname(fwc@target@iters[13,"min",])[!cmax_iters])), is_true())
##
##
##})
##
##test_that("Test Jose bug with 0 landings and F bounds",{
##    # This used to stuff up as discards.ratio was recalculated
##    # With 0 landings leading to NA
##    data(ple4)
##    # make the biol
##    # srr
##    srr_model_name <- "bevholt"
##    srr <- as.FLSR(ple4, model=srr_model_name)
##    srr <- fmle(srr, control = list(trace=0))
##    srr_params <- as.FLQuant(params(srr))
##    srr_residuals <- exp(residuals(srr))
##    srr_residuals <- window(srr_residuals,end=2009)
##    srr_residuals[,"2009"] <- srr_residuals[,"2008"]
##    srr_residuals[] <- 1 # turn off residuals
##    srr_residuals_mult <- TRUE
##    srr_timelag <- 1
##    # catch -> fishery -> fisheries
##    #niters <- 500
##    #niters <- 10
##    niters <- 1 
##    #niters <- 1000
##    data(ple4)
##    ple4 <- propagate(ple4,niters)
##    biol <- as(ple4, 'FLBiol')
##    catch <- as(ple4, 'FLCatch')
##    catch@name <- "ple4 catch"
##    catch@desc <- "ple4 catch"
##    # Hack
##    fishery <- FLFishery(ple4=catch)
##    fishery@name <- "ple4 fishery"
##    fishery@desc <- "ple4 fishery"
##    fisheries <- FLFisheries(ple4=fishery)
##    fisheries@desc <- "ple4 fisheries"
##    # Fs
##    f <- list(ple4 = harvest(ple4))
##    f_spwn_flq <- harvest(ple4)
##    f_spwn_flq[] <- 0
##    f_spwn <- list(ple4 = f_spwn_flq)
##
##    minAge <- 2
##    maxAge <- 6
##
##    # 0 landings target, with min and max f
##    target <- data.frame(year=c(2,2,2), quantity = c('landings','f','f'), value = c(0.0,0.0,0.0), minAge=c(NA,minAge,minAge), maxAge=c(NA,maxAge,maxAge), season=1L)
##    fwc <- fwdControl(target=target, iter=niters)
##    # hack target
##    fwc@target@element[2,c("value","min")] <- c(NA, 0)
##    fwc@target@iters[2,c("value","min"),] <- c(NA, 0)
##    fwc@target@element[3,c("value","max")] <- c(NA, 1.5)
##    fwc@target@iters[3,c("value","max"),] <- c(NA, 1.5)
##
##    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
##
##    # Catch, landings and discards should be 0
##    expect_that(c(landings(out[["fisheries"]][[1]][[1]])[,2]), equals(fwc@target@iters[1,"value",]))
##    expect_that(c(catch(out[["fisheries"]][[1]][[1]])[,2]), equals(fwc@target@iters[1,"value",]))
##    expect_that(c(discards(out[["fisheries"]][[1]][[1]])[,2]), equals(fwc@target@iters[1,"value",]))
##    # F should be 0
##    expect_that(c(apply(out[["f"]][[1]][ac(minAge:maxAge),2],2:6,mean)), equals(0))
##})

