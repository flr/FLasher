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
    # Set up parameters for full test - lots of things needed
    flq <- random_FLQuant_generator()
    flb <- random_FLBiol_generator(fixed_dims = dim(flq))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    params_ricker <- FLQuant(rnorm(2), dimnames = list(params = c("a","b")))
    residuals_ricker <- FLQuant(rnorm(100), dimnames = list(year = 1:10, iter = 1:10))
    residuals_mult <- TRUE
    timelag <- 0
    f <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq))
    f <- lapply(f,abs)
    f_spwn <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq))
    f_spwn <- lapply(f_spwn,abs)
    fc <- dummy_fwdControl_generator(years = 1, niters = dim(n(flb))[6])

    # At last, a test
    # Full constructor with wrap
    out <- test_operatingModel_full_constructor(flfs, flb, "ricker", params_ricker, timelag, residuals_ricker, residuals_mult, f, f_spwn, fc)
    expect_that(out[["biol"]], is_identical_to(flb))
    expect_that(out[["fisheries"]], is_identical_to(flfs))
    expect_that(out[["f"]], is_identical_to(f))
    expect_that(out[["f_spwn"]], is_identical_to(f_spwn))
    expect_that(out[["ctrl"]], is_identical_to(fc))

    # Change dim of f, f_spwn and biol - dimension check
    new_dim <- dim(flq) + round(runif(6,min=1,max=3))
    new_flb <- random_FLBiol_generator(fixed_dims = new_dim)
    new_flfs <- random_FLFisheries_generator(fixed_dims = new_dim, min_fisheries=1, max_fisheries=1)
    new_f <- random_FLQuant_list_generator(max_elements=1, fixed_dims = new_dim)
    new_f <- lapply(new_f,abs)
    new_f_spwn <- random_FLQuant_list_generator(max_elements=1, fixed_dims = new_dim)
    new_f_spwn <- lapply(new_f_spwn,abs)
    expect_that(test_operatingModel_full_constructor(flfs, new_flb, "ricker", params_ricker, timelag, residuals_ricker, residuals_mult, f, f_spwn, fc), throws_error()) 
    expect_that(test_operatingModel_full_constructor(new_flfs, flb, "ricker", params_ricker, timelag, residuals_ricker, residuals_mult, f, f_spwn, fc), throws_error())
    expect_that(test_operatingModel_full_constructor(flfs, flb, "ricker", params_ricker, timelag, residuals_ricker, residuals_mult, new_f, f_spwn, fc), throws_error())
    expect_that(test_operatingModel_full_constructor(flfs, flb, "ricker", params_ricker, timelag, residuals_ricker, residuals_mult, f, new_f_spwn, fc), throws_error())
})


test_that("operatingModel project timestep",{
    # Set up parameters for full test - lots of things needed
    nseasons <- round(runif(1, min = 2, max = 4))
    flq <- random_FLQuant_generator(fixed_dims=c(NA,10,1,nseasons,1,NA)) # fix unit and area to be 1, need 10 years, and with seasons > 1 too
    #flq <- random_FLQuant_generator(fixed_dims=c(NA,10,1,1,1,NA)) # fix unit and area to be 1, need 10 years
    flb <- random_FLBiol_generator(fixed_dims = dim(flq))
    m(flb) <- random_FLQuant_generator(fixed_dims = dim(flq), sd = 0.1)
    # 2 fisheries
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    #ple4_sr_ricker <- as.FLSR(ple4,model="ricker")
    #params(ple4_sr_ricker) <- FLPar(abs(rnorm(2, sd = c(10,1e-6))))#FLPar(c(9.1626, 3.5459e-6))
    params_ricker <- FLQuant(abs(rnorm(2, sd = c(10,1e-6))),dimnames=list(params=c("a","b")))
    
    residuals_ricker <- FLQuant(NA, dimnames = dimnames(flq[1,]))
    residuals_ricker[] <- abs(rnorm(prod(dim(residuals_ricker))))
    residuals_mult <- TRUE
    f <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq), sd = 0.1)
    f <- lapply(f,abs)
    f_spwn <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq))
    f_spwn <- lapply(f_spwn,abs)
    fc <- dummy_fwdControl_generator(years = 1, niters = dim(n(flb))[6]) # doesn't do anything - just needed to make OM object in C
    # Project over random timesteps
    min_timestep <- round(runif(1,min=2,max=dim(n(flb))[2]/2 * dim(n(flb))[4])) 
    max_timestep <- round(runif(1,min=min_timestep+1,max=dim(n(flb))[2] * dim(n(flb))[4])) - 1
    timesteps <- min_timestep:max_timestep
    timelag <- round(runif(1,min=1,max=min_timestep)) # random timelag for recruitment

    # Run!
    out <- test_operatingModel_project_timestep(flfs, flb, "ricker", params_ricker, timelag, residuals_ricker, residuals_mult, f, f_spwn, fc, timesteps)
    # Check landings_n and discards_n are correct
    total_f <- f[[1]]
    for (i in 2:length(flfs)){
        total_f <- total_f + f[[i]]
    }
    z <- total_f + m(flb)
    # SSB calc
    f_portion <- f[[1]] * f_spwn[[1]] 
    for (i in 2:length(f)){
        f_portion <- f_portion + f[[i]] * f_spwn[[i]]
    }
    ssb <- quantSums(n(out[["biol"]]) * wt(out[["biol"]]) * fec(out[["biol"]]) * exp(-f_portion - m(out[["biol"]]) * spwn(out[["biol"]])))
    for (timestep in timesteps){
        year <-  floor((timestep-1) / dim(flq)[4] + 1)
        season <- (timestep-1) %%  dim(n(flb))[4]+ 1;
        next_year <- (timestep) / dim(n(flb))[4] + 1
        next_season <- (timestep) %%  dim(n(flb))[4]+ 1;
        ssb_year <- (timestep - timelag) / dim(n(flb))[4] + 1
        ssb_season <- (timestep - timelag) %%  dim(n(flb))[4]+ 1;
        for (i in 1:length(flfs)){
            cn <- (f[[i]] / z) * (1 - exp(-z)) * n(out[["biol"]])
            dr <- discards.n(flfs[[i]][[1]]) / (landings.n(flfs[[i]][[1]]) + discards.n(flfs[[i]][[1]]))
            expect_that((cn * (1 - dr))[,year,,season,,]@.Data, is_identical_to(landings.n(out[["fisheries"]][[i]][[1]])[,year,,season,,]@.Data))
            expect_that((cn * dr)[,year,,season,,]@.Data, is_identical_to(discards.n(out[["fisheries"]][[i]][[1]])[,year,,season,,]@.Data))
        }
        # check n is OK - not SRR
        # Depends if it is start of the year (plus group and recruitment)
        next_n <- n(flb)[,1,,1,,]
        next_n[] <- 0
        #next_n[2:dim(next_n)[1]] <- (n(out[["biol"]]) * exp(-z))[1:(dim(next_n)[1]-1),year,,season,,]
        # if start of year
        if (next_season == 1){
            next_n[2:dim(next_n)[1]] <- (n(out[["biol"]]) * exp(-z))[1:(dim(next_n)[1]-1),year,,season,,]
            next_n[dim(next_n)[1]] <- next_n[dim(next_n)[1]] + ((n(out[["biol"]]) * exp(-z))[dim(next_n)[1],year,,season,,])
        }
        if (next_season > 1){
            next_n[1:dim(next_n)[1]] <- (n(out[["biol"]]) * exp(-z))[1:dim(next_n)[1],year,,season,,]
        }

        # Just checking first area and unit now
        # And ages 2+
        expect_that(c(next_n[2:dim(next_n)[1],,1,,1,]), is_identical_to(c(n(out[["biol"]])[2:dim(next_n)[1],next_year,1,next_season,1,])))
        # With no time structure in SRR params, SRR is calculated every timestep
        rec <- params_ricker["a",] * ssb[1,ssb_year,1,ssb_season,1,] * exp(-params_ricker["b",] * ssb[1,ssb_year,1,ssb_season,1,]) * residuals_ricker[1,next_year,1,next_season]
        # if start of year only recruitment in age 1
        if (next_season == 1){
            expect_that(c(rec), equals(c(n(out[["biol"]])[1,next_year,1,next_season,1,])))
        }
        if (next_season > 1){
            expect_that(c(rec + next_n[1,]), equals(c(n(out[["biol"]])[1,next_year,1,next_season,1,])))
        }
    }

    # Test with time structure in SR params - only recruitment in first season
    params_ricker2 <- FLQuant(0,dimnames=list(params=c("a","b"), season = dimnames(flq)$season))
    params_ricker2[,,,1] <- abs(rnorm(2, sd = c(10,1e-6)))
    # check over timesteps with and without recruitment
    timesteps2 <- (dim(flq)[4]):(dim(flq)[4] * 2 + 1)
    timelag2 <- round(runif(1,min=1,max=min(timesteps2))) # random timelag for recruitment
    out2 <- test_operatingModel_project_timestep(flfs, flb, "ricker", params_ricker2, timelag2, residuals_ricker, residuals_mult, f, f_spwn, fc, timesteps2)
    ssb2 <- quantSums(n(out2[["biol"]]) * wt(out2[["biol"]]) * fec(out2[["biol"]]) * exp(-f_portion - m(out2[["biol"]]) * spwn(out2[["biol"]])))
    for (timestep in timesteps2){
        year <-  floor((timestep-1) / dim(flq)[4] + 1)
        season <- (timestep-1) %%  dim(n(flb))[4]+ 1;
        next_year <- (timestep) / dim(n(flb))[4] + 1
        next_season <- (timestep) %%  dim(n(flb))[4]+ 1;
        ssb_year <- (timestep - timelag2) / dim(n(flb))[4] + 1
        ssb_season <- (timestep - timelag2) %%  dim(n(flb))[4]+ 1;
        rec2 <- params_ricker2["a",1,,next_season] * ssb2[1,ssb_year,1,ssb_season,1,] * exp(-params_ricker2["b",1,,next_season] * ssb2[1,ssb_year,1,ssb_season,1,]) * residuals_ricker[1,next_year,1,next_season]
        expect_that(c(rec2), equals(c(n(out2[["biol"]])[1,next_year,1,next_season,1,])))
    }

})

test_that("operatingModel SSB methods", {
    #  Lots of things needed for test
    data(ple4)
    ple4_sr_ricker <- as.FLSR(ple4,model="ricker")
    # Force values, don't fit
    params(ple4_sr_ricker) <- FLPar(abs(rnorm(2, sd = c(10,1e-6))))#FLPar(c(9.1626, 3.5459e-6))
    params_ricker <- as.FLQuant(params(ple4_sr_ricker))
    flq <- random_FLQuant_generator(sd=1)
    flb <- random_FLBiol_generator(fixed_dims = dim(flq), sd = 1 )
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=5, sd=1)
    f <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq), sd=1)
    f <- lapply(f,abs)
    f_spwn <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq), sd=1)
    f_spwn <- lapply(f_spwn,abs)
    residuals_ricker <- FLQuant(rnorm(dim(flq)[2] * dim(flq)[6]), dimnames = list(year = 1:dim(flq)[2], iter = 1:dim(flq)[6]))
    residuals_mult <- TRUE
    timelag <- 0
    fc <- dummy_fwdControl_generator(years = 1, niters = dim(n(flb))[6])
    # SSB - FLQuant - lots of time steps
    f_portion <- f[[1]] * f_spwn[[1]]
    for (i in 2:length(f)){
        f_portion <- f_portion + f[[i]] * f_spwn[[i]]
    }
    ssb_in <- quantSums(n(flb) * wt(flb) * fec(flb) * exp(-f_portion - m(flb) * spwn(flb)))
    ssb_out <- test_operatingModel_SSB_FLQ(flfs, flb, 'ricker', params_ricker, timelag, residuals_ricker, residuals_mult, f, f_spwn, fc)
    expect_that(ssb_in@.Data, equals(ssb_out@.Data))
    # SSB - FLQuant - single single timestep - all iters
    timestep <- floor(runif(1, min=1, max = dim(flq)[2] * dim(flq)[4]))
    unit <- floor(runif(1, min=1, max = dim(flq)[3]))
    area <- floor(runif(1, min=1, max = dim(flq)[5]))
    ssb_out <- test_operatingModel_SSB_iters(flfs, flb, 'ricker', params_ricker, timelag, residuals_ricker, residuals_mult, f, f_spwn, timestep, unit, area, fc)
    year <-  floor((timestep-1) / dim(flq)[4] + 1)
    season <- (timestep-1) %% dim(flq)[4] + 1;
    expect_that((ssb_in[,year,unit,season,area])@.Data, equals(ssb_out@.Data))
    # SSB - numeric - single timestep - single iter
    iter <- floor(runif(1, min=1, max = dim(flq)[6]))
    ssb_out <- test_operatingModel_SSB_single_iter(flfs, flb, 'ricker', params_ricker, timelag, residuals_ricker, residuals_mult, f, f_spwn, timestep, unit, area, iter, fc)
    expect_that(c(ssb_in[,year,unit,season,area,iter]), equals(c(ssb_out)))

    # SSB non-conformable FLQuant iters, e.g. wt has only 1 iter, but n has many
    single_iter <- round(runif(1,min=1,max=dim(flq)[6]))
    flb2 <- flb
    wt(flb2) <- iter(wt(flb2),single_iter)
    fec(flb2) <- iter(fec(flb2),single_iter)
    m(flb2) <- iter(m(flb2),single_iter)
    ssb_out <- test_operatingModel_SSB_single_iter(flfs, flb2, 'ricker', params_ricker, timelag, residuals_ricker, residuals_mult, f, f_spwn, timestep, unit, area, iter, fc)
    ssb_in <- quantSums(n(flb2) * wt(flb2) * fec(flb2) * exp(-f_portion - m(flb2) * spwn(flb2)))
    expect_that(c(ssb_in[,year,unit,season,area,iter]), equals(c(ssb_out)))
    # SSB with year and season
    ssb_out <- test_operatingModel_SSB_single_iter_year_season(flfs, flb2, 'ricker', params_ricker, timelag, residuals_ricker, residuals_mult, f, f_spwn, year, unit, season, area, iter, fc)
    expect_that(c(ssb_in[,year,unit,season,area,iter]), equals(c(ssb_out)))
})


# Biomass, SSB and other abundance based targets need to change F in the previous timestep
test_that("operatingModel get_target_fmult_timestep",{
    # Make an FLFishery with X FLFishery objects. Each FLFishery has an FLCatch that catches the FLBiol
    # This is all a massive faff
    # Have at least 5 years and 10 ages, random number of seasons
    nyears <- 10
    flq <- random_FLQuant_generator(fixed_dim=c(10,nyears,NA,NA,NA,NA), sd=1)
    # Single FLBiol
    flb <- random_FLBiol_generator(fixed_dims = dim(flq), sd = 1 )
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=5, min_catches = 1, max_catches = 3, sd=1)
    # Each element of F is F from an FLCatch attacking the same FLBiol
    f <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq), sd=1)
    f <- lapply(f,abs)
    f_spwn <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq), sd=1)
    f_spwn <- lapply(f_spwn,abs)
    # SRR bits
    srr_model_name <- "ricker"
    params_sr <- as.FLQuant(FLPar(a=10, b = 4))
    residuals_sr <- flq[1,]
    residuals_mult <- TRUE
    srr_timelag <- 1

    fc <- dummy_fwdControl_generator(years = 1:6, niters = dim(n(flb))[6])
    fc@target@element$season <- round(runif(dim(fc@target@element)[1], min = 1, max = dim(flq)[4]))
    fc@target@element[2,"quantity"] <- "catch"
    fc@target@element[3,"quantity"] <- "ssb"
    fc@target@element[4,"quantity"] <- "biomass"

    target_no <- 1 # F
    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
    year <- fc@target@element[target_no,"year"]
    season <- fc@target@element[target_no,"season"]
    ctrl_timestep <- as.integer((year-1) * dim(flq)[4] + season)
    expect_that(fmult_timestep, is_identical_to(ctrl_timestep))

    target_no <- 2 # Catch
    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
    year <- fc@target@element[target_no,"year"]
    season <- fc@target@element[target_no,"season"]
    ctrl_timestep <- as.integer((year-1) * dim(flq)[4] + season)
    expect_that(fmult_timestep, is_identical_to(ctrl_timestep))

    target_no <- 3 # SSB
    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
    year <- fc@target@element[target_no,"year"]
    season <- fc@target@element[target_no,"season"]
    ctrl_timestep <- as.integer((year-1) * dim(flq)[4] + season)
    expect_that(fmult_timestep, is_identical_to(ctrl_timestep-1L))

    target_no <- 4 # Biomass
    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
    year <- fc@target@element[target_no,"year"]
    season <- fc@target@element[target_no,"season"]
    ctrl_timestep <- as.integer((year-1) * dim(flq)[4] + season)
    expect_that(fmult_timestep, is_identical_to(ctrl_timestep-1L))
})

test_that("operatingModel target values and eval_target method", {
    # Make an FLFishery with X FLFishery objects. Each FLFishery has an FLCatch that catches the FLBiol
    # This is all a massive faff
    # Have at least 5 years and 10 ages, random number of seasons
    nyears <- 10
    flq <- random_FLQuant_generator(fixed_dim=c(10,nyears,NA,NA,NA,NA), sd=1)
    # Single FLBiol
    flb <- random_FLBiol_generator(fixed_dims = dim(flq), sd = 1 )
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=5, min_catches = 1, max_catches = 3, sd=1)
    # Each element of F is F from an FLCatch attacking the same FLBiol
    f <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq), sd=1)
    f <- lapply(f,abs)
    f_spwn <- random_FLQuant_list_generator(min_elements=length(flfs), max_elements=length(flfs), fixed_dims = dim(flq), sd=1)
    f_spwn <- lapply(f_spwn,abs)
    # SRR bits
    srr_model_name <- "ricker"
    params_sr <- as.FLQuant(FLPar(a=10, b = 4))
    residuals_sr <- flq[1,]
    residuals_mult <- TRUE
    srr_timelag <- 1
    # Catch no has be to 1 at the moment - we only have 1 biol and we need to think about how to link a biol to a catch
    catch_no <- 1 # round(runif(n=1,min=1,max=min(unlist(lapply(flfs, length))))) # which catch no of each fishery
    fishery_no <- round(runif(1, min=1, max=length(flfs)))

    #------------ Test age range indices --------------------
    fc <- dummy_fwdControl_generator(years = 1:6, niters = dim(n(flb))[6])
    fc@target@element[1,c("minAge","maxAge")] <- c(1L,5L)
    fc@target@element[2,c("minAge","maxAge")] <- c(2L,3L)
    fc@target@element[3,c("minAge","maxAge")] <- c(20L,30L)
    fc@target@element[4,c("minAge","maxAge")] <- c(2L,30L)
    fc@target@element[5,c("minAge","maxAge")] <- c(0L,30L)
    fc@target@element[6,c("minAge","maxAge")] <- c(as.integer(NA),as.integer(NA))

    age_indices <- test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 1)
    expect_that(age_indices, equals(unname(unlist(fc@target@element[1,c("minAge","maxAge")])) - as.numeric(dimnames(n(flb))[[1]][1])))
    age_indices <- test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 2)
    expect_that(age_indices, equals(unname(unlist(fc@target@element[2,c("minAge","maxAge")])) - as.numeric(dimnames(n(flb))[[1]][1])))
    # outside range - error
    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 3), throws_error())
    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 4), throws_error())
    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 5), throws_error())
    # range not set
    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 6), throws_error())


    #---------- Test target methods ---------------
    # Get the target values from the C++ code
    # Nothing to do with the target quantities in the control here - just testing internal methods
    targets <- test_operating_model_targets(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, fishery_no, catch_no, 1)
    # fbar of a single catch 
    expect_that(targets[["fbar_catch"]], equals(apply(f[[fishery_no]][fc@target@element[1,"minAge"]:fc@target@element[1,"maxAge"],],2:6,mean)))
    # a different age range
    targets <- test_operating_model_targets(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, fishery_no, catch_no, 2)
    # fbar of a single catch 
    expect_that(targets[["fbar_catch"]], equals(apply(f[[fishery_no]][fc@target@element[2,"minAge"]:fc@target@element[2,"maxAge"],],2:6,mean)))

    # total fbar of all fisheries
    f2 <- lapply(f, function(x) 
        apply(x[fc@target@element[2,"minAge"]:fc@target@element[2,"maxAge"],],2:6,mean)
    )
    f_total <- f2[[1]]
    f_total[] <- 0
    for (i in 1:length(flfs)){
        f_total <- f_total + f2[[i]]
    }
    expect_that(targets[["fbar"]]@.Data, is_identical_to(f_total@.Data))

    # catches - single
    expect_that(targets[["catches_catch"]]@.Data , equals(catch(flfs[[fishery_no]][[catch_no]])@.Data))
    # catches - total
    catches_total <- catch(flfs[[fishery_no]][[catch_no]])
    catches_total[] <- 0
    for (i in 1:length(flfs)){
        catches_total <- catches_total + catch(flfs[[i]][[catch_no]])
    }
    expect_that(targets[["catches"]]@.Data, equals(catches_total@.Data))

    # SSB - f portion from all catches
    f_portion <- f_spwn[[1]] * f[[1]]
    for (i in 2:length(f)){
        f_portion <- f_portion + (f_spwn[[i]] * f[[i]])
    }
    ssb_in <- quantSums(n(flb) * wt(flb) * fec(flb) * exp(-f_portion - m(flb) * spwn(flb)))
    expect_that(ssb_in@.Data, equals(targets[["ssb"]]@.Data))

    # Biomass
    expect_that(targets[["biomass"]]@.Data, equals(quantSums(n(flb) * wt(flb))@.Data))

    #-------- Test eval_target by target number----------
    # Set up fc to test the output
    # By defaulf fishery = NA
    fishery_no <- round(runif(1, min=1,max=length(f)))
    fc <- dummy_fwdControl_generator(years = 1:dim(flb@n)[2], niters = dim(n(flb))[6])
    fc@target@element$minAge <- 2
    fc@target@element$maxAge <- 5
    f2 <- lapply(f, function(x) 
        apply(x[fc@target@element[1,"minAge"]:fc@target@element[1,"maxAge"],],2:6,mean)
    )
    f_total <- f2[[1]]
    f_total[] <- 0
    for (i in 1:length(flfs)){
        f_total <- f_total + f2[[i]]
    }
    # No fishery
    fc@target@element[1,"quantity"] <- "f"
    fc@target@element[2,"quantity"] <- "catch"
    fc@target@element[3,"quantity"] <- "ssb"
    fc@target@element[4,"quantity"] <- "biomass"

    # Testing eval_target - is the correct quantity calculation called
    min_iter <- 1
    max_iter <- dim(fc@target@iters)[3]

    # fbar
    target_no <- 1 
    fout <- test_operatingModel_eval_target(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
    expect_that(fout@.Data, equals(f_total@.Data))
    # catch
    target_no <- 2 
    cout <- test_operatingModel_eval_target(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
    expect_that(cout@.Data, equals(catches_total@.Data))
    # ssb 
    target_no <- 3 
    ssb_out <- test_operatingModel_eval_target(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
    expect_that(ssb_out@.Data, equals(ssb_in@.Data))
    # biomass
    target_no <- 4 
    biomass_out <- test_operatingModel_eval_target(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
    biomass_in <- quantSums(n(flb) * wt(flb))
    expect_that(biomass_out@.Data, equals(biomass_in@.Data))

    #----------- Test calc_target_value  ----------
    # Not a relative target - should just the values in the target_iter slot
    target_no <- 1 # f target
    fout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
    expect_that(fout, is_identical_to(unname(c(fc@target@iters[target_no,"value",]))))
    target_no <- 2 # catch target
    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
    expect_that(cout, is_identical_to(unname(c(fc@target@iters[target_no,"value",]))))
    target_no <- 3 # ssb target
    ssb_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
    expect_that(ssb_out, is_identical_to(unname(c(fc@target@iters[target_no,"value",]))))
    target_no <- 4 # biomass target
    biomass_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
    expect_that(biomass_out, is_identical_to(unname(c(fc@target@iters[target_no,"value",]))))

    # Add a relative f target
    fc@target@element[3,"quantity"] <- "f"
    fc@target@element[3,"relYear"] <- 1
    fc@target@element[3,"relSeason"] <- round(runif(1,min=1,max=dim(flq)[4]))
    target_no <- 3
    fout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
    expect_that(unname(c(f_total[1,1,1,fc@target@element[3,"relSeason"],1,]) * fc@target@iters[target_no,"value",]), equals(fout))
    # Add a relative catch target
    fc@target[4,"quantity"] <- "catch"
    fc@target[4,"relYear"] <- 2
    fc@target[4,"relSeason"] <- round(runif(1,min=1,max=dim(flq)[4]))
    target_no <- 4
    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
    expect_that(unname(c(catches_total[1,2,1,fc@target@element[4,"relSeason"],1,]) * fc@target@iters[target_no,"value",]), equals(cout))
    # Add a relative ssb target
    fc@target[5,"quantity"] <- "ssb"
    fc@target[5,"relYear"] <- 3
    fc@target[5,"relSeason"] <- round(runif(1,min=1,max=dim(flq)[4]))
    target_no <- 5
    ssb_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
    expect_that(unname(c(ssb_in[1,3,1,fc@target@element[5,"relSeason"],1,]) * fc@target@iters[target_no,"value",]), equals(ssb_out))
    # Relative biomass target
    fc@target[6,"quantity"] <- "biomass"
    fc@target[6,"relYear"] <- 4
    fc@target[6,"relSeason"] <- round(runif(1,min=1,max=dim(flq)[4]))
    target_no <- 6
    biomass_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
    expect_that(unname(c(biomass_in[1,4,1,fc@target@element[6,"relSeason"],1,]) * fc@target@iters[target_no,"value",]), equals(biomass_out))

    # Min and max values
    # Set a Min bound to start with
    fc@target[7,"quantity"] <- "catch"
    fc@target@iters[7,"value",] <- NA
    fc@target@iters[7,"max",] <- NA
    fc@target@iters[7,"min",] <- rnorm(dim(fc@target@iters)[3], mean = 1000)
    # Set Catch in that year to be 0 so that catch should be 0 - but will be constrained by min catch
    flfs_min <- flfs
    for (i in 1:length(flfs_min)){
        flfs_min[[i]][[1]]@landings.n[,fc@target@element[7,"year"],,fc@target@element[7,"season"],,] <- 0
        flfs_min[[i]][[1]]@discards.n[,fc@target@element[7,"year"],,fc@target@element[7,"season"],,] <- 0
    }
    flfs_min@desc <- flfs@desc # eck
    cout <- test_operatingModel_calc_target_value(flfs_min, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 7) 
    expect_that(cout, is_identical_to(unname(fc@target@iters[7,"min",])))
    # Try a Max value
    fc@target[8,"quantity"] <- "catch"
    fc@target@iters[8,"value",] <- NA
    fc@target@iters[8,"min",] <- NA
    fc@target@iters[8,"max",] <- rnorm(dim(fc@target@iters)[3], mean = 1000)
    # Set Catch in that year to be larger than the catch in that year so that catch should be very big - but will be constrained by max catch
    flfs_max <- flfs
    for (i in 1:length(flfs_max)){
        flfs_max[[i]][[1]]@landings.n[,fc@target@element[8,"year"],,fc@target@element[8,"season"],,] <- fc@target@iters[8,"max",] + 1
        flfs_max[[i]][[1]]@discards.n[,fc@target@element[8,"year"],,fc@target@element[8,"season"],,] <- fc@target@iters[8,"max",] + 1
    }
    flfs_max@desc <- flfs@desc # eck
    cout <- test_operatingModel_calc_target_value(flfs_max, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 8) 
    expect_that(cout, is_identical_to(unname(fc@target@iters[8,"max",])))

    # Try Min and Max values
    # some iters are min, some are max, some are OK
    catches_total <- catch(flfs[[1]][[1]])
    catches_total[] <- 0
    for (i in 1:length(flfs)){
        catches_total <- catches_total + catch(flfs[[i]][[1]])
    }
    current_catches <- c(catches_total[,fc@target@element[7,"year"],1,fc@target@element[7,"season"],1,])
    min_iters <- as.logical(round(runif(dim(flq)[6], min = 0, max = 1)))
    max_iters <- !min_iters
    max_catches <- current_catches
    min_catches <- current_catches
    min_catches[min_iters] <- min_catches[min_iters] * 1.01
    max_catches[min_iters] <- max_catches[min_iters] * 1.1
    min_catches[max_iters] <- min_catches[max_iters] * 0.9
    max_catches[max_iters] <- max_catches[max_iters] * 0.99
    fc@target@element[7,"quantity"] <- "catch"
    fc@target@iters[7,"value",] <- NA
    fc@target@iters[7,"min",] <- min_catches
    fc@target@iters[7,"max",] <- max_catches
    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 7) 
    expect_that(cout[min_iters], is_identical_to(min_catches[min_iters]))
    expect_that(cout[max_iters], is_identical_to(max_catches[max_iters]))
    
    # Try Min and Max relative values
    # Catch cannot change by more than 5% in year 4
    fc@target@element[8,"relYear"] <- 4L
    fc@target@element[8,"relSeason"] <- 1L
    fc@target@element[8,"quantity"] <- "catch"
    fc@target@iters[8,"value",] <- NA
    fc@target@iters[8,"min",] <- 0.95
    fc@target@iters[8,"max",] <- 1.05
    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 8) 
    current_catches4 <- c(catches_total[,fc@target@element[4,"year"],1,fc@target@element[4,"season"],1,])
    current_catches8 <- c(catches_total[,fc@target@element[8,"year"],1,fc@target@element[8,"season"],1,])
    min_lim <- current_catches8 >= current_catches4
    expect_that(all((cout[min_lim] / current_catches4[min_lim]) >= 0.95), is_true())
    max_lim <- current_catches8 < current_catches4
    expect_that(all((cout[max_lim] / current_catches4[max_lim]) <= 1.05), is_true())
})


test_that("Test projection with individual targets",{
    # Test operating model.
    # Need FLFisheries, FLBiol, SRR, F and Fspwn
    data(ple4)
    # make the biol
    # srr
    srr_model_name <- "bevholt"
    srr <- as.FLSR(ple4, model=srr_model_name)
    srr <- fmle(srr, control = list(trace=0))
    srr_params <- as.FLQuant(params(srr))
    srr_residuals <- exp(residuals(srr))
    srr_residuals <- window(srr_residuals,end=2009)
    srr_residuals[,"2009"] <- srr_residuals[,"2008"]
    srr_residuals[] <- 1 # turn off residuals
    srr_residuals_mult <- TRUE
    srr_timelag <- 1
    niters <- 500
    data(ple4)
    ple4 <- propagate(ple4,niters)
    biol <- as(ple4, 'FLBiol')
    catch <- as(ple4, 'FLCatch')
    catch@name <- "ple4 catch"
    catch@desc <- "ple4 catch"
    # Hack
    fishery <- FLFishery(ple4=catch)
    fishery@name <- "ple4 fishery"
    fishery@desc <- "ple4 fishery"
    fisheries <- FLFisheries(ple4=fishery)
    fisheries@desc <- "ple4 fisheries"
    # Fs
    f <- list(ple4 = harvest(ple4))
    f_spwn_flq <- harvest(ple4)
    f_spwn_flq[] <- 0
    f_spwn <- list(ple4 = f_spwn_flq)

    # Add some noise on biol n and harvest
    n(biol) <- n(biol) * rlnorm(prod(dim(n(biol))), sd = 0.1)
    f[[1]] <- f[[1]] * rlnorm(prod(dim(f[[1]])), sd = 0.1)

    # A simple target object - 1 year, 1 target
    minAge <- 2
    maxAge <- 6
    # F target
    target <- data.frame(year=2, quantity = 'f', value = 0.5, minAge=minAge, maxAge=maxAge, season=1L)
    fwc <- fwdControl(target=target, iter=niters)
    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1)
    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
    # equals?
    expect_that(c(apply(out[["f"]][[1]][ac(minAge:maxAge),2] ,2:6,mean)), equals(unname(fwc@target@iters[1,"value",])))

    # Catch target
    target <- data.frame(year=2, quantity = 'catch', value = 100000, minAge=minAge, maxAge=maxAge, season=1L)
    fwc <- fwdControl(target=target, iter=niters)
    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(100000), sd = 0.1)
    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
    expect_that(c(catch(out[["fisheries"]][[1]][[1]])[,2]), equals(unname(fwc@target@iters[1,"value",])))

    # Biomass target
    target <- data.frame(year=2, quantity = 'biomass', value = 300000, minAge=minAge, maxAge=maxAge, season=1L)
    fwc <- fwdControl(target=target, iter=niters)
    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1)
    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
    expect_that(c(quantSums(n(out[["biol"]]) * wt(out[["biol"]]))[,2]), equals(unname(fwc@target@iters[1,"value",])))

    # SSB target
    target <- data.frame(year=2, quantity = 'ssb', value = 300000, minAge=minAge, maxAge=maxAge, season=1L)
    fwc <- fwdControl(target=target, iter=niters)
    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1)
    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
    ssb_out <- quantSums(n(out[["biol"]]) * wt(out[["biol"]]) * fec(out[["biol"]]) * exp(-out[["f_spwn"]][[1]] * out[["f"]][[1]] - m(out[["biol"]]) * spwn(out[["biol"]])))
    expect_that(c(ssb_out[,2]), equals(unname(fwc@target@iters[1,"value",])))
})

test_that("Test projection with really complicated target",{
    # Test operating model.
    # Need FLFisheries, FLBiol, SRR, F and Fspwn
    data(ple4)
    # make the biol
    # srr
    srr_model_name <- "bevholt"
    srr <- as.FLSR(ple4, model=srr_model_name)
    srr <- fmle(srr, control = list(trace=0))
    srr_params <- as.FLQuant(params(srr))
    srr_residuals <- exp(residuals(srr))
    srr_residuals <- window(srr_residuals,end=2009)
    srr_residuals[,"2009"] <- srr_residuals[,"2008"]
    srr_residuals[] <- 1 # turn off residuals
    srr_residuals_mult <- TRUE
    srr_timelag <- 1
    niters <- 500
    data(ple4)
    ple4 <- propagate(ple4,niters)
    biol <- as(ple4, 'FLBiol')
    catch <- as(ple4, 'FLCatch')
    catch@name <- "ple4 catch"
    catch@desc <- "ple4 catch"
    # Hack
    fishery <- FLFishery(ple4=catch)
    fishery@name <- "ple4 fishery"
    fishery@desc <- "ple4 fishery"
    fisheries <- FLFisheries(ple4=fishery)
    fisheries@desc <- "ple4 fisheries"
    # Fs
    f <- list(ple4 = harvest(ple4))
    f_spwn_flq <- harvest(ple4)
    f_spwn_flq[] <- 0
    f_spwn <- list(ple4 = f_spwn_flq)

    # Add some noise on biol n and harvest
    set.seed(1)
    n(biol) <- n(biol) * rlnorm(prod(dim(n(biol))), sd = 0.1)
    f[[1]] <- f[[1]] * rlnorm(prod(dim(f[[1]])), sd = 0.1)
    minAge <- 2
    maxAge <- 6

    target <- data.frame(year = 2, quantity = 'f', value = 0.5, minAge = 2, maxAge = 6, season = 1L)
    target <- rbind(target,
        # Absolute values
        data.frame(year = 3, quantity = 'catch', value = 100000, minAge = NA, maxAge = NA, season = 1L),
        data.frame(year = 5, quantity = 'biomass', value = 300000, minAge = NA, maxAge = NA, season = 1L), # affects F in year 4
        data.frame(year = 6, quantity = 'ssb', value = 300000, minAge = NA, maxAge = NA, season = 1L), # affects F in year 5
        # Max targets
        data.frame(year = 7, quantity = 'catch', value = 100000, minAge = NA, maxAge = NA, season = 1L), # Catch target
        data.frame(year = 7, quantity = 'f', value = 1, minAge = 2, maxAge = 6, season = 1L), # but maximum F limits it - need to hack this line after constructor
        # Min target
        data.frame(year = 8, quantity = 'f', value = 0.2, minAge = 2, maxAge = 6, season = 1L), # F target
        data.frame(year = 8, quantity = 'catch', value = 100000, minAge = NA, maxAge = NA, season = 1L), # but minimum catch limits it - need to hack this line after constructor
        # Relative target
        data.frame(year = 9, quantity = 'catch', value = 0.5, minAge = NA, maxAge = NA, season = 1L), # Need to hack this after constructor
        # Relative max target
        data.frame(year = 11, quantity = 'f', value = 0.5, minAge = 2, maxAge = 6, season = 1L), # F target
        data.frame(year = 11, quantity = 'catch', value = 1.15, minAge = NA, maxAge = NA, season = 1L), # But catch cannot increase more than 15% from previous yearNeed to hack this after constructor
        # Relative min target
        data.frame(year = 13, quantity = 'f', value = 0.3, minAge = 2, maxAge = 6, season = 1L), # F target
        data.frame(year = 13, quantity = 'catch', value = 0.85, minAge = NA, maxAge = NA, season = 1L) # But catch cannot increase more than 15% from previous yearNeed to hack this after constructor
    )

    fwc <- fwdControl(target=target, iter=niters)
    # Hack min and max values of f target
    fwc@target@element
    fwc@target@element[6,c("value","max")] <- c(NA,0.3)
    fwc@target@element[8,c("value","min")] <- c(NA,100000)
    fwc@target@element[9,c("relYear","relSeason")] <- c(8L,1L)
    fwc@target@element[11,c("value","max","relYear","relSeason")] <- c(NA, 1.15, 10L, 1L)
    fwc@target@element[13,c("value","min","relYear","relSeason")] <- c(NA, 0.85, 12L, 1L)

    # Fix iter values
    fwc@target@iters[1,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1) # f
    fwc@target@iters[2,"value",] <- rlnorm(niters, mean = log(100000), sd = 0.1) # sd = 1 results in bad iters - good for testing
    fwc@target@iters[3,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1) # biomass
    fwc@target@iters[4,"value",] <- rlnorm(niters, mean = log(300000), sd = 0.1) # ssb
    fwc@target@iters[5,"value",] <- rlnorm(niters, mean = log(100000), sd = 0.1) # catch for f max test
    fwc@target@iters[6,"value",] <- NA # fbar has max only
    fwc@target@iters[6,"max",] <- rlnorm(niters, mean = log(0.35), sd = 0.01)
    fwc@target@iters[7,"value",] <- rlnorm(niters, mean = log(0.2), sd = 0.1) 
    fwc@target@iters[8,"value",] <- NA # fbar has max only
    fwc@target@iters[8,"min",] <- rlnorm(niters, mean = log(100000), sd = 0.1) 
    fwc@target@iters[9,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1) 
    fwc@target@iters[10,"value",] <- rlnorm(niters, mean = log(0.5), sd = 0.1) 
    fwc@target@iters[11,"value",] <- NA
    fwc@target@iters[11,"max",] <- 1.15
    fwc@target@iters[12,"value",] <- rlnorm(niters, mean = log(0.3), sd = 0.1) 
    fwc@target@iters[13,"value",] <- NA
    fwc@target@iters[13,"min",] <- 0.85 

    # Call test run with (something like this will go in fwd() method)
    out <- test_operatingModel_run(fisheries, biol, srr_model_name, srr_params, srr_residuals, srr_residuals_mult, srr_timelag, f, f_spwn, fwc)
    ssb_out <- quantSums(n(out[["biol"]]) * wt(out[["biol"]]) * fec(out[["biol"]]) * exp(-out[["f_spwn"]][[1]] * out[["f"]][[1]] - m(out[["biol"]]) * spwn(out[["biol"]])))

    # Test output
    # Absolute targets
    expect_that(c(apply(out[["f"]][[1]][ac(minAge:maxAge),2] ,2:6,mean)), equals(unname(fwc@target@iters[1,"value",]))) # abs ssb
    expect_that(c(catch(out[["fisheries"]][[1]][[1]])[,3]), equals(unname(fwc@target@iters[2,"value",]))) # abs catch
    expect_that(c(quantSums(n(out[["biol"]]) * wt(out[["biol"]]))[,5]), equals(unname(fwc@target@iters[3,"value",]))) # abs biomass
    expect_that(c(ssb_out[,6]), equals(unname(fwc@target@iters[4,"value",])))

    # Max target - bit tricky for comparison - cannot use < > as differences are sometimes +-1e-16 
    # so need tolerances
    tol <- .Machine$double.eps ^ 0.5   
    # Find which Catches have not been hit
    fmax_iters <- abs((c(catch(out[["fisheries"]][[1]][[1]])[,7]) / unname(fwc@target@iters[5,"value",])) - 1) > tol
    # Check that fmax has been hit for these iters
    expect_that(all(abs((c(apply(out[["f"]][[1]][ac(minAge:maxAge),7] ,2:6,mean))[fmax_iters] / unname(fwc@target@iters[6,"max",])[fmax_iters]) -1) < tol), is_true())
    # And that F is less than Fmax for the iters where catches have been hit
    expect_that(all(c(apply(out[["f"]][[1]][ac(minAge:maxAge),7] ,2:6,mean))[!fmax_iters] < unname(fwc@target@iters[6,"max",])[!fmax_iters]), is_true())

    # Min test
    # Iters where F has not been hit
    cmin_iters <- abs((c(apply(out[["f"]][[1]][ac(minAge:maxAge),8] ,2:6,mean)) / unname(fwc@target@iters[7,"value",]))-1) > tol
    # cmin has been hit for these iters
    expect_that(all(abs((c(catch(out[["fisheries"]][[1]][[1]])[,8])[cmin_iters] / unname(fwc@target@iters[8,"min",])[cmin_iters])-1) < tol), is_true())
    # And that Catch is greater than Cmin for the other iters (where original f target was hit)
    expect_that(all(c(catch(out[["fisheries"]][[1]][[1]])[,8])[!cmin_iters] > unname(fwc@target@iters[8,"min",])[!cmin_iters]), is_true())

    # Relative test
    expect_that(c(catch(out[["fisheries"]][[1]][[1]])[,8]) * unname(fwc@target@iters[9,"value",]), equals(c(catch(out[["fisheries"]][[1]][[1]])[,9])))

    # Relative max test
    # Which iters have been limited by cmax, i.e. f target not hit
    cmax_iters <- abs((c(apply(out[["f"]][[1]][ac(minAge:maxAge),11] ,2:6,mean)) / unname(fwc@target@iters[10,"value",]))-1) > tol
    # Check that these iters have hit the limit
    # Catch should be at limit
    expect_that(all( abs((c(catch(out[["fisheries"]][[1]][[1]])[,11])[cmax_iters] / (c(catch(out[["fisheries"]][[1]][[1]])[,10])[cmax_iters] * unname(fwc@target@iters[11,"max",])[cmax_iters]))-1) < tol), is_true())
    # Unlimited iters should have catches less than max
    expect_that(all(c(catch(out[["fisheries"]][[1]][[1]])[,11])[!cmax_iters] < (c(catch(out[["fisheries"]][[1]][[1]])[,10])[!cmax_iters] * unname(fwc@target@iters[11,"max",])[!cmax_iters])), is_true())

    # Relative min test
    # Which iters have been limited by cmax, i.e. f target not hit
    cmax_iters <- abs(( c(apply(out[["f"]][[1]][ac(minAge:maxAge),13] ,2:6,mean)) / unname(fwc@target@iters[12,"value",]))-1) > tol
    # These iters should be at catch limit
    expect_that(all( abs((c(catch(out[["fisheries"]][[1]][[1]])[,13])[cmax_iters] / (c(catch(out[["fisheries"]][[1]][[1]])[,12])[cmax_iters] * unname(fwc@target@iters[13,"min",])[cmax_iters]))-1) < tol), is_true())
    # Other iters should have catches greater than catch limit
    expect_that(all(c(catch(out[["fisheries"]][[1]][[1]])[,13])[!cmax_iters] > (c(catch(out[["fisheries"]][[1]][[1]])[,12])[!cmax_iters] * unname(fwc@target@iters[13,"min",])[!cmax_iters])), is_true())


})

