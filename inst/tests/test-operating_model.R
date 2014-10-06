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

#test_that("euclid_norm",{
#    size_x <- runif(1, min=5, max = 100) 
#    x <- rnorm(size_x)
#    expect_that(test_euclid_norm(x), equals(sqrt(sum(x^2))))
#})
#
#test_that("Newton-Raphson tests",{
#    # Simplest test - solution is 2 or -2/3
#    max_iters <- 50
#    max_limit <- 100
#    tolerance <- 1e-12
#    initial <- rnorm(1, sd=1)
#    fit <- test_NR1(initial, max_iters, max_limit, tolerance)
#    expect_that(((fit$x - 2) < tolerance) | ((fit$x - (-2/3)) < tolerance), is_true())
#    expect_that(fit$out, is_identical_to(0L))
#    # Check max iters
#    fit <- test_NR1(-10, 2, max_limit, tolerance)
#    expect_that(fit$out, is_identical_to(1L))
#    # Check max limit
#    fit <- test_NR1(200, max_iters, max_limit, tolerance)
#    expect_that(fit$out, is_identical_to(2L))
#    tolerance2 <- 1
#    fit <- test_NR1(50, max_iters, max_limit, tolerance2)
#    expect_that(((fit$x - 2) < tolerance2) | ((fit$x - (-2/3)) < tolerance2), is_true())
#    # 2D test 
#    max_iters <- 50
#    max_limit <- 1e9 # Crank up the limit - it's OK for this example
#    tolerance <- 1e-12
#    initial <- abs(rnorm(2, sd=2))
#    fit <- test_NR2(initial, max_iters, max_limit, tolerance)
#    y1 <- fit$x[1]^2 + fit$x[2]^2 - 4
#    y2 <- fit$x[1]^2 - fit$x[2] + 1
#    expect_that(sqrt(sum(c(y1,y2)^2)) < tolerance, is_true())
#    expect_that(fit$out, is_identical_to(0L))
#})
#

context("Implementation of operatingModel")

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
    flq <- random_FLQuant_generator(fixed_dims=c(NA,10,1,NA,1,NA)) # fix unit and area to be 1, need 10 years
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
        next_n <- n(flb)[,1,,1,,]
        next_n[] <- 0
        next_n[2:dim(next_n)[1]] <- (n(out[["biol"]]) * exp(-z))[1:(dim(next_n)[1]-1),year,,season,,]
        next_n[dim(next_n)[1]] <- next_n[dim(next_n)[1]] + ((n(out[["biol"]]) * exp(-z))[dim(next_n)[1],year,,season,,])
        # Just checking first area and unit now
        # And ages 2+
        expect_that(c(next_n[2:dim(next_n)[1],,1,,1,]), is_identical_to(c(n(out[["biol"]])[2:dim(next_n)[1],next_year,1,next_season,1,])))
        # With no time structure in SRR params, SRR is calculated every timestep
        rec <- params_ricker["a",] * ssb[1,ssb_year,1,ssb_season,1,] * exp(-params_ricker["b",] * ssb[1,ssb_year,1,ssb_season,1,]) * residuals_ricker[1,next_year,1,next_season]
        expect_that(c(rec), equals(c(n(out[["biol"]])[1,next_year,1,next_season,1,])))
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


#test_that("operatingModel project_timestep", {
#    # Get the components
#    data(ple4)
#    srr_model_name <- "ricker"
#    ple4_sr <- fmle(as.FLSR(ple4,model=srr_model_name), control  = list(trace=0))
#    params_sr <- as.FLQuant(params(ple4_sr))
#    # Have at least 5 years
#    flq <- random_FLQuant_generator(fixed_dim=c(NA,5,1,NA,1,NA), sd=1)
#    flb <- random_FLBiol_generator(fixed_dims = dim(flq), sd = 1 )
#    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=1, max_fisheries=1, sd=1)
#    f <- random_FLQuant_list_generator(max_elements=1, fixed_dims = dim(flq), sd=1)
#    f <- lapply(f,abs)
#    f_spwn <- random_FLQuant_list_generator(max_elements=1, fixed_dims = dim(flq), sd=1)
#    f_spwn <- lapply(f_spwn,abs)
#    residuals_sr <- abs(FLQuant(rnorm(prod(dim(flq)[-1])), dimnames = list(year = 1:dim(flq)[2], unit = 1:dim(flq)[3], season = 1:dim(flq)[4], area = 1:dim(flq)[5], iter = 1:dim(flq)[6])))
#    residuals_mult <- TRUE
#    timelag <- 1
#    fc <- dummy_fwdControl_generator(years = 1, niters = dim(n(flb))[6])
#
#    # Test seasonal projection - ignore SRR
#    nseason <- dim(flq)[4]
#    timestep <- round(runif(1, min=1,max=(nseason * dim(flq)[2])-1))
#    om_out <- test_operating_model_project(flfs, flb, srr_model_name, params_sr, timelag, residuals_sr, residuals_mult, f, f_spwn, timestep, fc)
#    om_R <- simple_fisheries_project(flfs, flb, ple4_sr, f, f_spwn, residuals_sr, residuals_mult, timestep)
#    year =  (timestep-1) / nseason + 1; 
#    season = (timestep-1) %% nseason + 1;
#    next_year =  (timestep+1-1) / nseason + 1; 
#    next_season = (timestep+1-1) %% nseason + 1;
#    # check the catches etc
#    expect_that(catch.n(om_out[["fisheries"]][[1]][[1]])[,year,1,season,1,]@.Data, is_identical_to(catch.n(om_R[["flfs"]][[1]][[1]])[,year,1,season,1,]@.Data))
#    expect_that(landings.n(om_out[["fisheries"]][[1]][[1]])[,year,1,season,1,]@.Data, is_identical_to(landings.n(om_R[["flfs"]][[1]][[1]])[,year,1,season,1,]@.Data))
#    expect_that(discards.n(om_out[["fisheries"]][[1]][[1]])[,year,1,season,1,]@.Data, is_identical_to( discards.n(om_R[["flfs"]][[1]][[1]])[,year,1,season,1,]@.Data))
#    # Check n (not recs)
#    expect_that(om_out[["biol"]]@n[-1,next_year,1,next_season,1,]@.Data, is_identical_to( om_R[["flb"]]@n[-1,next_year,1,next_season,1,]@.Data))
#
#    # Check with annual SRR
#    # Test annual model to start with
#    flq <- random_FLQuant_generator(fixed_dim=c(NA,5,1,1,1,NA),sd=1)
#    flb <- random_FLBiol_generator(fixed_dims = dim(flq), sd = 1 )
#    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=1, max_fisheries=1, sd=1)
#    f <- random_FLQuant_list_generator(max_elements=1, fixed_dims = dim(flq), sd=1)
#    units(f[[1]]) <- "f"
#    f <- lapply(f,abs)
#    f_spwn <- random_FLQuant_list_generator(max_elements=1, fixed_dims = dim(flq), sd=1)
#    units(f_spwn[[1]]) <- "prop"
#    f_spwn <- lapply(f_spwn,abs)
#    residuals_sr <- abs(FLQuant(rnorm(prod(dim(flq)[-1])), dimnames = list(year = 1:dim(flq)[2], unit = 1:dim(flq)[3], season = 1:dim(flq)[4], area = 1:dim(flq)[5], iter = 1:dim(flq)[6])))
#    #residuals_sr <- abs(FLQuant(1, dimnames = list(year = 1:dim(flq)[2], unit = 1:dim(flq)[3], season = 1:dim(flq)[4], area = 1:dim(flq)[5], iter = 1:dim(flq)[6])))
#    residuals_mult <- TRUE
#    fc <- dummy_fwdControl_generator(years = 1, niters = dim(n(flb))[6])
#
#    timesteps <- 1:(dim(flq)[2]-1)
#    project_r <- list(flfs = flfs, flb=flb)
#
#    for (timestep in timesteps){
#        project_r <- simple_fisheries_project(project_r[["flfs"]], project_r[["flb"]], ple4_sr, f, f_spwn, residuals_sr, residuals_mult, timestep)
#    }
#    # Check recruitment in R method
#    project_c <- list(fisheries = flfs, biol=flb)
#    for (timestep in timesteps){
#        project_c <- test_operating_model_project(project_c[["fisheries"]], project_c[["biol"]], "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, timestep, fc)
#    }
#    # Check biol timestep up to max timesteps + 1
#    expect_that((project_c[["biol"]]), equals((project_r[["flb"]])))
#
#
#    # Check fisheries catch timestep up to max timesteps
#    flfs_c <- window(project_c[["fisheries"]][[1]][[1]], start=timesteps[1], end=max(timesteps))
#    flfs_r <- window(project_r[["flfs"]][[1]][[1]], start=timesteps[1], end=max(timesteps))
#    # expect_that(flfs_c, equals(flfs_r)) # cannot check units as c++ code does not adapt units
#    expect_that(flfs_c@discards.n@.Data, equals(flfs_r@discards.n@.Data))
#    expect_that(flfs_c@landings.n@.Data, equals(flfs_r@landings.n@.Data))
#
#    # What about SRR on a different timestep
#})
#
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
#    fc@target$season <- round(runif(dim(fc@target)[1], min = 1, max = dim(flq)[4]))
#    fc@target[2,"quantity"] <- "catch"
#    fc@target[3,"quantity"] <- "ssb"
#    fc@target[4,"quantity"] <- "biomass"
#
#    target_no <- 1
#    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    year <- fc@target[target_no,"year"]
#    season <- fc@target[target_no,"season"]
#    ctrl_timestep <- as.integer((year-1) * dim(flq)[4] + season)
#    expect_that(fmult_timestep, is_identical_to(ctrl_timestep))
#
#    target_no <- 2
#    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    year <- fc@target[target_no,"year"]
#    season <- fc@target[target_no,"season"]
#    ctrl_timestep <- as.integer((year-1) * dim(flq)[4] + season)
#    expect_that(fmult_timestep, is_identical_to(ctrl_timestep))
#
#    target_no <- 3
#    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    year <- fc@target[target_no,"year"]
#    season <- fc@target[target_no,"season"]
#    ctrl_timestep <- as.integer((year-1) * dim(flq)[4] + season)
#    expect_that(fmult_timestep, is_identical_to(ctrl_timestep-1L))
#
#    target_no <- 4
#    fmult_timestep <- test_operatingModel_get_target_fmult_timestep(flfs, flb, srr_model_name, params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, target_no)
#    year <- fc@target[target_no,"year"]
#    season <- fc@target[target_no,"season"]
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
#    fc@target[1,c("min_age","max_age")] <- c(1L,5L)
#    fc@target[2,c("min_age","max_age")] <- c(2L,3L)
#    fc@target[3,c("min_age","max_age")] <- c(20L,30L)
#    fc@target[4,c("min_age","max_age")] <- c(2L,30L)
#    fc@target[5,c("min_age","max_age")] <- c(0L,30L)
#
#    age_indices <- test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 1)
#    expect_that(age_indices, equals(unname(unlist(fc@target[1,c("min_age","max_age")])) - as.numeric(dimnames(n(flb))[[1]][1])))
#    age_indices <- test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 2)
#    expect_that(age_indices, equals(unname(unlist(fc@target[2,c("min_age","max_age")])) - as.numeric(dimnames(n(flb))[[1]][1])))
#    # outside range - error
#    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 3), throws_error())
#    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 4), throws_error())
#    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 5), throws_error())
#    # range not set
#    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 6), throws_error())
#    expect_that(test_operating_model_get_target_age_range_indices(flfs, flb, "ricker", params_sr, srr_timelag, residuals_sr, residuals_mult, f, f_spwn, fc, 7), throws_error())
#
#
#    #---------- Test target methods ---------------
#    # Get the target values from the C++ code
#    # Nothing to do with the target quantities in the control here - just testing internal methods
#    targets <- test_operating_model_targets(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, fishery_no, catch_no, 1)
#    # fbar of a single catch 
#    expect_that(targets[["fbar_catch"]], equals(apply(f[[fishery_no]][fc@target[1,"min_age"]:fc@target[1,"max_age"],],2:6,mean)))
#    # a different age range
#    targets <- test_operating_model_targets(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, fishery_no, catch_no, 2)
#    # fbar of a single catch 
#    expect_that(targets[["fbar_catch"]], equals(apply(f[[fishery_no]][fc@target[2,"min_age"]:fc@target[2,"max_age"],],2:6,mean)))
#
#    # total fbar of all fisheries
#    f2 <- lapply(f, function(x) 
#        apply(x[fc@target[2,"min_age"]:fc@target[2,"max_age"],],2:6,mean)
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
#    fc@target$min_age <- 2
#    fc@target$max_age <- 5
#    f2 <- lapply(f, function(x) 
#        apply(x[fc@target[1,"min_age"]:fc@target[1,"max_age"],],2:6,mean)
#    )
#    f_total <- f2[[1]]
#    f_total[] <- 0
#    for (i in 1:length(flfs)){
#        f_total <- f_total + f2[[i]]
#    }
#    # No fishery
#    fc@target[1,"quantity"] <- "f"
#    fc@target[2,"quantity"] <- "catch"
#    fc@target[3,"quantity"] <- "ssb"
#    fc@target[4,"quantity"] <- "biomass"
#
#    # Testing eval_target
#    min_iter <- 1
#    max_iter <- dim(fc@target_iters)[3]
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
#    expect_that(fout, is_identical_to(unname(c(fc@target_iters[target_no,"value",]))))
#    target_no <- 2 # catch target
#    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(cout, is_identical_to(unname(c(fc@target_iters[target_no,"value",]))))
#    target_no <- 3 # ssb target
#    ssb_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(ssb_out, is_identical_to(unname(c(fc@target_iters[target_no,"value",]))))
#    target_no <- 4 # biomass target
#    biomass_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(biomass_out, is_identical_to(unname(c(fc@target_iters[target_no,"value",]))))
#
#    # Add a relative f target
#    fc@target[3,"quantity"] <- "f"
#    fc@target[3,"rel_year"] <- 1
#    fc@target[3,"rel_season"] <- round(runif(1,min=1,max=dim(flq)[4]))
#    target_no <- 3
#    fout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(unname(c(f_total[1,1,1,fc@target[3,"rel_season"],1,]) * fc@target_iters[target_no,"value",]), equals(fout))
#    # Add a relative catch target
#    fc@target[4,"quantity"] <- "catch"
#    fc@target[4,"rel_year"] <- 2
#    fc@target[4,"rel_season"] <- round(runif(1,min=1,max=dim(flq)[4]))
#    target_no <- 4
#    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(unname(c(catches_total[1,2,1,fc@target[4,"rel_season"],1,]) * fc@target_iters[target_no,"value",]), equals(cout))
#    # Add a relative ssb target
#    fc@target[5,"quantity"] <- "ssb"
#    fc@target[5,"rel_year"] <- 3
#    fc@target[5,"rel_season"] <- round(runif(1,min=1,max=dim(flq)[4]))
#    target_no <- 5
#    ssb_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(unname(c(ssb_in[1,3,1,fc@target[5,"rel_season"],1,]) * fc@target_iters[target_no,"value",]), equals(ssb_out))
#    # Relative biomass target
#    fc@target[6,"quantity"] <- "biomass"
#    fc@target[6,"rel_year"] <- 4
#    fc@target[6,"rel_season"] <- round(runif(1,min=1,max=dim(flq)[4]))
#    target_no <- 6
#    biomass_out <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, target_no) 
#    expect_that(unname(c(biomass_in[1,4,1,fc@target[6,"rel_season"],1,]) * fc@target_iters[target_no,"value",]), equals(biomass_out))
#
#    # Min and max values
#    # Set a Min bound to start with
#    fc@target[7,"quantity"] <- "catch"
#    fc@target_iters[7,"value",] <- NA
#    fc@target_iters[7,"max",] <- NA
#    fc@target_iters[7,"min",] <- rnorm(dim(fc@target_iters)[3], mean = 1000)
#    # Set Catch in that year to be 0 so that catch should be 0 - but will be constrained by min catch
#    flfs_min <- flfs
#    for (i in 1:length(flfs_min)){
#        flfs_min[[i]][[1]]@landings.n[,fc@target[7,"year"],,fc@target[7,"season"],,] <- 0
#        flfs_min[[i]][[1]]@discards.n[,fc@target[7,"year"],,fc@target[7,"season"],,] <- 0
#    }
#    flfs_min@desc <- flfs@desc # eck
#    cout <- test_operatingModel_calc_target_value(flfs_min, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 7) 
#    expect_that(cout, is_identical_to(unname(fc@target_iters[7,"min",])))
#    # Try a Max value
#    fc@target[8,"quantity"] <- "catch"
#    fc@target_iters[8,"value",] <- NA
#    fc@target_iters[8,"min",] <- NA
#    fc@target_iters[8,"max",] <- rnorm(dim(fc@target_iters)[3], mean = 1000)
#    # Set Catch in that year to be larger than the catch in that year so that catch should be very big - but will be constrained by max catch
#    flfs_max <- flfs
#    for (i in 1:length(flfs_max)){
#        flfs_max[[i]][[1]]@landings.n[,fc@target[8,"year"],,fc@target[8,"season"],,] <- fc@target_iters[8,"max",] + 1
#        flfs_max[[i]][[1]]@discards.n[,fc@target[8,"year"],,fc@target[8,"season"],,] <- fc@target_iters[8,"max",] + 1
#    }
#    flfs_max@desc <- flfs@desc # eck
#    cout <- test_operatingModel_calc_target_value(flfs_max, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 8) 
#    expect_that(cout, is_identical_to(unname(fc@target_iters[8,"max",])))
#
#    # Try Min and Max values
#    # some iters are min, some are max, some are OK
#    catches_total <- catch(flfs[[1]][[1]])
#    catches_total[] <- 0
#    for (i in 1:length(flfs)){
#        catches_total <- catches_total + catch(flfs[[i]][[1]])
#    }
#    current_catches <- c(catches_total[,fc@target[7,"year"],1,fc@target[7,"season"],1,])
#    min_iters <- as.logical(round(runif(dim(flq)[6], min = 0, max = 1)))
#    max_iters <- !min_iters
#    max_catches <- current_catches
#    min_catches <- current_catches
#    min_catches[min_iters] <- min_catches[min_iters] * 1.01
#    max_catches[min_iters] <- max_catches[min_iters] * 1.1
#    min_catches[max_iters] <- min_catches[max_iters] * 0.9
#    max_catches[max_iters] <- max_catches[max_iters] * 0.99
#    fc@target[7,"quantity"] <- "catch"
#    fc@target_iters[7,"value",] <- NA
#    fc@target_iters[7,"min",] <- min_catches
#    fc@target_iters[7,"max",] <- max_catches
#    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 7) 
#    expect_that(cout[min_iters], is_identical_to(min_catches[min_iters]))
#    expect_that(cout[max_iters], is_identical_to(max_catches[max_iters]))
#    
#    # Try Min and Max relative values
#    # Catch cannot change by more than 5% in year 4
#    fc@target[8,"rel_year"] <- 4L
#    fc@target[8,"rel_season"] <- 1L
#    fc@target[8,"quantity"] <- "catch"
#    fc@target_iters[8,"value",] <- NA
#    fc@target_iters[8,"min",] <- 0.95
#    fc@target_iters[8,"max",] <- 1.05
#    cout <- test_operatingModel_calc_target_value(flfs, flb, "ricker", params_sr, 1, residuals_sr, residuals_mult, f, f_spwn, fc, 8) 
#    current_catches4 <- c(catches_total[,fc@target[4,"year"],1,fc@target[4,"season"],1,])
#    current_catches8 <- c(catches_total[,fc@target[8,"year"],1,fc@target[8,"season"],1,])
#    min_lim <- current_catches8 >= current_catches4
#    expect_that(all((cout[min_lim] / current_catches4[min_lim]) >= 0.95), is_true())
#    max_lim <- current_catches8 < current_catches4
#    expect_that(all((cout[max_lim] / current_catches4[max_lim]) <= 1.05), is_true())
#})
#
#
