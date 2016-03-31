context("Implementation of fwdSR")

test_that("fwdSR constructors and wrap",{
    # Make an FLSR to help
    data(ple4)
    ricker <- fmle(as.FLSR(ple4,model="ricker"), control  = list(trace=0))
    params <- as.FLQuant(params(ricker))
    residuals <- FLQuant(rnorm(100), dimnames = list(year = 1:10, iter = 1:10))
    residuals_mult <- TRUE
    # Empty constructor - does nothing - shouldn't fail
    expect_error(!test_fwdSR_empty_constructor())
    # Simple wrap
    sr_op <- test_fwdSR_constructor_wrap("ricker", params, residuals, residuals_mult)
    expect_identical(c(sr_op[["params"]]), c(params))
    expect_identical(sr_op[["residuals"]], residuals)
    expect_identical(sr_op[["residuals_mult"]], residuals_mult)
})

test_that("fwdSR accessor methods",{
    data(ple4)
    ricker <- fmle(as.FLSR(ple4,model="ricker"), control  = list(trace=0))
    ricker_params <- as.FLQuant(params(ricker))
    residuals <- FLQuant(rnorm(100), dimnames = list(year = 1:10, iter = 1:10))
    residuals_mult <- TRUE
    constant <- fmle(as.FLSR(ple4,model="geomean"), control  = list(trace=0), method="Brent", lower=0, upper=1e12)
    constant_params <- as.FLQuant(params(constant))
    # test get_nparams
    nparams <- test_fwdSR_get_nparams("ricker", ricker_params, residuals, residuals_mult)
    expect_identical(nparams, unname(dim(ricker_params)[1]))
    nparams <- test_fwdSR_get_nparams("constant", constant_params, residuals, residuals_mult)
    expect_identical(nparams, unname(dim(constant_params)[1]))
})

test_that("fwdSR get_params",{
    residuals <- FLQuant(rnorm(100), dimnames = list(year = 1:10, iter = 1:10))
    residuals_mult <- TRUE
    # No structure, just params
    params_in <- FLQuant(rnorm(2), dim=c(2,1,1,1,1,1))
    # Asking for any dim
    params_indices <- round(runif(6,min=1,max=10))
    params_out <- test_fwdSR_get_params("ricker", params_in, residuals, residuals_mult, params_indices)
    expect_identical(params_out, c(params_in))
    # With year
    params_in <- FLQuant(rnorm(2*10), dim=c(2,10,1,1,1,1))
    params_indices <- round(runif(6,min=1,max=10))
    params_out <- test_fwdSR_get_params("ricker", params_in, residuals, residuals_mult, params_indices)
    expect_identical(params_out, c(params_in[,params_indices[1],]))
    # With Seasons and Units
    # Units 1 and 2 are M/F recruiting in Season 1
    # Units 3 and 4 are M/F recruiting in Season 3
    params_in <- FLQuant(NA, dim=c(2,1,4,4,1,1)) 
    params_in[,,c(1,2),1,] <- rnorm(4)
    params_in[,,c(3,4),3,] <- rnorm(4)
    # Asking for Year 10, Season 1, Unit 1
    params_indices <- c(10,1,1,1,1)
    params_out <- test_fwdSR_get_params("ricker", params_in, residuals, residuals_mult, params_indices)
    expect_identical(params_out, c(params_in[,1,1,1,1,1]))
    # Asking for Year 10, Season 1, Unit 2
    params_indices <- c(10,2,1,1,1)
    params_out <- test_fwdSR_get_params("ricker", params_in, residuals, residuals_mult, params_indices)
    expect_identical(params_out, c(params_in[,1,2,1,1,1]))
    # Asking for Year 10, Season 1, Unit 3
    params_indices <- c(10,3,1,1,1)
    params_out <- test_fwdSR_get_params("ricker", params_in, residuals, residuals_mult, params_indices)
    expect_true(all(is.na(params_out)))
    expect_identical(params_out, c(params_in[,1,3,1,1,1]))
    # Asking for Year 10, Season 1, Unit 4
    params_indices <- c(10,4,1,1,1)
    params_out <- test_fwdSR_get_params("ricker", params_in, residuals, residuals_mult, params_indices)
    expect_true(all(is.na(params_out)))
    expect_identical(params_out, c(params_in[,1,4,1,1,1]))
    # Asking for Year 10, Season 3, Unit 1
    params_indices <- c(10,1,3,1,1)
    params_out <- test_fwdSR_get_params("ricker", params_in, residuals, residuals_mult, params_indices)
    expect_true(all(is.na(params_out)))
    expect_identical(params_out, c(params_in[,1,1,3,1,1]))
    # Asking for Year 10, Season 3, Unit 2
    params_indices <- c(10,2,3,1,1)
    params_out <- test_fwdSR_get_params("ricker", params_in, residuals, residuals_mult, params_indices)
    expect_true(all(is.na(params_out)))
    expect_identical(params_out, c(params_in[,1,2,3,1,1]))
    # Asking for Year 10, Season 3, Unit 3
    params_indices <- c(10,3,3,1,1)
    params_out <- test_fwdSR_get_params("ricker", params_in, residuals, residuals_mult, params_indices)
    expect_identical(params_out, c(params_in[,1,3,3,1,1]))
    # Asking for Year 10, Season 3, Unit 4
    params_indices <- c(10,4,3,1,1)
    params_out <- test_fwdSR_get_params("ricker", params_in, residuals, residuals_mult, params_indices)
    expect_identical(params_out, c(params_in[,1,4,3,1,1]))
})

test_that("fwdSR eval",{
    data(ple4)
    ricker <- fmle(as.FLSR(ple4,model="ricker"), control  = list(trace=0))
    ricker_params <- as.FLQuant(params(ricker))
    bevholt <- fmle(as.FLSR(ple4,model="bevholt"), control  = list(trace=0))
    bevholt_params <- as.FLQuant(params(bevholt))
    constant <- fmle(as.FLSR(ple4,model="geomean"), control  = list(trace=0), method="Brent", lower=0, upper=1e12)
    constant_params <- as.FLQuant(params(constant))
    residuals <- FLQuant(rnorm(100), dimnames = list(year = 1:10, iter = 1:10))
    residuals_mult <- TRUE
    # Eval with parameters not disaggregated and not asking for disaggregated
    srp <- c(ssb(ple4))[1]
    param_indices <- c(1,1,1,1,1)
    rec <- test_fwdSR_eval("ricker", ricker_params, residuals, residuals_mult, srp, param_indices)
    expect_equal(rec, c(predict(ricker, ssb=FLQuant(srp))))
    rec <- test_fwdSR_eval("bevholt", bevholt_params, residuals, residuals_mult, srp, param_indices)
    expect_equal(rec, c(predict(bevholt, ssb=FLQuant(srp))))
    rec <- test_fwdSR_eval("constant", constant_params, residuals, residuals_mult, srp, param_indices)
    expect_equal(rec, c(predict(constant, ssb=FLQuant(srp))))
    # Eval with parameters not disaggregated but asking for disaggregated params
    srp <- c(ssb(ple4))[1]
    param_indices <- round(runif(5, min=2, max=10))
    rec <- test_fwdSR_eval("ricker", ricker_params, residuals, residuals_mult, srp, param_indices)
    expect_equal(rec, c(predict(ricker, ssb=FLQuant(srp))))
    rec <- test_fwdSR_eval("bevholt", bevholt_params, residuals, residuals_mult, srp, param_indices)
    expect_equal(rec, c(predict(bevholt, ssb=FLQuant(srp))))
    rec <- test_fwdSR_eval("constant", constant_params, residuals, residuals_mult, srp, param_indices)
    expect_equal(rec, c(predict(constant, ssb=FLQuant(srp))))
    # Eval with disaggregated parameters
    params_size <- round(runif(5, min=2, max=10))
    # Make the params - ugly
    params_multi <- FLPar(abs(rnorm(prod(params_size)*2, sd=c(params(ricker)))), dimnames=list(params = c("a","b"), year=1:params_size[1], unit=1:params_size[2], season=1:params_size[3], area=1:params_size[4], iter=1:params_size[5]))
    units(params_multi) <- "NA" # just to tidy up
    # Asking for disagg parameters within the range
    params_indices <- round(runif(5,min=1,max=params_size))
    rec <- test_fwdSR_eval("ricker", params_multi, residuals, residuals_mult, srp, params_indices)
    rec_in <- params_multi["a",params_indices[1], params_indices[2], params_indices[3], params_indices[4], params_indices[5]] * srp * exp(-params_multi["b", params_indices[1], params_indices[2], params_indices[3], params_indices[4], params_indices[5]] * srp)
    expect_equal(rec, c(rec_in))
    # Asking for disagg parameters outside the range - should pick index = 1 for that dim
    params_indices <- round(runif(5,min=1,max=params_size))
    params_outside <- round(runif(1,min=1,max=5)) # which dim is too big
    # Ask for param outside max range  
    params_indices[params_outside] <- dim(params_multi)[params_outside+1] + 1
    rec <- test_fwdSR_eval("ricker", params_multi, residuals, residuals_mult, srp, params_indices)
    # Set the too big one to 1
    params_indices[(params_indices > dim(params_multi)[-1])] <- 1
    rec_in <- params_multi["a",params_indices[1], params_indices[2], params_indices[3], params_indices[4], params_indices[5]] * srp * exp(-params_multi["b", params_indices[1], params_indices[2], params_indices[3], params_indices[4], params_indices[5]] * srp)
    expect_equal(rec, c(rec_in))
})

test_that("fwdSR copy constructor and assignment operator",{
    data(ple4)
    ricker <- fmle(as.FLSR(ple4,model="ricker"), control  = list(trace=0))
    params <- as.FLQuant(params(ricker))
    residuals <- FLQuant(rnorm(100), dimnames = list(year = 1:10, iter = 1:10))
    residuals_mult <- TRUE
    srp <- c(ssb(ple4))[1]
    # Copy constructor
    srs <- test_fwdSR_copy_constructor("ricker", params, residuals, residuals_mult, srp)
    expect_that(srs[[1]], is_identical_to(srs[[2]]))
    expect_that(srs[[3]], is_identical_to(srs[[4]]))
    # Assignment operator
    srs <- test_fwdSR_assignment_operator("ricker", params, residuals, residuals_mult, srp)
    expect_that(srs[[1]], is_identical_to(srs[[2]]))
    expect_that(srs[[3]], is_identical_to(srs[[4]]))
})


test_that("fwdSR predict_recruitment",{
    data(ple4)
    ricker <- fmle(as.FLSR(ple4,model="ricker"), control  = list(trace=0))
    params <- as.FLQuant(params(ricker))
    srp <- ssb(ricker)
    rickerR <- function(srp, a, b){
        return(a * srp *(exp(-b * srp)))
    }
    # Set up some parameters and SRP for the tests
    # With seasonal timestep and multiple iterations
    niters <- 10
    srp_in <- FLQuant(NA, dim=c(1,dim(srp)[2],1,4), iter=niters)
    srp_in[] <- abs(rnorm(prod(dim(srp_in)), mean=c(ssb(ricker)), sd=10))
    params_season <- FLQuant(NA, dim=c(2,dim(srp)[2],1,4), dimnames=list(param = c("a","b")), iter=niters)
    params_season["a",] <- abs(rnorm(prod(dim(params_season["a",])), mean=c(params["a",]), sd=1))
    params_season["b",] <- abs(rnorm(prod(dim(params_season["b",])), mean=c(params["b",]), sd=1e-6))
    # Set seasons 1, 3, 4 to 0. Only reproduces in season 2
    season <- 2
    params_season[,,,!(1:4 %in% 2)] <- 0
    # residuals must be same dim as SRP - even if seasonal
    residuals_mult <- srp_in
    residuals_mult[] <- abs(rnorm(prod(dim(residuals_mult)), mean=1, sd = 0.1))
    residuals_mult[,,,!(1:4 %in% 2)] <- 0
    residuals_add <- srp_in
    residuals_add[] <- abs(rnorm(prod(dim(residuals_mult)), mean=0, sd = 0.1))
    residuals_add[,,,!(1:4 %in% 2)] <- 0
    # time lag between SRP and recruitment - 1 year = 4 timesteps
    srp_timelag <- 4

    # Annual timestep.
    # All SRP years, single iter. One param year, single iter (same params for all years)
    iter <- round(runif(1, min=1, max=niters))
    rec_timestep <- round(runif(1, min=2, max=dim(srp_in)[2]))
    srp_timestep <- rec_timestep - (srp_timelag / 4) # Annual not seasonal timestep here
    rec <- rickerR(srp_in[,,,season,,iter], c(params_season["a",srp_timestep,,season,,iter]), c(params_season["b",srp_timestep,,season,,iter]))
    recm <- rec * residuals_mult[,,,season,,iter]
    reca <- rec + residuals_add[,,,season,,iter]
    # params and residuals start at beginning as passing in all SRP years
    rec_outm <- test_fwdSR_predict_recruitment("ricker", params_season[,srp_timestep,,season,,iter], residuals_mult[,,,season,,iter], TRUE, srp_in[,,,season,,iter], c(1,1,1,1,1), c(1,1,1,1,1))
    rec_outa <- test_fwdSR_predict_recruitment("ricker", params_season[,srp_timestep,,season,,iter], residuals_add[,,,season,,iter], FALSE, srp_in[,,,season,,iter], c(1,1,1,1,1), c(1,1,1,1,1))
    expect_equal(c(rec_outm), c(recm))
    expect_equal(c(rec_outa), c(reca))

    # Annual timestep.
    # All SSB years, all iters. One param year, single iter (same params for all years)
    rec <- FLQuant(NA, dim=c(1,dim(srp_in)[2]), iter=niters)
    for (i in 1:niters){
        iter(rec,i) <- rickerR(srp_in[,,,season,,i], c(params_season["a",srp_timestep,,season,,iter]), c(params_season["b",srp_timestep,,season,,iter]))
    }
    recm <- rec * residuals_mult[,,,season,]
    reca <- rec + residuals_add[,,,season,]
    rec_outm <- test_fwdSR_predict_recruitment("ricker", params_season[,srp_timestep,,season,,iter], residuals_mult[,,,season,], TRUE, srp_in[,,,season,,], c(1,1,1,1,1), c(1,1,1,1,1))
    rec_outa <- test_fwdSR_predict_recruitment("ricker", params_season[,srp_timestep,,season,,iter], residuals_add[,,,season,], FALSE, srp_in[,,,season,,], c(1,1,1,1,1), c(1,1,1,1,1))
    expect_equal(c(rec_outm), c(recm))
    expect_equal(c(rec_outa), c(reca))

    # Annual timestep.
    # All SSB years, all iters. One param year, all iters
    rec <- FLQuant(NA, dim=c(1,dim(srp_in)[2]), iter=niters)
    for (i in 1:niters){
        iter(rec,i) <- rickerR(srp_in[,,,season,,i], c(params_season["a",srp_timestep,,season,,i]), c(params_season["b",srp_timestep,,season,,i]))
    }
    recm <- rec * residuals_mult[,,,season,]
    reca <- rec + residuals_add[,,,season,]
    rec_outm <- test_fwdSR_predict_recruitment("ricker", params_season[,srp_timestep,,season,], residuals_mult[,,,season,], TRUE, srp_in[,,,season,], c(1,1,1,1,1), c(1,1,1,1,1))
    rec_outa <- test_fwdSR_predict_recruitment("ricker", params_season[,srp_timestep,,season,], residuals_add[,,,season,], FALSE, srp_in[,,,season,], c(1,1,1,1,1), c(1,1,1,1,1))
    expect_equal(c(rec_outm), c(recm))
    expect_equal(c(rec_outa), c(reca))

    # Annual timestep.
    # All SSB years, all iters. All params years, all iters - either subset params, or use indices
    rec <- FLQuant(NA, dim=c(1,dim(srp_in)[2]), iter=niters)
    for (i in 1:niters){
        iter(rec,i) <- rickerR(srp_in[,,,season,,i], c(params_season["a",,,season,,i]), c(params_season["b",,,season,,i]))
    }
    recm <- rec * residuals_mult[,,,season,]
    reca <- rec + residuals_add[,,,season,]
    rec_outm <- test_fwdSR_predict_recruitment("ricker", params_season, residuals_mult, TRUE, srp_in[,,,season,], c(1,1,season,1,1), c(1,1,season,1,1))
    rec_outa <- test_fwdSR_predict_recruitment("ricker", params_season, residuals_add, FALSE, srp_in[,,,season,], c(1,1,season,1,1), c(1,1,season,1,1))
    expect_equal(c(rec_outm), c(recm))
    expect_equal(c(rec_outa), c(reca))

    # Annual timestep.
    # Subset SSB years, all iters. All params years (subset through initial indices), all iters 
    rec_timestep1 <- round(runif(1, min=2, max=dim(srp_in)[2]/2))
    rec_timestep2 <- round(runif(1, min=rec_timestep1+1, max=dim(srp_in)[2]))
    rec_timestep_subset <- rec_timestep1:rec_timestep2
    srp_timestep_subset <- rec_timestep_subset - (srp_timelag / 4) # 4 seasons but here an annual timestep
    rec <- FLQuant(NA, dim=c(1,length(rec_timestep_subset)), iter=niters)
    for (i in 1:niters){
        iter(rec,i) <- rickerR(srp_in[,srp_timestep_subset,,season,,i], c(params_season["a",srp_timestep_subset,,season,,i]), c(params_season["b",srp_timestep_subset,,season,,i]))
    }
    recm <- rec * residuals_mult[,rec_timestep_subset,,season,]
    reca <- rec + residuals_add[,rec_timestep_subset,,season,]
    # Pass complete objects but subset params and residuals through the initial indices
    rec_outm <- test_fwdSR_predict_recruitment("ricker", params_season[,,,season,], residuals_mult[,,,season,], TRUE, srp_in[,srp_timestep_subset,,season,], c(srp_timestep_subset[1],1,1,1,1), c(rec_timestep_subset[1],1,1,1,1))
    rec_outa <- test_fwdSR_predict_recruitment("ricker", params_season[,,,season,], residuals_add[,,,season,], FALSE, srp_in[,srp_timestep_subset,,season,], c(srp_timestep_subset[1],1,1,1,1), c(rec_timestep_subset[1],1,1,1,1))
    expect_equal(c(rec_outm), c(recm))
    expect_equal(c(rec_outa), c(reca))

    # Seasonal timestep 
    # One year, all seasons, one iter for SRP and params - subset params and residuals using initial indices
    iter <- round(runif(1, min=1, max=niters))
    rec_year <- round(runif(1, min=2, max=dim(srp_in)[2])) 
    srp_year <- rec_year - (srp_timelag / 4) 
    rec <- rickerR(srp_in[,srp_year,,,,iter], c(params_season["a",srp_year,,,,iter]), c(params_season["b",srp_year,,,,iter]))
    recm <- rec * residuals_mult[,rec_year,,,,iter]
    reca <- rec + residuals_add[,rec_year,,,,iter]
    rec_outm <- test_fwdSR_predict_recruitment("ricker", params_season, residuals_mult, TRUE, srp_in[,srp_year,,,,iter], c(srp_year,1,1,1,iter), c(rec_year,1,1,1,iter))
    expect_equal(c(rec_outm), c(recm))
    rec_outa <- test_fwdSR_predict_recruitment("ricker", params_season, residuals_add, FALSE, srp_in[,srp_year,,,,iter], c(srp_year,1,1,1,iter), c(rec_year,1,1,1,iter))
    expect_equal(c(rec_outa), c(reca))

    # Seasonal timestep 
    # One year, all seasons, all iters for SRP.  One year, all seasons, one iter for params
    rec <- srp_in[,1] # Just make rec the right size
    rec[] <- NA
    for (i in 1:niters){
        iter(rec,i) <- rickerR(srp_in[,srp_year,,,,i], c(params_season["a",srp_year,,,,iter]), c(params_season["b",srp_year,,,,iter]))
    }
    recm <- rec * residuals_mult[,rec_year,]
    reca <- rec + residuals_add[,rec_year,]
    rec_outm <- test_fwdSR_predict_recruitment("ricker", params_season[,,,,,iter], residuals_mult, TRUE, srp_in[,srp_year,], c(srp_year,1,1,1,1), c(rec_year,1,1,1,1))
    expect_equal(c(rec_outm), c(recm))
    rec_outa <- test_fwdSR_predict_recruitment("ricker", params_season[,,,,,iter], residuals_add, FALSE, srp_in[,srp_year,], c(srp_year,1,1,1,1), c(rec_year,1,1,1,1))
    expect_equal(c(rec_outa), c(reca))
    
    # Seasonal timestep 
    # All year, all seasons, all iters for SRP.  All year, all seasons, all iters for params
    rec <- srp_in
    rec[] <- NA
    for (i in 1:niters){
        for (season_count in 1:dim(srp_in)[4]){
            rec[,,,season_count,,i] <- rickerR(srp_in[,,,season_count,,i], c(params_season["a",,,season_count,,i]), c(params_season["b",,,season_count,,i]))
        }
    }
    recm <- rec * residuals_mult
    reca <- rec + residuals_add
    rec_outm <- test_fwdSR_predict_recruitment("ricker", params_season, residuals_mult, TRUE, srp_in, c(1,1,1,1,1), c(1,1,1,1,1))
    rec_outa <- test_fwdSR_predict_recruitment("ricker", params_season, residuals_add, FALSE, srp_in, c(1,1,1,1,1), c(1,1,1,1,1))
    expect_equal(c(rec_outm), c(recm))
    expect_equal(c(rec_outa), c(reca))

    # Seasonal timestep 
    # Subset year, all seasons, all iters for SRP.  All year (subset through initial indices), all seasons, all iters for params
    rec_year1 <- round(runif(1, min=2, max=dim(srp_in)[2]/2))
    rec_year2 <- round(runif(1, min=rec_year1+1, max=dim(srp_in)[2]))
    rec_year_subset <- rec_year1:rec_year2
    srp_year_subset <- rec_year_subset - (srp_timelag / 4) # 4 seasons but here an annual year
    rec <- FLQuant(NA, dim=c(1,length(rec_year_subset),1,dim(srp_in)[4]), iter=niters)
    for (i in 1:niters){
        for (season_count in 1:dim(srp_in)[4]){
            rec[,,,season_count,,i] <- rickerR(srp_in[,srp_year_subset,,season_count,,i], c(params_season["a",srp_year_subset,,season_count,,i]), c(params_season["b",srp_year_subset,,season_count,,i]))
        }
    }
    recm <- rec * residuals_mult[,rec_year_subset]
    reca <- rec + residuals_add[,rec_year_subset]
    rec_outm <- test_fwdSR_predict_recruitment("ricker", params_season, residuals_mult, TRUE, srp_in[,srp_year_subset,], c(srp_year_subset[1],1,1,1,1), c(rec_year_subset[1],1,1,1,1))
    rec_outa <- test_fwdSR_predict_recruitment("ricker", params_season, residuals_add, FALSE, srp_in[,srp_year_subset,], c(srp_year_subset[1],1,1,1,1), c(rec_year_subset[1],1,1,1,1))
    expect_equal(c(rec_outm), c(recm))
    expect_equal(c(rec_outa), c(reca))
})
