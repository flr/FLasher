# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Implementation of fwdSR")
source("expect_funs.R")

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

# Stuff in an FLQuant of SSB and calc the recruitment
# Nothing in this method about timing between spawning and recruitment
# That is handled by operating model calc_rec()
test_that("fwdSR predict_recruitment",{
    # Base this on ple4 ricker
    data(ple4)
    ricker <- fmle(as.FLSR(ple4,model="ricker"), control  = list(trace=0))
    params <- as.FLQuant(params(ricker))
    srp <- ssb(ricker)
    rickerR <- function(srp, a, b){
        return(a * srp *(exp(-b * srp)))
    }
    # Set up some parameters and SRP for the tests -  With seasonal timestep, 4 units and multiple iterations
    niters <- 10
    srp_in <- FLQuant(NA, dim=c(1,dim(srp)[2],4,4), iter=niters)
    srp_in[] <- abs(rnorm(prod(dim(srp_in)), mean=c(ssb(ricker)), sd=10))
    #sr_params <- FLQuant(NA, dim=c(2,dim(srp)[2],4,4), dimnames=list(param = c("a","b")), iter=niters)
    sr_params <- FLQuant(NA, dim=c(2,dim(srp)[2],4,4), iter=niters)
    names(dimnames(sr_params))[1] <- "param"
    dimnames(sr_params)[[1]] <- c("a","b")
    sr_params["a",] <- abs(rnorm(prod(dim(sr_params["a",])), mean=c(params["a",]), sd=1))
    sr_params["b",] <- abs(rnorm(prod(dim(sr_params["b",])), mean=c(params["b",]), sd=1e-6))
    # These parameters are hacked about with for the tests
    # In an operating model residuals must be same dim as biol - here set up as dim of SRP
    residuals_mult <- srp_in
    residuals_mult[] <- abs(rnorm(prod(dim(residuals_mult)), mean=1, sd = 0.1))
    residuals_add <- srp_in
    residuals_add[] <- abs(rnorm(prod(dim(residuals_mult)), mean=0, sd = 0.1))
    # Tests for:
    # Check sr parameter recycling
    # Check subset of parameters and residuals - pass in small srp and specify start position
    # Test 1: Simple annual timestep.
    # Single unit and season for srp
    # No structure to params - just a single year, season, unit, iter
    iter <- round(runif(1, min=1, max=niters))
    year <- round(runif(1, min=2, max=dim(srp_in)[2]-1)) 
    season <- round(runif(1,min=1,max=4))
    unit <- round(runif(1,min=1,max=4))
    sr_params_temp <- sr_params[,year,season,unit,1,iter]
    srp_temp <- srp_in[,,unit,season,1,iter]
    rec <- rickerR(srp_temp, c(sr_params_temp["a",]), c(sr_params_temp["b",]))
    recm <- rec * residuals_mult[,,unit,season,,iter]
    reca <- rec + residuals_add[,,unit,season,,iter]
    # params and residuals start at beginning as passing in all SRP years
    rec_outm <- test_fwdSR_predict_recruitment("ricker", sr_params_temp, residuals_mult[,,unit,season,1,iter], TRUE, srp_temp, c(1,1,1,1,1))
    rec_outa <- test_fwdSR_predict_recruitment("ricker", sr_params_temp, residuals_add[,,unit,season,1,iter], FALSE, srp_temp, c(1,1,1,1,1))
    expect_equal(c(rec_outm), c(recm))
    expect_equal(c(rec_outa), c(reca))
    # What if start year of residuals is too big? Does not recycle like SR params - should fail
    expect_error(test_fwdSR_predict_recruitment("ricker", sr_params_temp, residuals_mult[,,unit,season,1,iter], TRUE, srp_temp, c(year+1,1,1,1,1)))
    # Test 2: Annual timestep with a subset of SRP - no dims in params
    # SRP starts at year, so we start params and residuals also at year
    srp_temp <- srp_in[,year:dim(srp_in)[2],unit,season,1,iter]
    rec_outm <- test_fwdSR_predict_recruitment("ricker", sr_params_temp, residuals_mult[,,unit,season,1,iter], TRUE, srp_temp, c(year,1,1,1,1))
    rec_outa <- test_fwdSR_predict_recruitment("ricker", sr_params_temp, residuals_add[,,unit,season,1,iter], FALSE, srp_temp, c(year,1,1,1,1))
    expect_equal(recm[,year:dim(srp_in)[2]], rec_outm)
    expect_equal(reca[,year:dim(srp_in)[2]], rec_outa)
    # Parameters get recycled
    # What if start year of residuals is too big? Does not recycle like SR params - should fail
    expect_error(test_fwdSR_predict_recruitment("ricker", sr_params_temp, residuals_mult[,,unit,season,1,iter], TRUE, srp_temp, c(year+1,1,1,1,1)))
    # Test 3: Annual timestep but with iters in SRP and none in params and residuals
    srp_temp <- srp_in[,,unit,season,1,]
    rec <- rickerR(srp_temp, c(sr_params_temp["a",]), c(sr_params_temp["b",]))
    recm <- rec %*% residuals_mult[,,unit,season,,iter]
    rec_outm <- test_fwdSR_predict_recruitment("ricker", sr_params_temp, residuals_mult[,,unit,season,1,iter], TRUE, srp_temp, c(1,1,1,1,1))
    expect_equal(c(rec_outm), c(recm))
    # Test 4: Annual timestep with subset of SRP. Iters in SRP, not residuals or params. Iters get recycled in residuals
    srp_temp <- srp_in[,year:dim(srp_in)[2],unit,season,1,]
    rec_outm <- test_fwdSR_predict_recruitment("ricker", sr_params_temp, residuals_mult[,,unit,season,1,iter], TRUE, srp_temp, c(year,1,1,1,1))
    expect_FLQuant_equal(recm[,year:dim(srp_in)[2]], rec_outm)
    # Test 5: Seasonal and Unit SRP with iters. Units in params. Residuals dim match SRP (including iters)
    srp_temp <- srp_in[,,,season,]
    sr_params_temp <- sr_params[,,,season,,iter]
    rec <- rickerR(srp_temp, c(sr_params_temp["a",]), c(sr_params_temp["b",]))
    recm <- rec %*% residuals_mult[,,,season,1,]
    rec_outm <- test_fwdSR_predict_recruitment("ricker", sr_params_temp, residuals_mult[,,,season,1,], TRUE, srp_temp, c(1,1,1,1,1))
    expect_equal(c(rec_outm), c(recm))
    # Tests 6: Subset SRP for a single timestep and unit. All iters
    # Params have no time structure but all units. No iters
    # We want just a single unit calculated
    # This is what happens inside operatingModel::calc_rec()
    srp_temp <- srp_in[,year,unit,season,1,]
    sr_params_temp <- sr_params[,year,,season,1,iter]
    rec <- rickerR(srp_temp, c(sr_params_temp["a",,unit]), c(sr_params_temp["b",,unit]))
    recm <- rec %*% residuals_mult[,year,unit,season,,]
    rec_outm <- test_fwdSR_predict_recruitment("ricker", sr_params_temp, residuals_mult, TRUE, srp_temp, c(year,unit,season,1,1))
    expect_equal(c(rec_outm), c(recm))
})

test_that("fwdBiol does_recruitment_happen",{
    nyears <- 10
    niters <- 100
    flq_in <- FLQuant(NA, dimnames=list(age=0:5, unit=1:4, year=1:nyears, season=1:4,iter=niters))
    residuals <- FLQuant(rnorm(100), dimnames = list(year = 1:nyears, unit=1:4, season=1:4, iter = 1:niters))
    residuals_mult <- TRUE
    # With 4 Seasons and 4 Units
    # Units 1 and 2 are M/F recruiting in Season 1
    # Units 3 and 4 are M/F recruiting in Season 3
    # No recruitment in seaons 2 and 4
    sr_params <- FLQuant(NA, dim=c(2,1,4,4,1,1)) 
    sr_params[,,c(1,2),1,] <- rnorm(4)
    sr_params[,,c(3,4),3,] <- rnorm(4)
    # Random years - recycled internally
    season1_years <- seq(from = 1, to=nyears)
    season2_years <- seq(from = 1, to=nyears)
    season3_years <- seq(from = 1, to=nyears)
    season4_years <- seq(from = 1, to=nyears)
    # Season 1
    expect_true(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 1, sample(season1_years, 1), 1))
    expect_true(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 2, sample(season2_years, 1), 1))
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 3, sample(season3_years, 1), 1))
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 4, sample(season4_years, 1), 1))
    # Season 2
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 1, sample(season1_years, 1), 2))
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 2, sample(season2_years, 1), 2))
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 3, sample(season3_years, 1), 2))
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 4, sample(season4_years, 1), 2))
    # Season 3
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 1, sample(season1_years, 1), 3))
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 2, sample(season2_years, 1), 3))
    expect_true(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 3, sample(season3_years, 1), 3))
    expect_true(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 4, sample(season4_years, 1), 3))
    # Season 4
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 1, sample(season1_years, 1), 4))
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 2, sample(season2_years, 1), 4))
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 3, sample(season3_years, 1), 4))
    expect_false(test_fwdSR_does_recruitment_happen("Ricker", sr_params, residuals, residuals_mult, 4, sample(season4_years, 1), 4))
})

