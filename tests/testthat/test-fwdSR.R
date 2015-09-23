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
    expect_identical(sr_op[["params"]], params)
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


