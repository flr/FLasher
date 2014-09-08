context("Implementation of FLSR")

test_that("fwdSR constructors",{
    data(ple4)
    ple4.sr.ricker <- fmle(as.FLSR(ple4,model="ricker"), control  = list(trace=0))
    params.ricker <- as.FLQuant(params(ple4.sr.ricker))
    residuals.ricker <- FLQuant(rnorm(100), dimnames = list(year = 1:10, iter = 1:10))
    ple4.sr.bevholt <- fmle(as.FLSR(ple4,model="bevholt"), control  = list(trace=0))
    params.bevholt <- as.FLQuant(params(ple4.sr.bevholt))
    residuals.bevholt <- FLQuant(rnorm(100), dimnames = list(year = 1:10, iter = 1:10))
    residuals_mult <- TRUE
    timelag = 0
    # Empty constructor - shouldn't fail
    test_fwdSR_empty_constructor()
    # Wrap
    sr_op <- test_fwdSR_constructor_wrap("ricker", params.ricker, timelag, residuals.ricker, residuals_mult)
    expect_that(sr_op[["params"]], is_identical_to(params.ricker))
    expect_that(sr_op[["residuals"]], is_identical_to(residuals.ricker))
    expect_that(sr_op[["residuals_mult"]], is_identical_to(residuals_mult))
    # eval
    rec <- test_fwdSR_eval_simple("ricker", params.ricker, timelag, residuals.ricker, residuals_mult, c(ssb(ple4.sr.ricker)[1,1,]))
    expect_that(rec, is_identical_to(c(predict(ple4.sr.ricker)[1,1,])))
    rec <- test_fwdSR_eval_simple("Ricker", params.ricker, timelag, residuals.ricker, residuals_mult, c(ssb(ple4.sr.ricker)[1,1,]))
    expect_that(rec, is_identical_to(c(predict(ple4.sr.ricker)[1,1,])))
    rec <- test_fwdSR_eval_simple("bevholt", params.bevholt, timelag, residuals.bevholt, residuals_mult, c(ssb(ple4.sr.bevholt)[1,1,]))
    expect_that(rec, is_identical_to(c(predict(ple4.sr.bevholt)[1,1,])))
    rec <- test_fwdSR_eval_simple("Bevholt", params.bevholt, timelag, residuals.bevholt, residuals_mult, c(ssb(ple4.sr.bevholt)[1,1,]))
    expect_that(rec, is_identical_to(c(predict(ple4.sr.bevholt)[1,1,])))
    # test get_nparams
    expect_that(dim(params.ricker)[1], is_identical_to(test_fwdSR_get_nparams("ricker", params.ricker, timelag, residuals.ricker, residuals_mult)))
    timelag <- as.integer(round(runif(1,min=0,max=10)))
    expect_that(timelag, is_identical_to(test_fwdSR_get_timelag("ricker", params.ricker, timelag, residuals.ricker, residuals_mult)))
    # Copy constructor
    srs <- test_fwdSR_copy_constructor("ricker", params.ricker, timelag, residuals.ricker, residuals_mult, c(ssb(ple4.sr.bevholt)[1,1,]))
    expect_that(srs[[1]], is_identical_to(srs[[2]]))
    expect_that(srs[[3]], is_identical_to(srs[[4]]))
    # Assignment operator
    srs <- test_fwdSR_assignment_operator("ricker", params.ricker, timelag, residuals.ricker, residuals_mult, c(ssb(ple4.sr.bevholt)[1,1,]))
    expect_that(srs[[1]], is_identical_to(srs[[2]]))
    expect_that(srs[[3]], is_identical_to(srs[[4]]))
})

test_that("fwdSR eval_model",{
    data(ple4)
    ple4.sr.ricker <- fmle(as.FLSR(ple4,model="ricker"), control  = list(trace=0))
    params.ricker <- as.FLQuant(params(ple4.sr.ricker))
    residuals.ricker <- FLQuant(rnorm(100), dimnames = list(year = 1:10, iter = 1:10))
    ple4.sr.bevholt <- fmle(as.FLSR(ple4,model="bevholt"), control  = list(trace=0))
    params.bevholt <- as.FLQuant(params(ple4.sr.bevholt))
    residuals.bevholt <- FLQuant(rnorm(100), dimnames = list(year = 1:10, iter = 1:10))
    residuals_mult <- TRUE
    timelag <- 0
    # simple eval
    rec <- test_fwdSR_eval_simple("ricker", params.ricker, timelag, residuals.ricker, residuals_mult, c(ssb(ple4.sr.ricker)[1,1,]))
    expect_that(rec, is_identical_to(c(predict(ple4.sr.ricker)[1,1,])))
    rec <- test_fwdSR_eval_simple("Ricker", params.ricker, timelag, residuals.ricker, residuals_mult, c(ssb(ple4.sr.ricker)[1,1,]))
    expect_that(rec, is_identical_to(c(predict(ple4.sr.ricker)[1,1,])))
    rec <- test_fwdSR_eval_simple("bevholt", params.bevholt, timelag, residuals.bevholt, residuals_mult, c(ssb(ple4.sr.bevholt)[1,1,]))
    expect_that(rec, is_identical_to(c(predict(ple4.sr.bevholt)[1,1,])))
    rec <- test_fwdSR_eval_simple("Bevholt", params.bevholt, timelag, residuals.bevholt, residuals_mult, c(ssb(ple4.sr.bevholt)[1,1,]))
    expect_that(rec, is_identical_to(c(predict(ple4.sr.bevholt)[1,1,])))
    # test eval with multiple years etc
    ssb <- 1000
    year <- round(runif(1,min=2,max=10))
    unit <- round(runif(1,min=2,max=10))
    season <- round(runif(1,min=2,max=10))
    area <- round(runif(1,min=2,max=10))
    iter <- round(runif(1,min=2,max=10))
    rec <- test_fwdSR_eval_full("ricker", params.ricker, timelag, residuals.ricker, residuals_mult, ssb, year, unit, season, area, iter)
    expect_that(rec, is_identical_to(c(predict(ple4.sr.ricker, ssb=FLQuant(ssb)))))

    # Expand params - cannot properly test as FLSR not set up yet
    # random indices for dims
    indices <- round(runif(6,min=2,max=10))
    # Multi years - accessing other dims with dim greater than their lengths
    nyears <- round(runif(1,min=2,max=10))
    params.multi <- FLPar(abs(rnorm(nyears*2, sd=c(params(ple4.sr.ricker)))), dimnames=list(params = c("a","b"), year=1:nyears, iter=1))
    year <- round(runif(1,min=1,max=nyears))
    rec_R <- params.multi["a",year] * ssb * exp(-params.multi["b",year] * ssb)
    rec_C <- test_fwdSR_eval_full("ricker", as.FLQuant(params.multi), timelag, residuals.ricker, residuals_mult, ssb, year, indices[3], indices[4], indices[5], indices[6])
    expect_that(c(rec_R), is_identical_to(rec_C))
    # And if year is > nyears in params, use params in year 1
    rec_C <- test_fwdSR_eval_full("ricker", as.FLQuant(params.multi), timelag, residuals.ricker, residuals_mult, ssb, nyears+1, indices[3], indices[4], indices[5], indices[6])
    rec_R <- params.multi["a",1] * ssb * exp(-params.multi["b",1] * ssb)
    expect_that(c(rec_R), is_identical_to(rec_C))
    # Multi seasons
    nseasons <- round(runif(1,min=2,max=10))
    params.multi <- FLPar(abs(rnorm(nseasons*2, sd=c(params(ple4.sr.ricker)))), dimnames=list(params = c("a","b"), year=1,unit=1,season=1:nseasons, iter=1))
    season <- round(runif(1,min=1,max=nseasons))
    rec_R <- params.multi["a",1,1,season] * ssb * exp(-params.multi["b",1,1,season] * ssb)
    rec_C <- test_fwdSR_eval_full("ricker", as.FLQuant(params.multi), timelag, residuals.ricker, residuals_mult, ssb, indices[2], indices[3], season, indices[5], indices[6])
    expect_that(c(rec_R), is_identical_to(rec_C))
    # And if season is > nseason in params, use params in year 1
    rec_C <- test_fwdSR_eval_full("ricker", as.FLQuant(params.multi), timelag, residuals.ricker, residuals_mult, ssb, indices[2], indices[3], nseasons+1, indices[5], indices[6])
    rec_R <- params.multi["a",1,1,1] * ssb * exp(-params.multi["b",1,1,1] * ssb)
    expect_that(c(rec_R), is_identical_to(rec_C))
    # Multi years and seasons
    nyears <- round(runif(1,min=2,max=10))
    nseasons <- round(runif(1,min=2,max=10))
    params.multi <- FLPar(abs(rnorm(nyears*nseasons*2, sd=c(params(ple4.sr.ricker)))), dimnames=list(params = c("a","b"), year=1:nyears,unit=1,season=1:nseasons, iter=1))
    year <- round(runif(1,min=1,max=nyears))
    season <- round(runif(1,min=1,max=nseasons))
    rec_R <- params.multi["a",year,1,season] * ssb * exp(-params.multi["b",year,1,season] * ssb)
    rec_C <- test_fwdSR_eval_full("ricker", as.FLQuant(params.multi), timelag, residuals.ricker, residuals_mult, ssb, year, indices[3], season, indices[5], indices[6])
    expect_that(c(rec_R), is_identical_to(rec_C))
    # And if season is > nseason in params, use params in year 1
    rec_C <- test_fwdSR_eval_full("ricker", as.FLQuant(params.multi), timelag, residuals.ricker, residuals_mult, ssb, year, indices[3], nseasons+1, indices[5], indices[6])
    rec_R <- params.multi["a",year,1,1] * ssb * exp(-params.multi["b",year,1,1] * ssb)
    expect_that(c(rec_R), is_identical_to(rec_C))

    # with iters
    nyears <- round(runif(1,min=2,max=10))
    niters <- round(runif(1,min=2,max=10))
    params.multi <- FLPar(abs(rnorm(nyears*2, sd=c(params(ple4.sr.ricker)))), dimnames=list(params = c("a","b"), year=1:nyears, unit=1,season=1,area=1,iter=1:niters))
    year <- round(runif(1,min=1,max=nyears))
    iter <- round(runif(1,min=1,max=niters))
    rec_R <- params.multi["a",year,1,1,1,iter] * ssb * exp(-params.multi["b",year,1,1,1,iter] * ssb)
    rec_C <- test_fwdSR_eval_full("ricker", as.FLQuant(params.multi), timelag, residuals.ricker, residuals_mult, ssb, year, indices[3], indices[4], indices[5], iter)
    expect_that(c(rec_R), is_identical_to(rec_C))

})
