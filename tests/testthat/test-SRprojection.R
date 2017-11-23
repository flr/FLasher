# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Checking the SR is evaluated correctly")
# Based on the tutorial Setting_Stock_Recruitment....
# If these don't work we are in trouble!
source("expect_funs.R")


test_that("Test1",{
    data(ple4)
    slots <- c("landings.n", "discards.n", "catch.n", "stock.n")
    for (i in slots){
        slot(ple4, i) <- slot(ple4, i) / 1000
        # Correct units too
        units(slot(ple4, i)) <- "10^6"
    }
    # Recalc aggregate slots
    catch(ple4) <- computeCatch(ple4)
    landings(ple4) <- computeLandings(ple4)
    discards(ple4) <- computeDiscards(ple4)
    stock(ple4) <- computeStock(ple4)
    nyears <- 10
    ple4mtf <- stf(ple4, nyears)
    years <- 2009:(2008+nyears)
    catch_target <- 100
    # Not interested in hitting a target - just that the SR is OK
    control <- fwdControl(data.frame(year=years, quant="catch", value=catch_target))
    ple4_srr <- fmle(as.FLSR(ple4, model="bevholt"), control=list(trace=0))
    # Pass FLSR object
    test <- fwd(ple4mtf, control=control, sr=ple4_srr)
    expect_equal(c(rec(test)[,ac(years)]), c(predict(ple4_srr, ssb=ssb(test)[,ac(years-1)])))
    # Pass list
    bh_params <- params(ple4_srr)
    test <- fwd(ple4mtf, control=control, sr=list(model = "bevholt", params=bh_params))
    expect_equal(c(rec(test)[,ac(years)]), c(predict(ple4_srr, ssb=ssb(test)[,ac(years-1)])))
    # Fix the parameters
    geomeanrec <- exp(mean(log(rec(ple4)[,ac(2006:2008)])))
    test <- fwd(ple4mtf, control=control, sr=list(model = "geomean", params=FLPar(a=geomeanrec)))
    expect_equal(c(rec(test)[,ac(years)]), rep(geomeanrec, nyears))
    # Time varying fixed parameters
    decrec <- seq(900, 500, length=nyears)
    decpar <- FLPar(decrec, dimnames=list(params="a", year=2009:2018, iter=1))
    test <- fwd(ple4mtf, control=control, sr=list(model = "geomean", params=decpar))
    expect_equal(c(rec(test)[,ac(years)]), decrec)
    # Error - year dim in the FLPar but only one year
    badpar <- FLPar(1000, dimnames=list(params="a", year=2009, iter=1))
    expect_error(fwd(ple4mtf, control=control, sr=list(model = "geomean", params=badpar)))
    # Cycling BH parameters
    a <- c(params(ple4_srr)["a"])
    acycle <- a * (1 + sin(seq(from=0,to=2*pi,length=10))/10)
    srpar <- FLPar(NA, dimnames=list(params=c("a","b"), year=2009:2018, iter=1))
    srpar["a"] <- acycle
    srpar["b"] <- c(params(ple4_srr)["b"])
    test <- fwd(ple4mtf, control=control, sr=list(model = "bevholt", params=srpar))
    # Set up a predictModel with these pars
    cyclebh <- predictModel(model=model(ple4_srr), params=srpar)
    expect_equal(c(rec(test)[,ac(years)]), c(predict(cyclebh, ssb=ssb(test)[,ac(years-1)])))
    # Stochastic projections
    niters <- 50
    ple4mtfp <- propagate(ple4mtf, niters)
    # Recycle params over iters
    test <- fwd(ple4mtfp, control=control, sr=list(model = "bevholt", params=bh_params))
    expect_equal(c(rec(test)[,ac(years)]), c(predict(ple4_srr, ssb=ssb(test)[,ac(years-1)])))
    # Residuals 1 iter
    res <- FLQuant(NA, dimnames=list(year=years, iter=1))
    res[] <- sample(c(exp(residuals(ple4_srr))), prod(dim(res)), replace=TRUE)
    test <- fwd(ple4mtf, control=control, sr=ple4_srr, residuals=res)
    expect_equal(c(rec(test)[,ac(years)]),c(predict(ple4_srr, ssb=ssb(test)[,ac(years-1)]) %*% res))
    # Residuals multi iters
    res <- FLQuant(NA, dimnames=list(year=years, iter=1:niters))
    res[] <- sample(c(exp(residuals(ple4_srr))), prod(dim(res)), replace=TRUE)
    test <- fwd(ple4mtfp, control=control, sr=ple4_srr, residuals=res)
    expect_equal(c(rec(test)[,ac(years)]),c(predict(ple4_srr, ssb=ssb(test)[,ac(years-1)]) %*% res))

    # Iterations in the params
#    vc <- vcov(ple4_srr)[,,1]
#    invsd <- solve(sqrt(diag(diag(vc))))
#    corr_matrix <- invsd %*% vc %*% invsd
#    newsd <- diag(c(params(ple4_srr)) * 0.25)
#    newvc <- newsd %*% corr_matrix %*% newsd
#    newparams <- mvrnorm(niters, mu = c(params(ple4_srr)), Sigma=newvc)
#    newparams[newparams<=0] <- 1
#    iter_params <- propagate(params(ple4_srr),niters)
#    iter_params["a",] <- newparams[,1]
#    iter_params["b",] <- newparams[,2]
#    test <- fwd(ple4mtfp, control=control, sr=list(model="bevholt", params=iter_params))
#
#
# predict model has a problem with iters
#
#    iterbh <- predictModel(model=model(ple4_srr), params=iter_params)
#    ssb <- ssb(test)
#    predict(iterbh, ssb=ssb[,ac(2009)])
#    predict(iterbh, ssb=ssb[,ac(2009:2010)])
#    rec(test)[,ac(2010)]
#
#    expect_equal(c(
#                   rec(test)[,ac(years)]
#                   ), c(
#                                             predict(iterbh, ssb=ssb(test)[,ac(years-1)])
#                                             
#                                             ))
#
#
#

    niters <- 50
    a <- rlnorm(niters, mean=log(900), sd=1)
    b <- rlnorm(niters, mean=log(10), sd=1)
    pm <- predictModel(model=bevholt()$model, params=FLPar(a=a, b=b, iter=1:niters))
    ssb <- FLQuant(rlnorm(niters*10, mean=log(200), sd=1), dimnames=list(year=1:10, iter=1:niters))
    predict(pm, ssb=ssb[,1])
    predict(pm, ssb=ssb[,1:2])


})

