# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Checking the SR is evaluated correctly")
# Based on the tutorial Setting_Stock_Recruitment....
# If these don't work we are in trouble!
source("expect_funs.R")


test_that("SR models with FLStock",{
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
    years <- seq(dims(ple4)$maxyear + 1, dims(ple4mtf)$maxyear)
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
  
    geomeanrec <- exp(mean(log(rec(ple4)[,ac(years[1] - c(1, 2, 3))])))
    test <- fwd(ple4mtf, control=control, sr=list(model = "geomean", params=FLPar(a=geomeanrec)))
    expect_equal(c(rec(test)[,ac(years)]), rep(geomeanrec, nyears))
    
    # Time varying fixed parameters
    
    decrec <- seq(900, 500, length=nyears)
    decpar <- FLPar(decrec, dimnames=list(params="a", year=years, iter=1))
    test <- fwd(ple4mtf, control=control, sr=list(model = "geomean", params=decpar))
    expect_equal(c(rec(test)[,ac(years)]), decrec)
    
    # Error - year dim in the FLPar but only one year
    
    badpar <- FLPar(1000, dimnames=list(params="a", year=2009, iter=1))
    expect_error(fwd(ple4mtf, control=control, sr=list(model = "geomean", params=badpar)))
    
    # Cycling BH parameters
    
    a <- c(params(ple4_srr)["a"])
    acycle <- a * (1 + sin(seq(from=0,to=2*pi,length=10))/10)
    srpar <- FLPar(NA, dimnames=list(params=c("a","b"), year=years, iter=1))
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
    
    # deviances 1 iter
    
    res <- FLQuant(NA, dimnames=list(year=years, iter=1))
    res[] <- sample(c(exp(residuals(ple4_srr))), prod(dim(res)), replace=TRUE)
    test <- fwd(ple4mtf, control=control, sr=ple4_srr, deviances=res)
    expect_equal(c(rec(test)[,ac(years)]),c(predict(ple4_srr, ssb=ssb(test)[,ac(years-1)]) %*% res))
    
    # deviances multi iters
    
    res <- FLQuant(NA, dimnames=list(year=years, iter=1:niters))
    res[] <- sample(c(exp(residuals(ple4_srr))), prod(dim(res)), replace=TRUE)
    test <- fwd(ple4mtfp, control=control, sr=ple4_srr, deviances=res)
    expect_equal(c(rec(test)[,ac(years)]),c(predict(ple4_srr, ssb=ssb(test)[,ac(years-1)]) %*% res))
    
    # Iterations in the params
    
    vc <- vcov(ple4_srr)[,,1]
    invsd <- solve(sqrt(diag(diag(vc))))
    corr_matrix <- invsd %*% vc %*% invsd
    newsd <- diag(c(params(ple4_srr)) * 0.10)
    newvc <- newsd %*% corr_matrix %*% newsd
    newparams <- mvrnorm(niters, mu = c(params(ple4_srr)), Sigma=newvc)
    newparams[newparams<=0] <- 1
    iter_params <- propagate(params(ple4_srr),niters)
    iter_params["a",] <- newparams[,1]
    iter_params["b",] <- newparams[,2]
    test <- fwd(ple4mtfp, control=control, sr=list(model="bevholt", params=iter_params))
    iterbh <- predictModel(model=model(ple4_srr), params=iter_params)
    
    # predict model has a problem with iters
    
    expect_equal(c(rec(test)[,ac(years)]), c(predict(iterbh, ssb=ssb(test)[,ac(years-1)])))
    
    # deviances and iterations in the params
    
    test <- fwd(ple4mtfp, control=control, sr=list(model="bevholt", params=iter_params), deviances=res)
    expect_equal(c(rec(test)[,ac(years)]), c(predict(iterbh, ssb=ssb(test)[,ac(years-1)]) %*% res))
})

test_that("SR models with FLBiol and FLFishery",{
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
    years <- seq(2018, length=nyears)
    catch_target <- 100
    # Not interested in hitting a target - just that the SR is OK
    control <- fwdControl(data.frame(year=years, quant="catch", biol=1, value=catch_target))
    ple4_srr <- fmle(as.FLSR(ple4, model="bevholt"), control=list(trace=0))
    biol <- as(ple4mtf, "FLBiol")
    fishery <- as(ple4mtf, "FLFishery")
    # Set SR directly
    biol@rec@model <- model(ple4_srr)
    biol@rec@params <- params(ple4_srr)
    test <- fwd(biol, fishery=fishery, control=control)
    bh <- predictModel(model=model(ple4_srr), params=params(ple4_srr))
    # ssb method be OK here as m.spawn and harvest.spawn of seed FLStock are 0
    # Setting SR as predict model
    biol@rec <- bh
    test <- fwd(biol, fishery=fishery, control=control)
    expect_equal(c(n(test[["biols"]])[1,ac(years)]), c(predict(bh, ssb=ssb(test[["biols"]])[,ac(years-1)])))
    # Time varying parameters
    decrec <- seq(900, 500, length=10)
    decpar <- FLPar(decrec, dimnames=list(params="a", year=control$year, iter=1))
    biol@rec@model <- geomean()[["model"]]
    biol@rec@params <- decpar
    test <- fwd(biol, fishery=fishery, control=control)
    expect_equal(c(n(test[["biols"]])[1,ac(years)]), decrec)
    # Stochastic projections
    niters <- 100
    biolp <- biol
    biolp@n <- propagate(biolp@n, niters)
    fisheryp <- propagate(fishery, niters)
    fisheryp[[1]] <- propagate(fishery[[1]], niters)
    biolp@rec@model <- model(ple4_srr)
    biolp@rec@params <- params(ple4_srr)
    # deviances
    res <- FLQuant(NA, dimnames=list(year=years, iter=1:niters))
    res[] <- sample(c(exp(residuals(ple4_srr))), prod(dim(res)), replace=TRUE)
    test <- fwd(biolp, fishery=fisheryp, control=control, deviances=res)
    expect_equal(c(n(test[["biols"]])[1,ac(years)]), c(predict(bh, ssb=ssb(test[["biols"]])[,ac(years-1)]) %*% res))
    # Iterations in SR pars
    vc <- vcov(ple4_srr)[,,1]
    invsd <- solve(sqrt(diag(diag(vc))))
    corr_matrix <- invsd %*% vc %*% invsd
    newsd <- diag(c(params(ple4_srr)) * 0.10)
    newvc <- newsd %*% corr_matrix %*% newsd
    newparams <- mvrnorm(niters, mu = c(params(ple4_srr)), Sigma=newvc)
    newparams[newparams<=0] <- 1
    iter_params <- propagate(params(ple4_srr),niters)
    iter_params["a",] <- newparams[,1]
    iter_params["b",] <- newparams[,2]
    iterbh <- predictModel(model=model(ple4_srr), params=iter_params)
    biolp@rec@params <- iter_params
    test <- fwd(biolp, fishery=fisheryp, control=control)
    expect_equal(c(n(test[["biols"]])[1,ac(years)]), c(predict(iterbh, ssb=ssb(test[["biols"]])[,ac(years-1)])))
    # deviances and iterations in SR pars
    test <- fwd(biolp, fishery=fisheryp, control=control, deviances=res)
    expect_equal(c(n(test[["biols"]])[1,ac(years)]), c(predict(iterbh, ssb=ssb(test[["biols"]])[,ac(years-1)]) %*% res))
})
