# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Projections with FLStock")
# If these don't work we are in trouble!
source("expect_funs.R")


test_that("Catch target, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    min_year[1] <- min_year[1] + 1
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    catch_val <- rlnorm(n=nyears, mean=log(min(catch(ple4)/10)), sd=0.1)
    control=fwdControl(data.frame(year=years, quant="catch", value=catch_val))
    res <- fwd(ple4, control=control, sr=predictModel(model="geomean", 
      params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))
    catch_out <- c(catch(res)[,ac(years)])
    # Test to within tolerance of 1.5e-8
    expect_equal(catch_out, catch_val)
})

test_that("Catch target, multiple iters",{
    data(ple4)
    niters <- round(runif(1, min=10, max=50))
    ple4p <- propagate(ple4, niters)
    # Spoof up stock.n, landings.wt, landings.n and harvest
    harvest(ple4p)[] <- rlnorm(n=prod(dim(harvest(ple4p))), mean=log(c(harvest(ple4p))), sd=0.001)
    stock.n(ple4p)[] <- rlnorm(n=prod(dim(stock.n(ple4p))), mean=log(c(stock.n(ple4p))), sd=0.001)
    landings.wt(ple4p)[] <- rlnorm(n=prod(dim(landings.wt(ple4p))), mean=log(c(landings.wt(ple4p))), sd=0.001)
    landings.n(ple4p)[] <- rlnorm(n=prod(dim(landings.n(ple4p))), mean=log(c(landings.n(ple4p))), sd=0.001)
    year_range <- range(ple4p)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    min_year[1] <- min_year[1] + 1
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    catch_val <- rlnorm(n=nyears*niters, mean=log(min(catch(ple4p)/10)), sd=0.1)
    control=fwdControl(data.frame(year=years, quant="catch", value=0), iters=niters)
    control@iters[,"value",] <- catch_val
    res <- fwd(ple4p, control=control, sr=predictModel(model="geomean", 
      params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))
    catch_out <- c(catch(res)[,ac(years)])
    # Test to within tolerance of 1.5e-8
    expect_equal(catch_out, catch_val)
})

test_that("Fbar target, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    min_year[1] <- min_year[1] + 1
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    f_val <- rlnorm(n=nyears, mean=log(min(fbar(ple4)/10)), sd=0.1)
    control=fwdControl(data.frame(year=years, quant="fbar", value=f_val))
    res <- fwd(ple4, control=control, sr=predictModel(model="geomean", 
      params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))
    f_out <- c(fbar(res)[,ac(years)])
    # Test to within tolerance of 1.5e-8
    expect_equal(f_out, f_val)
})

test_that("Fbar target, multiple iters",{
    data(ple4)
    niters <- round(runif(1, min=10, max=50))
    ple4p <- propagate(ple4, niters)
    # Spoof up stock.n, landings.wt, landings.n and harvest
    harvest(ple4p)[] <- rlnorm(n=prod(dim(harvest(ple4p))), mean=log(c(harvest(ple4p))), sd=0.001)
    stock.n(ple4p)[] <- rlnorm(n=prod(dim(stock.n(ple4p))), mean=log(c(stock.n(ple4p))), sd=0.001)
    landings.wt(ple4p)[] <- rlnorm(n=prod(dim(landings.wt(ple4p))), mean=log(c(landings.wt(ple4p))), sd=0.001)
    landings.n(ple4p)[] <- rlnorm(n=prod(dim(landings.n(ple4p))), mean=log(c(landings.n(ple4p))), sd=0.001)
    year_range <- range(ple4p)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    min_year[1] <- min_year[1] + 1
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    f_val <- rlnorm(n=nyears*niters, mean=log(min(fbar(ple4p)/10)), sd=0.1)
    control=fwdControl(data.frame(year=years, quant="fbar", value=0), iters=niters)
    control@iters[,"value",] <- f_val 
    res <- fwd(ple4p, control=control, sr=predictModel(model="geomean", 
      params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))
    f_out <- c(fbar(res)[,ac(years)])
    # Test to within tolerance of 1.5e-8
    expect_equal(f_out, f_val)
})

test_that("Catch target with min limit, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    min_year[1] <- min_year[1] + 1
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    catch_val <- rlnorm(n=nyears, mean=log(min(catch(ple4)/10)), sd=0.1)
    catch_lim <- mean(catch_val)
    # First project without limit
    control=fwdControl(list(year=years, quant="catch", value=catch_val))
    res <- fwd(ple4, control=control, sr=predictModel(model="geomean", params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))
    # Get resulting fbar
    fout <- fbar(res)[,ac(years)]
    limit_year <- sample(years,1)
    flim <- c(fout[,ac(limit_year)] * 1.5)
    # New projection
    control=fwdControl(list(year=years, quant="catch", value=catch_val),
                       list(year=limit_year, quant="fbar", min=flim))
    res <- fwd(ple4, control=control, sr=predictModel(model="geomean", params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))
    # All years apart from lim_year met catch OK
    catch_out <- catch(res)[,ac(years)]
    non_lim_years <- !(years %in% limit_year)
    expect_equal(c(catch_out[,ac(years[non_lim_years])]), catch_val[non_lim_years])
    # lim year has f = flim
    expect_equal(c(fbar(res)[,ac(limit_year)]), flim)
})

test_that("Catch target with max limit, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    min_year[1] <- min_year[1] + 1
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    catch_val <- rlnorm(n=nyears, mean=log(min(catch(ple4)/10)), sd=0.1)
    catch_lim <- mean(catch_val)
    # First project without limit
    control=fwdControl(list(year=years, quant="catch", value=catch_val))
    res <- fwd(ple4, control=control, sr=predictModel(model="geomean", params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))
    # Get resulting fbar
    fout <- fbar(res)[,ac(years)]
    limit_year <- sample(years,1)
    flim <- c(fout[,ac(limit_year)] * 0.5)
    # New projection
    control=fwdControl(list(year=years, quant="catch", value=catch_val),
                       list(year=limit_year, quant="fbar", max=flim))
    res <- fwd(ple4, control=control, sr=predictModel(model="geomean", params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))
    # All years apart from lim_year met catch OK
    catch_out <- catch(res)[,ac(years)]
    non_lim_years <- !(years %in% limit_year)
    expect_equal(c( catch_out[,ac(years[non_lim_years])]), catch_val[non_lim_years])
    # lim year has f = flim
    expect_equal(c(fbar(res)[,ac(limit_year)]), flim)
})

test_that("Catch relative target, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1]+1, max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    min_year[1] <- min_year[1] + 1
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    catch_rel_val <- runif(nyears, min=0.5, max=0.9)
    control=fwdControl(data.frame(year=years, quant="catch", value=catch_rel_val, relYear=years-1))
    res <- fwd(ple4, control=control, sr=predictModel(model="geomean", 
      params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))
    catch_out <- c(catch(res)[,ac(years)])
    catch_trg <- c(catch(ple4)[,ac(years[1]-1)]) * cumprod(catch_rel_val)
    # Test to within tolerance of 1.5e-8
    expect_equal(catch_out, catch_trg)
})

test_that("Fbar relative target, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    min_year[1] <- min_year[1] + 1
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    f_rel_val <- runif(nyears, min=0.5, max=0.9)
    control=fwdControl(data.frame(year=years, quant="fbar", value=f_rel_val, relYear=years-1))
    res <- fwd(ple4, control=control, sr=predictModel(model="geomean", 
      params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))))
    f_out <- c(fbar(res)[,ac(years)])
    f_trg <- c(fbar(ple4)[,ac(years[1]-1)]) * cumprod(f_rel_val)
    # Test to within tolerance of 1.5e-8
    expect_equal(f_out, f_trg)
})

#---------------------
test_that("Tests from Running Medium Term Forecasts with FLasher tutorial",{

    data(ple4)
    ple4_mtf <- stf(ple4, nyears = 10)
    ple4_sr <- fmle(as.FLSR(ple4, model="bevholt"), control=list(trace=0))
 
    # Simple F
 
    f_status_quo <- mean(fbar(ple4)[,as.character(2005:2008)])
    ctrl_f <- fwdControl(list(year=2009:2018, quant="f", value=f_status_quo))
    ple4_f_sq <- fwd(ple4_mtf, control = ctrl_f, sr = ple4_sr)
    expect_equal(c(fbar(ple4_f_sq)[,ac(2009:2018)]), rep(f_status_quo,10))
    expect_equal(c(predict(ple4_sr,ssb=ssb(ple4_f_sq)[,ac(2008:2017)])), c(rec(ple4_f_sq)[,ac(2009:2018)]))
    
    # Decreasing catch
    
    future_catch <- c(catch(ple4)[,"2008"]) * 0.9^(1:10)
    ctrl_catch <- fwdControl(list(year=2009:2018, quant = "catch", value=future_catch))
    ple4_catch <- fwd(ple4_mtf, control = ctrl_catch, sr = ple4_sr)
    expect_equal(c(catch(ple4_catch)[,ac(2009:2018)]), future_catch)
    expect_equal(c(predict(ple4_sr,ssb=ssb(ple4_catch)[,ac(2008:2017)])), c(rec(ple4_catch)[,ac(2009:2018)]))
    
    # SSB end
    
    final_ssb <- 100000
    ctrl_ssb <- fwdControl(list(year=2009, quant = "ssb_end", value=final_ssb))
    ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)
    survivors <- stock.n(ple4_ssb) * exp(-harvest(ple4_ssb) - m(ple4_ssb))
    expect_equal(c(quantSums((survivors * stock.wt(ple4_ssb) * mat(ple4_ssb))[,ac(2009)])), final_ssb)
    
    # SSB at spawn
    
    spawn_ssb <- 200000
    ctrl_ssb <- fwdControl(list(year=2009, quant = "ssb_spawn", value=spawn_ssb))
    expect_warning(fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr))
    m.spwn(ple4_mtf)[,ac(2009)] <- 0.5
    harvest.spwn(ple4_mtf)[,ac(2009)] <- 0.5
    ctrl_ssb <- fwdControl(data.frame(year=2009, quant = "ssb_spawn", value=spawn_ssb))
    ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)
    expect_equal(c(ssb(ple4_ssb)[,ac(2009)]), spawn_ssb)
    
    # SRP
    
    srp <- 200000
    ctrl_ssb <- fwdControl(data.frame(year=2009, quant = "srp", value=srp))
    ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)
    expect_equal(c(ssb(ple4_ssb)[,ac(2009)]), srp)
    
    # FLash-like
    
    m.spwn(ple4_mtf)[,ac(2009)] <- 0.5
    harvest.spwn(ple4_mtf)[,ac(2009)] <- 0.5
    flash_ssb <- 150000
    ctrl_ssb <- fwdControl(data.frame(year=2009, quant = "ssb_flash", value=flash_ssb))
    ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)
    expect_equal(flash_ssb, c(ssb(ple4_ssb)[,ac(2009)]))
    
    m.spwn(ple4_mtf)[,ac(2009)] <- 0.0
    harvest.spwn(ple4_mtf)[,ac(2009)] <- 0.0
    flash_ssb <- 150000
    ctrl_ssb <- fwdControl(data.frame(year=2009, quant = "ssb_flash", value=flash_ssb))
    ple4_ssb <- fwd(ple4_mtf, control=ctrl_ssb, sr = ple4_sr)
    expect_equal(c(ssb(ple4_ssb)[,ac(2010)]), flash_ssb)
    
    # Longer SSB
    
    m.spwn(ple4_mtf)[,ac(dims(ple4)$maxyear + 1)] <- 0.0
    harvest.spwn(ple4_mtf)[,ac(dims(ple4)$maxyear + 1)] <- 0.0
    future_ssb <- c(ssb(ple4)[,'2017']) * 0.8
    ctrl_ssb <- fwdControl(data.frame(year=seq(2018, 2027), quant = "ssb_flash", value=future_ssb))
    expect_warning(ple4_ssb <- fwd(ple4_mtf, control = ctrl_ssb, sr = ple4_sr))
    expect_equal(c(ssb(ple4_ssb)[,ac(2019:2027)]), rep(future_ssb, 9))
    expect_equal(c(predict(ple4_sr,ssb=ssb(ple4_ssb)[,ac(2019:2026)])), c(rec(ple4_ssb)[,ac(2020:2027)]))

    # Relative catch

    ctrl_rel_catch <- fwdControl(data.frame(
      year = 2009:2018, quant = "catch", value = 0.9, relYear = 2008:2017))
    ple4_rel_catch <- fwd(ple4_mtf, control = ctrl_rel_catch, sr = ple4_sr)
    expect_equal(c(catch(ple4_rel_catch)[,ac(2009:2018)] / catch(ple4_rel_catch)[,ac(2008:2017)]),
      rep(0.9, 10))
    expect_equal(c(predict(ple4_sr,ssb=ssb(ple4_rel_catch)[,ac(2008:2017)])),
      c(rec(ple4_rel_catch)[,ac(2009:2018)]))
    
    # Min Max
    
    f01 <- 0.1
    min_catch <- mean(catch(ple4_mtf)[,as.character(2006:2008)])
    ctrl_min_catch <- fwdControl(
      list(year=2009:2018, quant="f", value=f01), list(year=2009:2018, quant="catch", min=min_catch))
    ple4_min_catch <- fwd(ple4_mtf, control = ctrl_min_catch, sr = ple4_sr)
    ple4c <- catch(ple4_min_catch)[,ac(2009:2018)]
    
    # Expect a solving tolerance of better than 1e-6
    
    expect_true(all(ple4c > (min_catch - 1e-6)))

    # Relative targets and bounds
    
    current_fbar <- c(fbar(ple4)[,"2008"])
    f_target <- c(seq(from = current_fbar, to = f01, length = 8)[-1], rep(f01, 3))
    rel_catch_bound <- 0.10
    ctrl_rel_min_max_catch <- fwdControl(
    list(year=2009:2018, quant="f", value=f_target),
    list(year=2009:2018, quant="catch", relYear=2008:2017, max=1+rel_catch_bound, min=1-rel_catch_bound))
    recovery<-fwd(ple4_mtf, control=ctrl_rel_min_max_catch, sr=ple4_sr)
    ple4c <- catch(recovery)[,ac(2009:2018)]
    relcatch <- catch(recovery)[,ac(2009:2018)] / catch(recovery)[,ac(2008:2017)]
    
    # All relative catches within bounds within tolerance
    
    expect_true(all(relcatch > (1-rel_catch_bound - 1e6) & relcatch < (1+rel_catch_bound + 1e6)))
    
    # Prepare iterations
    
    niters <- 20
    ple4_mtf <- stf(ple4, nyears = 10)
    ple4_mtf <- propagate(ple4_mtf, niters)
    
    # deviances
    
    rec_deviances <- FLQuant(NA, dimnames = list(year=2009:2018, iter=1:niters))
    sample_years <- sample(dimnames(residuals(ple4_sr))$year, niters * 10, replace = TRUE)
    rec_deviances[] <- exp(residuals(ple4_sr)[,sample_years])
    ple4_stoch_rec <- fwd(ple4_mtf, control = ctrl_catch, sr = ple4_sr, deviances = rec_deviances) 
    expect_equal(c(catch(ple4_stoch_rec)[,ac(2009:2018)]),rep(future_catch,niters))
    expect_equal(c(predict(ple4_sr,ssb=ssb(ple4_stoch_rec)[,ac(2008:2017)]) %*% rec_deviances), c(rec(ple4_stoch_rec)[,ac(2009:2018)]))
    
    # Stochastic target
    
    stoch_catch  <- rlnorm(10*niters, meanlog=log(future_catch), sdlog=0.3)
    ctrl_catch_iters <- fwdControl(list(year=2009:2018, quant="catch", value=stoch_catch))
    ple4_catch_iters <- fwd(ple4_mtf, control=ctrl_catch_iters, sr = ple4_sr)
    expect_equal(c(catch(ple4_catch_iters)[,ac(2009:2018)]), stoch_catch)
    
    # Iterations in recruitment parameters
    
    sr_iters <- FLPar(NA, dimnames=list(params=c("a","b"), iter=1:niters))
    aiters <- rlnorm(niters, meanlog=log(params(ple4_sr)["a"]), sdlog=0.5)
    biters <- rlnorm(niters, meanlog=log(params(ple4_sr)["b"]), sdlog=0.01)
    sr_iters["a"] <- aiters
    sr_iters["b"] <- biters
    ple4_sr_iters <- fwd(ple4_mtf, control=ctrl_catch, sr = list(model="bevholt", params=sr_iters))
    expect_equal(c(catch(ple4_sr_iters)[,ac(2009:2018)]), rep(future_catch, niters))
    srmod <- predictModel(model=bevholt()$model, params=sr_iters)
    expect_equal(c(predict(srmod, ssb=ssb(ple4_sr_iters)[,ac(2008:2017)])), c(rec(ple4_sr_iters)[,ac(2009:2018)]))
    
    # Stochastic targets and recruitment
    
    ple4_iters <- fwd(ple4_mtf, control=ctrl_catch_iters, sr = list(model="bevholt", params=sr_iters), deviances = rec_deviances)
    expect_equal(c(catch(ple4_iters)[,ac(2009:2018)]), stoch_catch)
    expect_equal(c(predict(srmod, ssb=ssb(ple4_iters)[,ac(2008:2017)]) %*% rec_deviances), c(rec(ple4_iters)[,ac(2009:2018)]))

})


test_that("Fbar target, effort_max limiting",{
    data(ple4)
    control=fwdControl(year=1990, quant="fbar", value=200)
    expect_warning(res <- fwd(ple4, control=control, sr=predictModel(model="geomean", 
      params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)])))))
    f_out <- c(fbar(res)[,"1990"])
    expect_true(f_out < 200)

    control=fwdControl(year=1990, quant="fbar", value=5)
    expect_warning(res <- fwd(ple4, control=control, sr=predictModel(model="geomean", 
      params=FLPar(a=yearMeans(rec(ple4)[, ac(2006:2008)]))), effort_max=5))
    f_out <- c(fbar(res)[,"1990"])
    expect_true(f_out < 5)
})

test_that("control as FLQuants works", {
    data(ple4)
    
    # catch
    res <- fwd(ple4, catch=catch(ple4)[,ac(2000:2008)], 
      sr=predictModel(model="geomean",
        params=FLPar(a=yearMeans(rec(ple4)[, ac(2000:2008)]))))
    expect_equal(catch(ple4)[,ac(2000:2008)], catch(res)[,ac(2000:2008)])
    
    # landings
    res <- fwd(ple4, landings=landings(ple4)[,ac(2000:2008)], 
      sr=predictModel(model="geomean",
        params=FLPar(a=yearMeans(rec(ple4)[, ac(2000:2008)]))))
    expect_equal(landings(ple4)[,ac(2000:2008)], landings(res)[,ac(2000:2008)])
    
    # discards
    res <- fwd(ple4, discards=discards(ple4)[,ac(2000:2008)], 
      sr=predictModel(model="geomean",
        params=FLPar(a=yearMeans(rec(ple4)[, ac(2000:2008)]))))
    expect_equal(discards(ple4)[,ac(2000:2008)], discards(res)[,ac(2000:2008)])
    
    # fbar
    res <- fwd(ple4, fbar=fbar(ple4)[,ac(2000:2008)], 
      sr=predictModel(model="geomean",
        params=FLPar(a=yearMeans(rec(ple4)[, ac(2000:2008)]))))
    expect_equal(fbar(ple4)[,ac(2000:2008)], fbar(res)[,ac(2000:2008)])

    # f
    res <- fwd(ple4, f=fbar(ple4)[,ac(2000:2008)], 
      sr=predictModel(model="geomean",
        params=FLPar(a=yearMeans(rec(ple4)[, ac(2000:2008)]))))
    expect_equal(fbar(ple4)[,ac(2000:2008)], fbar(res)[,ac(2000:2008)])
})
