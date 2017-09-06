# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Simple projections with FLStock work")
# If these don't work we are in trouble!
source("expect_funs.R")


test_that("Catch target, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    catch_val <- rlnorm(n=nyears, mean=log(min(catch(ple4)/10)), sd=0.1)
    control=fwdControl(data.frame(year=years, quant="catch", value=catch_val))
    res <- fwd(ple4, control=control)
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
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    catch_val <- rlnorm(n=nyears*niters, mean=log(min(catch(ple4p)/10)), sd=0.1)
    control=fwdControl(data.frame(year=years, quant="catch", value=0), iters=niters)
    control@iters[,"value",] <- catch_val
    res <- fwd(ple4p, control=control)
    catch_out <- c(catch(res)[,ac(years)])
    # Test to within tolerance of 1.5e-8
    expect_equal(catch_out, catch_val)
})

test_that("Fbar target, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    f_val <- rlnorm(n=nyears, mean=log(min(fbar(ple4)/10)), sd=0.1)
    control=fwdControl(data.frame(year=years, quant="fbar", value=f_val))
    res <- fwd(ple4, control=control)
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
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    f_val <- rlnorm(n=nyears*niters, mean=log(min(fbar(ple4p)/10)), sd=0.1)
    control=fwdControl(data.frame(year=years, quant="fbar", value=0), iters=niters)
    control@iters[,"value",] <- f_val 
    res <- fwd(ple4p, control=control)
    f_out <- c(fbar(res)[,ac(years)])
    # Test to within tolerance of 1.5e-8
    expect_equal(f_out, f_val)
})

test_that("Catch target with min limit, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    catch_val <- rlnorm(n=nyears, mean=log(min(catch(ple4)/10)), sd=0.1)
    catch_lim <- mean(catch_val)
    control=fwdControl(
        rbind(data.frame(year=years, quant="catch", value=catch_val, min=NA),
            data.frame(year=years, quant="catch", value=NA, min=catch_lim))
    )
    res <- fwd(ple4, control=control)
    catch_out <- c(catch(res)[,ac(years)])
    catch_trg <- catch_val
    catch_trg[catch_val < catch_lim] <- catch_lim
    # Test to within tolerance of 1.5e-8
    expect_equal(catch_out, catch_trg)
})

test_that("Catch target with max limit, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    catch_val <- rlnorm(n=nyears, mean=log(min(catch(ple4)/10)), sd=0.1)
    catch_lim <- mean(catch_val)
    control=fwdControl(
        rbind(data.frame(year=years, quant="catch", value=catch_val, max=NA),
            data.frame(year=years, quant="catch", value=NA, max=catch_lim))
    )
    res <- fwd(ple4, control=control)
    catch_out <- c(catch(res)[,ac(years)])
    catch_trg <- catch_val
    catch_trg[catch_val > catch_lim] <- catch_lim
    # Test to within tolerance of 1.5e-8
    expect_equal(catch_out, catch_trg)
})

test_that("Catch relative target, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1]+1, max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    catch_rel_val <- runif(nyears, min=0.5, max=0.9)
    control=fwdControl(data.frame(year=years, quant="catch", value=catch_rel_val, relYear=years-1))
    res <- fwd(ple4, control=control)
    catch_out <- c(catch(res)[,ac(years)])
    catch_trg <- c(catch(ple4)[,ac(years[1]-1)]) * cumprod(catch_rel_val)
    # Test to within tolerance of 1.5e-8
    expect_equal(catch_out, catch_trg)
})

test_that("Fbar relative target, single iter",{
    data(ple4)
    year_range <- range(ple4)[c("minyear","maxyear")]
    min_year <- round(runif(1, min=year_range[1], max=(year_range[1] + round((year_range[2] - year_range[1])/2))))
    nyears <- round(runif(n=1,min=3,max=(round((year_range[2] - year_range[1])/2)-5)))
    years <- min_year:(min_year+nyears-1)
    f_rel_val <- runif(nyears, min=0.5, max=0.9)
    control=fwdControl(data.frame(year=years, quant="fbar", value=f_rel_val, relYear=years-1))
    res <- fwd(ple4, control=control)
    f_out <- c(fbar(res)[,ac(years)])
    f_trg <- c(fbar(ple4)[,ac(years[1]-1)]) * cumprod(f_rel_val)
    # Test to within tolerance of 1.5e-8
    expect_equal(f_out, f_trg)
})





