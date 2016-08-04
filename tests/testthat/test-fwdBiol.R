# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Implementation of fwdBiol and fwdBiols - double and AD versions")
source("expect_funs.R")

test_that("fwdBiol as and wrap",{
    flb_in <- random_FLBiolcpp_generator()
    flb_out <- test_fwdBiol_as_wrap(flb_in)
    expect_identical(flb_in, flb_out)
    flb_out <- test_fwdBiolAD_as_wrap(flb_in)
    expect_identical(flb_in, flb_out)
})

test_that("fwdBiol constructors - double",{
    flb_in <- random_FLBiolcpp_generator(min_dims=c(3,3,1,1,1,1))
    # SEXP constructors
    flb_out <- test_fwdBiol_sexp_constructor(flb_in)
    expect_identical(flb_in, flb_out)
    flb_out <- test_fwdBiolAD_sexp_constructor(flb_in)
    expect_identical(flb_in, flb_out)
    # FLBiolcpp fwdSR constructor
    data(ple4)
    ricker <- fmle(as.FLSR(ple4,model="ricker"), control  = list(trace=0))
    sr_params <- as.FLQuant(params(ricker))
    too_small_residuals <- FLQuant(1, dim=c(1, dim(n(flb_in))[2]-1, dim(n(flb_in))[3], dim(n(flb_in))[4], dim(n(flb_in))[5], 1)) 
    residuals <- FLQuant(1, dim=c(1, dim(n(flb_in))[2], dim(n(flb_in))[3], dim(n(flb_in))[4], dim(n(flb_in))[5], 1)) 
    residuals_mult <- TRUE
    expect_error(test_fwdBiol_fwdSR_constructor(flb_in, "ricker", sr_params, too_small_residuals, residuals_mult))
    out <- test_fwdBiol_fwdSR_constructor(flb_in, "ricker", sr_params, residuals, residuals_mult)
    expect_identical(out[["fwb"]], flb_in)
    expect_identical(c(out[["srr"]][["params"]]), c(sr_params))
    expect_identical(out[["srr"]][["residuals"]], residuals)
    expect_identical(out[["srr"]][["residuals_mult"]], residuals_mult)
    expect_error(test_fwdBiol_fwdSRAD_constructor(flb_in, "ricker", sr_params, too_small_residuals, residuals_mult))
    out <- test_fwdBiol_fwdSR_constructor(flb_in, "ricker", sr_params, residuals, residuals_mult)
    expect_identical(out[["fwb"]], flb_in)
    expect_identical(c(out[["srr"]][["params"]]), c(sr_params))
    expect_identical(out[["srr"]][["residuals"]], residuals)
    expect_identical(out[["srr"]][["residuals_mult"]], residuals_mult)
    # FLBiolcpp SR bits constructor
    out <- test_fwdBiol_FLSR_bits_constructor(flb_in, "ricker", sr_params, residuals, residuals_mult)
    expect_identical(out[["fwb"]], flb_in)
    expect_identical(c(out[["srr"]][["params"]]), c(sr_params))
    expect_identical(out[["srr"]][["residuals"]], residuals)
    expect_identical(out[["srr"]][["residuals_mult"]], residuals_mult)
    out <- test_fwdBiolAD_FLSR_bits_constructor(flb_in, "ricker", sr_params, residuals, residuals_mult)
    expect_identical(out[["fwb"]], flb_in)
    expect_identical(c(out[["srr"]][["params"]]), c(sr_params))
    expect_identical(out[["srr"]][["residuals"]], residuals)
    expect_identical(out[["srr"]][["residuals_mult"]], residuals_mult)
    # Copy constructor
    flb_out <- test_fwdBiol_copy_constructor(flb_in)
    expect_identical(flb_in, flb_out)
    flb_out <- test_fwdBiolAD_copy_constructor(flb_in)
    expect_identical(flb_in, flb_out)
    # Copy constructor2
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    value <- rnorm(1)
    # Makes a copy of flq_in, changes a value of flq_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flbs <-  test_fwdBiol_copy_constructor2(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flb_in, flbs[["fwdb1"]])
    expect_identical(c(n(flbs[["fwdb2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    flbs <-  test_fwdBiolAD_copy_constructor2(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flb_in, flbs[["fwdb1"]])
    expect_identical(c(n(flbs[["fwdb2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Assignment operator
    flb_out <- test_fwdBiol_assignment_operator(flb_in)
    expect_identical(flb_in, flb_out)
    flb_out <- test_fwdBiolAD_assignment_operator(flb_in)
    expect_identical(flb_in, flb_out)
    # Assignment operator2
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    value <- rnorm(1)
    # Makes a copy of flq_in, changes a value of flq_in, returns original and new FLQuant
    # Checks that the assignment operator makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flbs <-  test_fwdBiol_assignment_operator2(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flb_in, flbs[["fwdb1"]])
    expect_identical(c(n(flbs[["fwdb2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    flbs <-  test_fwdBiolAD_assignment_operator2(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flb_in, flbs[["fwdb1"]])
    expect_identical(c(n(flbs[["fwdb2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
})

test_that("fwdBiol get and set data accessors", {
    # Get const double
    flb_in <- random_FLBiolcpp_generator()
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    # fwdBiol const get
    values_out <- test_fwdBiol_const_get_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(n(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(values_out, values_in)
    # Get const subset
    dims_max <- dim(n(flb_in))
    dims_min <- round(runif(6, min=1, max=dims_max))
    out <- test_fwdBiol_const_get_accessors_subset(flb_in, dims_min, dims_max)
    expect_equal(out[["n"]], n(flb_in)[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]])
    expect_equal(out[["m"]], m(flb_in)[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]])
    expect_equal(out[["wt"]], wt(flb_in)[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]]) 
    expect_equal(out[["fec"]], fec(flb_in)[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]]) 
    expect_equal(out[["spwn"]], spwn(flb_in)[1, dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]]) 
    expect_equal(out[["mat"]], mat(flb_in)[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]]) 
    # Get const AD
    values_out <- test_fwdBiolAD_const_get_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(n(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(values_out, values_in)
    # Get double
    flb_in <- random_FLBiolcpp_generator()
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    values_out <- test_fwdBiol_get_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(n(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(values_out, values_in)
    # Get AD
    values_out <- test_fwdBiolAD_get_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(n(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(values_out, values_in)
    # Set double
    flb_in <- random_FLBiolcpp_generator()
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    values_in <- rnorm(6)
    flb_out <- test_fwdBiol_set_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], values_in)
    # Check inserted values are correct
    values_out <- c(c(n(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_out)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(values_out, values_in)
    # Check other values have been left alone
    narea <- dim(n(flb_out))[5]
    nseason <- dim(n(flb_out))[4]
    nunit<- dim(n(flb_out))[3]
    nyear <- dim(n(flb_out))[2]
    nquant <- dim(n(flb_out))[1]
    element <- (narea * nseason * nunit * nyear * nquant * (indices[6] - 1)) + (nseason * nunit * nyear * nquant * (indices[5] - 1)) + (nunit * nyear * nquant * (indices[4] - 1)) + (nyear * nquant * (indices[3] - 1)) + (nquant * (indices[2] - 1)) + (indices[1] - 1) + 1; 
    nquant <- 1
    spwn_element <- (narea * nseason * nunit * nyear * nquant * (indices[6] - 1)) + (nseason * nunit * nyear * nquant * (indices[5] - 1)) + (nunit * nyear * nquant * (indices[4] - 1)) + (nyear * nquant * (indices[3] - 1)) + (nquant * (indices[2] - 1)) + 1; 
    expect_identical(c(n(flb_out))[-element], c(n(flb_in))[-element])
    expect_identical(c(m(flb_out))[-element], c(m(flb_in))[-element])
    expect_identical(c(wt(flb_out))[-element], c(wt(flb_in))[-element])
    expect_identical(c(fec(flb_out))[-element], c(fec(flb_in))[-element])
    expect_identical(c(spwn(flb_out))[-spwn_element], c(spwn(flb_in))[-spwn_element])
    expect_identical(c(mat(flb_out))[-element], c(mat(flb_in))[-element])
    # Set AD 
    flb_in <- random_FLBiolcpp_generator()
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    values_in <- rnorm(6)
    flb_out <- test_fwdBiolAD_set_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], values_in)
    # Check inserted values are correct
    values_out <- c(c(n(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_out)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(values_out, values_in)
    # Check other values have been left alone
    narea <- dim(n(flb_out))[5]
    nseason <- dim(n(flb_out))[4]
    nunit<- dim(n(flb_out))[3]
    nyear <- dim(n(flb_out))[2]
    nquant <- dim(n(flb_out))[1]
    element <- (narea * nseason * nunit * nyear * nquant * (indices[6] - 1)) + (nseason * nunit * nyear * nquant * (indices[5] - 1)) + (nunit * nyear * nquant * (indices[4] - 1)) + (nyear * nquant * (indices[3] - 1)) + (nquant * (indices[2] - 1)) + (indices[1] - 1) + 1; 
    nquant <- 1
    spwn_element <- (narea * nseason * nunit * nyear * nquant * (indices[6] - 1)) + (nseason * nunit * nyear * nquant * (indices[5] - 1)) + (nunit * nyear * nquant * (indices[4] - 1)) + (nyear * nquant * (indices[3] - 1)) + (nquant * (indices[2] - 1)) + 1; 
    expect_identical(c(n(flb_out))[-element], c(n(flb_in))[-element])
    expect_identical(c(m(flb_out))[-element], c(m(flb_in))[-element])
    expect_identical(c(wt(flb_out))[-element], c(wt(flb_in))[-element])
    expect_identical(c(fec(flb_out))[-element], c(fec(flb_in))[-element])
    expect_identical(c(spwn(flb_out))[-spwn_element], c(spwn(flb_in))[-spwn_element])
    expect_identical(c(mat(flb_out))[-element], c(mat(flb_in))[-element])
})

test_that("fwdBiol get and set direct data accessors", {
    flb_in <- random_FLBiolcpp_generator()
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    value <- rnorm(1)
    # Get
    out <- test_fwdBiolAD_n_direct_get_accessor(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(c(n(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), out)
    # Set
    flb_out <- test_fwdBiolAD_n_direct_set_accessor(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(c(n(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
})

test_that("fwdBiol methods",{
    # Biomass FLQuant
    flb_in <- random_FLBiolcpp_generator()
    biomass <-  fwdBiolAD_biomass_FLQ(flb_in)
    expect_equal(biomass@.Data, quantSums(n(flb_in) * wt(flb_in))@.Data)
    # Biomass FLQuant subset
    dims_max <- dim(n(flb_in))
    dims_min <- round(runif(6, min=1,max=dims_max))
    expect_error(fwdBiolAD_biomass_subset(flb_in, dims_min, dims_max))
    biomass <-  fwdBiolAD_biomass_subset(flb_in, dims_min[-1], dims_max[-1])
    expect_equal(biomass@.Data, quantSums(n(flb_in) * wt(flb_in))[,dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]]@.Data)
})


test_that("fwdBiol srp_timelag",{
    # first age 0, no seasons
    flq_in <- FLQuant(NA, dimnames=list(age=0:5, year=1:10, iter=1:100))
    flb_in <- as(FLBiol(flq_in), "FLBiolcpp")
    desc(flb_in) <- "eh"
    timelag <- test_fwdBiol_srp_timelag(flb_in)
    expect_identical(timelag, 0)
    # first age 0, with 12 seasons
    flq_in <- FLQuant(NA, dimnames=list(age=0:5, year=1:10, season=1:12, iter=1:100))
    flb_in <- as(FLBiol(flq_in), "FLBiolcpp")
    desc(flb_in) <- "eh"
    timelag <- test_fwdBiol_srp_timelag(flb_in)
    expect_identical(timelag, 1)
    # first age anything, with no seasons
    min_age <- round(runif(1, min=1, max=5))
    max_age <- round(runif(1, min=min_age, max=10))
    flq_in <- FLQuant(NA, dimnames=list(age=min_age:max_age, year=1:10, iter=1:100))
    flb_in <- as(FLBiol(flq_in), "FLBiolcpp")
    desc(flb_in) <- "eh"
    timelag <- test_fwdBiol_srp_timelag(flb_in)
    expect_identical(timelag, min_age)
    # first age anything, with seasons
    min_age <- round(runif(1, min=1, max=5))
    max_age <- round(runif(1, min=min_age, max=10))
    nseasons <- round(runif(1, min=2, max=10))
    flq_in <- FLQuant(NA, dimnames=list(age=min_age:max_age, year=1:10, season=1:nseasons, iter=1:100))
    flb_in <- as(FLBiol(flq_in), "FLBiolcpp")
    desc(flb_in) <- "eh"
    timelag <- test_fwdBiol_srp_timelag(flb_in)
    expect_identical(timelag, nseasons*min_age)
})

test_that("fwdBiol does_recruitment_happen",{
    nyears <- 10
    niters <- 100
    flq_in <- FLQuant(NA, dimnames=list(age=0:5, unit=1:4, year=1:nyears, season=1:4,iter=niters))
    flb_in <- as(FLBiol(flq_in), "FLBiolcpp")
    desc(flb_in) <- "The Professor of Dub"
    residuals <- FLQuant(rnorm(100), dimnames = list(year = 1:nyears, unit=1:4, season=1:4, iter = 1:niters))
    residuals_mult <- TRUE
    # With 4 Seasons and 4 Units
    # Units 1 and 2 are M/F recruiting in Season 1
    # Units 3 and 4 are M/F recruiting in Season 3
    sr_params <- FLQuant(NA, dim=c(2,1,4,4,1,1)) 
    sr_params[,,c(1,2),1,] <- rnorm(4)
    sr_params[,,c(3,4),3,] <- rnorm(4)
    # Test season 1 in any year
    season1_timesteps <- seq(from = 1, to=4 * nyears - 3, by=4)
    season2_timesteps <- seq(from = 2, to=4 * nyears - 2, by=4)
    season3_timesteps <- seq(from = 3, to=4 * nyears - 1, by=4)
    season4_timesteps <- seq(from = 4, to=4 * nyears, by=4)
    # Season 1
    expect_true(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 1, sample(season1_timesteps, 1)))
    expect_true(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 2, sample(season1_timesteps, 1)))
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 3, sample(season1_timesteps, 1)))
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 4, sample(season1_timesteps, 1)))
    # Season 2
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 1, sample(season2_timesteps, 1)))
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 2, sample(season2_timesteps, 1)))
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 3, sample(season2_timesteps, 1)))
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 4, sample(season2_timesteps, 1)))
    # Season 3
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 1, sample(season3_timesteps, 1)))
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 2, sample(season3_timesteps, 1)))
    expect_true(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 3, sample(season3_timesteps, 1)))
    expect_true(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 4, sample(season3_timesteps, 1)))
    # Season 4
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 1, sample(season4_timesteps, 1)))
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 2, sample(season4_timesteps, 1)))
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 3, sample(season4_timesteps, 1)))
    expect_false(test_fwdBiol_does_recruitment_happen(flb_in, "Ricker", sr_params, residuals, residuals_mult, 4, sample(season4_timesteps, 1)))
})




#------------------------------------------------------------------------------
# fwdBiols
test_that("fwdBiols constructors",{
    # Takes a list, list_fwdBiol
    # Each element of list_fwdBiol is a list containing the fwdBiol components:
    # FLBiolcpp, params, residuals, residuals_mult 
    biols <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5)
    # Scrape out the FLBiolcpps only (to test the wrap)
    flbs_in <- lapply(biols, function(x) return(x[["biol"]]))
    # as constructor
    flbs_out <- test_fwdBiols_as_wrap(biols)
    expect_identical(flbs_out, flbs_in)
    flbs_ad_out <- test_fwdBiolsAD_as_wrap(biols)
    expect_identical(flbs_ad_out, flbs_in)
    # fwdBiols constructor
    biol_no <- round(runif(1,min=1,max=length(biols)))
    flbs_out <- test_fwdBiolsAD_fwdBiolAD_constructor(biols[[biol_no]][["biol"]],
                                      biols[[biol_no]][["srr_model_name"]],
                                      biols[[biol_no]][["srr_params"]],
                                      biols[[biol_no]][["srr_residuals"]],
                                      biols[[biol_no]][["srr_residuals_mult"]])
    expect_identical(length(flbs_out), 1L)
    expect_identical(flbs_out[[1]], flbs_in[[biol_no]])
    # Copy constructor
    indices <- round(runif(6, min=1, max=dim(n(biols[[biol_no]][["biol"]]))))
    value <- abs(rnorm(1))
    out <- test_fwdBiolsAD_copy_constructor(biols, biol_no, indices, value)
    # Original should have changed
    expect_identical(c(n(out[[1]][[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Copy should be same as original original
    expect_identical(c(n(out[[2]][[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), c(n(flbs_in[[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # Assignment constructor
    value <- abs(rnorm(1))
    out <- test_fwdBiolsAD_assignment_operator(biols, biol_no, indices, value)
    # Original should have changed
    expect_identical(c(n(out[[1]][[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Copy should be same as original original
    expect_identical(c(n(out[[2]][[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), c(n(flbs_in[[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(c(n(out[[2]][[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), c(n(flbs_in[[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
})

test_that("fwdBiols methods",{
    biols <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5)
    biol_no <- round(runif(1,min=1,max=length(biols)))
    flbs_in <- lapply(biols, function(x) return(x[["biol"]]))
    # Get (const)
    flb_out <- test_fwdBiolsAD_const_get_single_index_accessor(biols, biol_no)
    expect_identical(flb_out, flbs_in[[biol_no]])
    # Get 
    flb_out <- test_fwdBiolsAD_get_single_index_accessor(biols, biol_no)
    expect_identical(flb_out, flbs_in[[biol_no]])
    # Get value const
    indices <- round(runif(6, min=1, max=dim(n(biols[[biol_no]][["biol"]]))))
    out <- test_fwdBiolsAD_const_get_value_accessor(biols, biol_no, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(c(n(biols[[biol_no]][["biol"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), out)
    out <- test_fwdBiolsAD_get_value_accessor(biols, biol_no, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(c(n(biols[[biol_no]][["biol"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), out)
    # Set biol
    biol_no2 <- round(runif(1,min=1,max=length(biols)))
    flbs_out <- test_fwdBiolsAD_set_single_index_accessor(biols, biol_no, biols[[biol_no2]][["biol"]],
                                      biols[[biol_no2]][["srr_model_name"]],
                                      biols[[biol_no2]][["srr_params"]],
                                      biols[[biol_no2]][["srr_residuals"]],
                                      biols[[biol_no2]][["srr_residuals_mult"]])
    expect_identical(flbs_out[[biol_no]], flbs_in[[biol_no2]])
    # Set value
    value <- abs(rnorm(1))
    flbs_out <- test_fwdBiolsAD_set_value_accessor(biols, biol_no, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(c(n(flbs_out[[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
})

test_that("fwdBiols iterators",{
    # Biols of same size
    fixed_dims <- round(runif(6,min=2,max=10))
    biols <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims=fixed_dims)
    flbs_in <- lapply(biols, function(x) return(x[["biol"]]))
    nin <- lapply(flbs_in, function(x) return(n(x)))
    # Const - just pulls out n - does nit do names
    nout <- test_fwdBiolsAD_const_iterator(biols)
    expect_identical(nout, unname(nin))
    # Not const - sets a value
    indices <- round(runif(6, min=1, max=dim(n(flbs_in[[1]]))))
    value <- rnorm(1)
    biols_out <- test_fwdBiolsAD_iterator(biols, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    value_out <- unname(unlist(lapply(biols_out, function(x) return(n(x)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))))
    expect_identical(rep(value,length(flbs_in)), value_out)
    # All others are OK
    element <- get_FLQuant_element(n(flbs_in[[1]]), indices)
    for (i in 1:length(flbs_in)){
        expect_identical(c(n(biols_out[[i]]))[-element], c(n(flbs_in[[i]]))[-element])
    }
})


