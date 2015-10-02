context("Implementation of fwdBiol and fwdBiols - double and AD versions")

test_that("fwdBiol as and wrap",{
    flb_in <- random_FLBiol_generator()
    flb_out <- test_fwdBiol_as_wrap(flb_in)
    expect_that(flb_in, is_identical_to(flb_out))
    flb_out <- test_fwdBiolAD_as_wrap(flb_in)
    expect_that(flb_in, is_identical_to(flb_out))
})

test_that("fwdBiol constructors - double",{
    flb_in <- random_FLBiol_generator()
    # SEXP constructors
    flb_out <- test_fwdBiol_sexp_constructor(flb_in)
    expect_that(flb_in, is_identical_to(flb_out))
    flb_out <- test_fwdBiolAD_sexp_constructor(flb_in)
    expect_that(flb_in, is_identical_to(flb_out))
    # FLBiol fwdSR constructor
    data(ple4)
    ricker <- fmle(as.FLSR(ple4,model="ricker"), control  = list(trace=0))
    sr_params <- as.FLQuant(params(ricker))
    too_small_residuals <- FLQuant(1, dim=c(1, dim(n(flb_in))[2]-1, dim(n(flb_in))[3], dim(n(flb_in))[4], dim(n(flb_in))[5], 1)) 
    residuals <- FLQuant(1, dim=c(1, dim(n(flb_in))[2], dim(n(flb_in))[3], dim(n(flb_in))[4], dim(n(flb_in))[5], 1)) 
    residuals_mult <- TRUE
    expect_error(test_fwdBiol_fwdSR_constructor(flb_in, "ricker", sr_params, too_small_residuals, residuals_mult))
    out <- test_fwdBiol_fwdSR_constructor(flb_in, "ricker", sr_params, residuals, residuals_mult)
    expect_that(out[["fwb"]], is_identical_to(flb_in))
    expect_that(c(out[["srr"]][["params"]]), is_identical_to(c(sr_params)))
    expect_that(out[["srr"]][["residuals"]], is_identical_to(residuals))
    expect_that(out[["srr"]][["residuals_mult"]], is_identical_to(residuals_mult))
    expect_error(test_fwdBiol_fwdSRAD_constructor(flb_in, "ricker", sr_params, too_small_residuals, residuals_mult))
    out <- test_fwdBiol_fwdSR_constructor(flb_in, "ricker", sr_params, residuals, residuals_mult)
    expect_that(out[["fwb"]], is_identical_to(flb_in))
    expect_that(c(out[["srr"]][["params"]]), is_identical_to(c(sr_params)))
    expect_that(out[["srr"]][["residuals"]], is_identical_to(residuals))
    expect_that(out[["srr"]][["residuals_mult"]], is_identical_to(residuals_mult))
    # FLBiol SR bits constructor
    out <- test_fwdBiol_FLSR_bits_constructor(flb_in, "ricker", sr_params, residuals, residuals_mult)
    expect_that(out[["fwb"]], is_identical_to(flb_in))
    expect_that(c(out[["srr"]][["params"]]), is_identical_to(c(sr_params)))
    expect_that(out[["srr"]][["residuals"]], is_identical_to(residuals))
    expect_that(out[["srr"]][["residuals_mult"]], is_identical_to(residuals_mult))
    out <- test_fwdBiolAD_FLSR_bits_constructor(flb_in, "ricker", sr_params, residuals, residuals_mult)
    expect_that(out[["fwb"]], is_identical_to(flb_in))
    expect_that(c(out[["srr"]][["params"]]), is_identical_to(c(sr_params)))
    expect_that(out[["srr"]][["residuals"]], is_identical_to(residuals))
    expect_that(out[["srr"]][["residuals_mult"]], is_identical_to(residuals_mult))
    # Copy constructor
    flb_out <- test_fwdBiol_copy_constructor(flb_in)
    expect_that(flb_in, is_identical_to(flb_out))
    flb_out <- test_fwdBiolAD_copy_constructor(flb_in)
    expect_that(flb_in, is_identical_to(flb_out))
    # Copy constructor2
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    value <- rnorm(1)
    # Makes a copy of flq_in, changes a value of flq_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flbs <-  test_fwdBiol_copy_constructor2(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flb_in, is_identical_to(flbs[["fwdb1"]]))
    expect_that(c(n(flbs[["fwdb2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    flbs <-  test_fwdBiolAD_copy_constructor2(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flb_in, is_identical_to(flbs[["fwdb1"]]))
    expect_that(c(n(flbs[["fwdb2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Assignment operator
    flb_out <- test_fwdBiol_assignment_operator(flb_in)
    expect_that(flb_in, is_identical_to(flb_out))
    flb_out <- test_fwdBiolAD_assignment_operator(flb_in)
    expect_that(flb_in, is_identical_to(flb_out))
    # Assignment operator2
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    value <- rnorm(1)
    # Makes a copy of flq_in, changes a value of flq_in, returns original and new FLQuant
    # Checks that the assignment operator makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flbs <-  test_fwdBiol_assignment_operator2(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flb_in, is_identical_to(flbs[["fwdb1"]]))
    expect_that(c(n(flbs[["fwdb2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    flbs <-  test_fwdBiolAD_assignment_operator2(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flb_in, is_identical_to(flbs[["fwdb1"]]))
    expect_that(c(n(flbs[["fwdb2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
})

test_that("fwdBiol get and set data accessors", {
    # Get const double
    flb_in <- random_FLBiol_generator()
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    # fwdBiol const get
    values_out <- test_fwdBiol_const_get_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(n(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_that(values_out, is_identical_to(values_in))
    # Get const subset
    dims_max <- dim(n(flb_in))
    dims_min <- round(runif(6, min=1, max=dims_max))
    out <- test_fwdBiol_const_get_accessors_subset(flb_in, dims_min, dims_max)
    expect_that(out[["n"]], equals(n(flb_in)[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]])) 
    expect_that(out[["m"]], equals(m(flb_in)[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]])) 
    expect_that(out[["wt"]], equals(wt(flb_in)[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]])) 
    expect_that(out[["fec"]], equals(fec(flb_in)[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]])) 
    expect_that(out[["spwn"]], equals(spwn(flb_in)[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]])) 
    expect_that(out[["mat"]], equals(mat(flb_in)[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]])) 
    # Get const AD
    values_out <- test_fwdBiolAD_const_get_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(n(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_that(values_out, is_identical_to(values_in))
    # Get double
    flb_in <- random_FLBiol_generator()
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    values_out <- test_fwdBiol_get_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(n(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_that(values_out, is_identical_to(values_in))
    # Get AD
    values_out <- test_fwdBiolAD_get_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(n(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_that(values_out, is_identical_to(values_in))
    # Set double
    flb_in <- random_FLBiol_generator()
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    values_in <- rnorm(6)
    flb_out <- test_fwdBiol_set_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], values_in)
    # Check inserted values are correct
    values_out <- c(c(n(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_that(values_out, is_identical_to(values_in))
    # Check other values have been left alone
    narea <- dim(n(flb_out))[5]
    nseason <- dim(n(flb_out))[4]
    nunit<- dim(n(flb_out))[3]
    nyear <- dim(n(flb_out))[2]
    nquant <- dim(n(flb_out))[1]
    element <- (narea * nseason * nunit * nyear * nquant * (indices[6] - 1)) + (nseason * nunit * nyear * nquant * (indices[5] - 1)) + (nunit * nyear * nquant * (indices[4] - 1)) + (nyear * nquant * (indices[3] - 1)) + (nquant * (indices[2] - 1)) + (indices[1] - 1) + 1; 
    expect_that(c(n(flb_out))[-element], is_identical_to(c(n(flb_in))[-element]))
    expect_that(c(m(flb_out))[-element], is_identical_to(c(m(flb_in))[-element]))
    expect_that(c(wt(flb_out))[-element], is_identical_to(c(wt(flb_in))[-element]))
    expect_that(c(fec(flb_out))[-element], is_identical_to(c(fec(flb_in))[-element]))
    expect_that(c(spwn(flb_out))[-element], is_identical_to(c(spwn(flb_in))[-element]))
    expect_that(c(mat(flb_out))[-element], is_identical_to(c(mat(flb_in))[-element]))
    # Set AD 
    flb_in <- random_FLBiol_generator()
    indices <- round(runif(6,min=1, max = dim(n(flb_in))))
    values_in <- rnorm(6)
    flb_out <- test_fwdBiolAD_set_accessors(flb_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], values_in)
    # Check inserted values are correct
    values_out <- c(c(n(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(m(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(wt(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fec(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(spwn(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(mat(flb_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_that(values_out, is_identical_to(values_in))
    # Check other values have been left alone
    narea <- dim(n(flb_out))[5]
    nseason <- dim(n(flb_out))[4]
    nunit<- dim(n(flb_out))[3]
    nyear <- dim(n(flb_out))[2]
    nquant <- dim(n(flb_out))[1]
    element <- (narea * nseason * nunit * nyear * nquant * (indices[6] - 1)) + (nseason * nunit * nyear * nquant * (indices[5] - 1)) + (nunit * nyear * nquant * (indices[4] - 1)) + (nyear * nquant * (indices[3] - 1)) + (nquant * (indices[2] - 1)) + (indices[1] - 1) + 1; 
    expect_that(c(n(flb_out))[-element], is_identical_to(c(n(flb_in))[-element]))
    expect_that(c(m(flb_out))[-element], is_identical_to(c(m(flb_in))[-element]))
    expect_that(c(wt(flb_out))[-element], is_identical_to(c(wt(flb_in))[-element]))
    expect_that(c(fec(flb_out))[-element], is_identical_to(c(fec(flb_in))[-element]))
    expect_that(c(spwn(flb_out))[-element], is_identical_to(c(spwn(flb_in))[-element]))
    expect_that(c(mat(flb_out))[-element], is_identical_to(c(mat(flb_in))[-element]))
})

test_that("fwdBiol methods",{
    # Biomass FLQuant
    flb_in <- random_FLBiol_generator()
    biomass <-  fwdBiolAD_biomass_FLQ(flb_in)
    expect_that(biomass@.Data, equals(quantSums(n(flb_in) * wt(flb_in))@.Data))
    # Biomass FLQuant subset
    dims_max <- dim(n(flb_in))
    dims_min <- round(runif(6, min=1,max=dims_max))
    biomass <-  expect_that(fwdBiolAD_biomass_subset(flb_in, dims_min, dims_max), throws_error())
    biomass <-  fwdBiolAD_biomass_subset(flb_in, dims_min[-1], dims_max[-1])
    expect_that(biomass@.Data, equals(quantSums(n(flb_in) * wt(flb_in))[,dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]]@.Data))
})


test_that("fwdBiols constructors",{
    # Takes a list, list_fwdBiol
    # Each element of list_fwdBiol is a list containing the fwdBiol components:
    # FLBiol, params, residuals, timelag, residuals_mult 
    biols <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5)
    # Scrape out the FLBiols only (to test the wrap)
    flbs_in <- FLBiols(lapply(biols, function(x) return(x[["biol"]])))

    # List constructors 
    flbs_out <- test_fwdBiols_list_constructor(biols)
    expect_that(flbs_out, is_identical_to(flbs_in))
    flbs_ad_out <- test_fwdBiolsAD_list_constructor(biols)
    expect_that(flbs_ad_out, is_identical_to(flbs_in))

    # fwdBiol constructor
    biol_no <- round(runif(1,min=1,max=length(biols)))
    flbs_out <- test_fwdBiolsAD_fwdBiolAD_constructor(biols[[biol_no]][["biol"]],
                                      biols[[biol_no]][["srr_model_name"]],
                                      biols[[biol_no]][["srr_params"]],
                                      biols[[biol_no]][["srr_residuals"]],
                                      biols[[biol_no]][["srr_residuals_mult"]])
    expect_that(length(flbs_out), is_identical_to(1L))
    expect_that(flbs_out[[1]], is_identical_to(flbs_in[[biol_no]]))

    # Copy constructor
    indices <- round(runif(6, min=1, max=dim(n(biols[[biol_no]][["biol"]]))))
    value <- abs(rnorm(1))
    out <- test_fwdBiolsAD_copy_constructor(biols, biol_no, indices, value)
    # Original should have changed
    expect_that(c(n(out[[1]][[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Copy should be same as original original
    expect_that(c(n(out[[2]][[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(c(n(flbs_in[[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))

    # Assignment constructor
    value <- abs(rnorm(1))
    out <- test_fwdBiolsAD_assignment_operator(biols, biol_no, indices, value)
    # Original should have changed
    expect_that(c(n(out[[1]][[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Copy should be same as original original
    expect_that(c(n(out[[2]][[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(c(n(flbs_in[[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
})

test_that("fwdBiols methods",{
    biols <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5)
    biol_no <- round(runif(1,min=1,max=length(biols)))
    flbs_in <- FLBiols(lapply(biols, function(x) return(x[["biol"]])))
    # Get (const)
    flb_out <- test_fwdBiolsAD_const_get_single_index_accessor(biols, biol_no)
    expect_that(flb_out, is_identical_to(flbs_in[[biol_no]]))
    # Get 
    flb_out <- test_fwdBiolsAD_get_single_index_accessor(biols, biol_no)
    expect_that(flb_out, is_identical_to(flbs_in[[biol_no]]))
    # Get value const
    indices <- round(runif(6, min=1, max=dim(n(biols[[biol_no]][["biol"]]))))
    out <- test_fwdBiolsAD_const_get_value_accessor(biols, biol_no, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(c(n(biols[[biol_no]][["biol"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(out))
    out <- test_fwdBiolsAD_get_value_accessor(biols, biol_no, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(c(n(biols[[biol_no]][["biol"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(out))

    # Set biol
    biol_no2 <- round(runif(1,min=1,max=length(biols)))
    flbs_out <- test_fwdBiolsAD_set_single_index_accessor(biols, biol_no, biols[[biol_no2]][["biol"]],
                                      biols[[biol_no2]][["srr_model_name"]],
                                      biols[[biol_no2]][["srr_params"]],
                                      biols[[biol_no2]][["srr_residuals"]],
                                      biols[[biol_no2]][["srr_residuals_mult"]])
    expect_that(flbs_out[[biol_no]], is_identical_to(flbs_in[[biol_no2]]))

    # Set value
    value <- abs(rnorm(1))
    flbs_out <- test_fwdBiolsAD_set_value_accessor(biols, biol_no, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(c(n(flbs_out[[biol_no]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
})


