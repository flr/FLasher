# 
# Copyright 2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, JRC
#

context("Implementation of FLCatch - double and AD versions")

test_that("FLCatch as and wrap",{
    flc_in <- random_FLCatch_generator()
    expect_that(test_simple_FLCatch_sexp_constructor(flc_in), is_identical_to(as.integer(0)))
    flc_out <- test_FLCatch_as_wrap(flc_in)
    expect_that(flc_in, is_identical_to(flc_out))
    flc_out <- test_FLCatchAD_as_wrap(flc_in)
    expect_that(flc_in, is_identical_to(flc_out))
})

test_that("FLCatch constructors",{
    flc_in <- random_FLCatch_generator()
    # SEXP constructors - problem with catch.q
    flc_out <- test_FLCatch_sexp_constructor(flc_in)
    expect_that(flc_in, is_identical_to(flc_out))
    flc_out <- test_FLCatchAD_sexp_constructor(flc_in)
    expect_that(flc_in, is_identical_to(flc_out))
    # Copy constructor
    flc_out <- test_FLCatch_copy_constructor(flc_in)
    expect_that(flc_in, is_identical_to(flc_out))
    flc_out <- test_FLCatchAD_copy_constructor(flc_in)
    expect_that(flc_in, is_identical_to(flc_out))
    # Copy constructor2
    indices <- round(runif(6,min=1, max = dim(landings.n(flc_in))))
    value <- rnorm(1)
    # Makes a copy of flq_in, changes a value of flq_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flcs <-  test_FLCatch_copy_constructor2(flc_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flc_in, is_identical_to(flcs[["flc1"]]))
    expect_that(c(landings.n(flcs[["flc2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    flcs <-  test_FLCatchAD_copy_constructor2(flc_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flc_in, is_identical_to(flcs[["flc1"]]))
    expect_that(c(landings.n(flcs[["flc2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Assignment operator
    flc_out <- test_FLCatch_assignment_operator(flc_in)
    expect_that(flc_in, is_identical_to(flc_out))
    flc_out <- test_FLCatchAD_assignment_operator(flc_in)
    expect_that(flc_in, is_identical_to(flc_out))
    # Assignment operator2
    indices <- round(runif(6,min=1, max = dim(landings.n(flc_in))))
    value <- rnorm(1)
    # Makes a copy of flq_in, changes a value of flq_in, returns original and new FLQuant
    # Checks that the assignment operator makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flcs <-  test_FLCatch_assignment_operator2(flc_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flc_in, is_identical_to(flcs[["flc1"]]))
    expect_that(c(landings.n(flcs[["flc2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    flcs <-  test_FLCatchAD_assignment_operator2(flc_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flc_in, is_identical_to(flcs[["flc1"]]))
    expect_that(c(landings.n(flcs[["flc2"]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
})

test_that("FLCatch get and set data accessors", {
    # Get const double
    flc_in <- random_FLCatch_generator()
    indices <- round(runif(6,min=1, max = dim(landings.n(flc_in))))
    values_out <- test_FLCatch_const_get_accessors(flc_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(landings.n(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_that(values_out, is_identical_to(values_in))
    # Get const AD
    flc_in <- random_FLCatch_generator()
    indices <- round(runif(6,min=1, max = dim(landings.n(flc_in))))
    values_out <- test_FLCatchAD_const_get_accessors(flc_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(landings.n(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_that(values_out, is_identical_to(values_in))
    # Get double
    flc_in <- random_FLCatch_generator()
    indices <- round(runif(6,min=1, max = dim(landings.n(flc_in))))
    values_out <- test_FLCatch_get_accessors(flc_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(landings.n(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_that(values_out, is_identical_to(values_in))
    # Get AD
    values_out <- test_FLCatchAD_get_accessors(flc_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(landings.n(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flc_in)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_that(values_out, is_identical_to(values_in))
    # Set double
    flc_in <- random_FLCatch_generator()
    indices <- round(runif(6,min=1, max = dim(landings.n(flc_in))))
    values_in <- rnorm(6)
    flc_out <- test_FLCatch_set_accessors(flc_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], values_in)
    values_out <- c(c(landings.n(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # Check inserted values are correct
    expect_that(values_out, is_identical_to(values_in))
    # Check other values have been left alone
    narea <- dim(landings.n(flc_out))[5]
    nseason <- dim(landings.n(flc_out))[4]
    nunit<- dim(landings.n(flc_out))[3]
    nyear <- dim(landings.n(flc_out))[2]
    nquant <- dim(landings.n(flc_out))[1]
    element <- (narea * nseason * nunit * nyear * nquant * (indices[6] - 1)) + (nseason * nunit * nyear * nquant * (indices[5] - 1)) + (nunit * nyear * nquant * (indices[4] - 1)) + (nyear * nquant * (indices[3] - 1)) + (nquant * (indices[2] - 1)) + (indices[1] - 1) + 1; 
    expect_that(c(landings.n(flc_out))[-element], is_identical_to(c(landings.n(flc_in))[-element]))
    expect_that(c(discards.n(flc_out))[-element], is_identical_to(c(discards.n(flc_in))[-element]))
    expect_that(c(landings.wt(flc_out))[-element], is_identical_to(c(landings.wt(flc_in))[-element]))
    expect_that(c(discards.wt(flc_out))[-element], is_identical_to(c(discards.wt(flc_in))[-element]))
    expect_that(c(catch.sel(flc_out))[-element], is_identical_to(c(catch.sel(flc_in))[-element]))
    expect_that(c(price(flc_out))[-element], is_identical_to(c(price(flc_in))[-element]))
    # Set AD 
    flc_in <- random_FLCatch_generator()
    indices <- round(runif(6,min=1, max = dim(landings.n(flc_in))))
    values_in <- rnorm(6)
    flc_out <- test_FLCatchAD_set_accessors(flc_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], values_in)
    values_out <- c(c(landings.n(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flc_out)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # Check inserted values are correct
    expect_that(values_out, is_identical_to(values_in))
    # Check other values have been left alone
    narea <- dim(landings.n(flc_out))[5]
    nseason <- dim(landings.n(flc_out))[4]
    nunit<- dim(landings.n(flc_out))[3]
    nyear <- dim(landings.n(flc_out))[2]
    nquant <- dim(landings.n(flc_out))[1]
    element <- (narea * nseason * nunit * nyear * nquant * (indices[6] - 1)) + (nseason * nunit * nyear * nquant * (indices[5] - 1)) + (nunit * nyear * nquant * (indices[4] - 1)) + (nyear * nquant * (indices[3] - 1)) + (nquant * (indices[2] - 1)) + (indices[1] - 1) + 1; 
    expect_that(c(landings.n(flc_out))[-element], is_identical_to(c(landings.n(flc_in))[-element]))
    expect_that(c(discards.n(flc_out))[-element], is_identical_to(c(discards.n(flc_in))[-element]))
    expect_that(c(landings.wt(flc_out))[-element], is_identical_to(c(landings.wt(flc_in))[-element]))
    expect_that(c(discards.wt(flc_out))[-element], is_identical_to(c(discards.wt(flc_in))[-element]))
    expect_that(c(catch.sel(flc_out))[-element], is_identical_to(c(catch.sel(flc_in))[-element]))
    expect_that(c(price(flc_out))[-element], is_identical_to(c(price(flc_in))[-element]))
})


test_that("FLCatch catch_q accessor", {
    flc_in <- random_FLCatch_generator()
    dims <- dim(landings.n(flc_in))
    # Make a random FLPar with max dims of flq
    # Make random indices of FLPar (-1)
    # Call it and test
    catch.q.params <- FLPar(rnorm(2), dimnames = list(params = c("alpha","beta"), iter = 1))
    catch.q.year <- FLPar(rnorm(2 * dims[2]), dimnames = list(params = c("alpha","beta"), year = 1:dims[2], iter = 1))
    catch.q.iter <- FLPar(rnorm(2 * dims[6]), dimnames = list(params = c("alpha","beta"), iter = 1:dims[6]))
    catch.q2 <- FLPar(rnorm(2 * dims[6] * dims[2]), dimnames = list(params = c("alpha","beta"), year = 1:dims[2], iter = 1:dims[6]))
    catch.q4 <- FLPar(rnorm(2 * dims[6] * dims[4]), dimnames = list(params = c("alpha","beta"), season = 1:dims[4], iter = 1:dims[6]))
    catch.q24 <- FLPar(rnorm(2 * dims[2] * dims[6] * dims[4]), dimnames = list(params = c("alpha","beta"), year=1:dims[2], season = 1:dims[4], iter = 1:dims[6]))
    indices <- round(runif(6, min=1, max=dims))
    indices_max <- round(runif(6, min=1, max=dims))
    indices_max[1] <- 2
    indices_min <- round(runif(6, min=1, max=indices_max))

    # Asking for too many parameters
    indices_max_error <- indices_max
    indices_max_error[1] <- 3
    expect_that(test_FLCatchAD_catch_q_params_subset(flc_in, indices_min, indices_max_error), throws_error())

    # Indices out of range
    indices_min_error <- indices_min
    indices_min_error[2] <- dims[2]+1
    expect_that(test_FLCatchAD_catch_q_params_subset(flc_in, indices_min_error, indices_max), throws_error())

    # Only params - no other dims
    catch.q(flc_in) <- catch.q.params
    params <- test_FLCatchAD_catch_q_params(flc_in, indices)
    expect_that(c(catch.q.params[]), is_identical_to(c(params)))
    # Get all params
    indices_min_all <- indices_min
    indices_min_all[1] <- 1
    params <- test_FLCatchAD_catch_q_params_subset(flc_in, indices_min_all, indices_max)
    expect_that(dim(params), equals(indices_max - indices_min_all + 1))
    expect_that(c(params[1,]), equals(rep(c(catch.q.params[1,]), prod(dim(params[-1])))))
    expect_that(c(params[2,]), equals(rep(c(catch.q.params[2,]), prod(dim(params[-1])))))
    # Subset params
    params <- test_FLCatchAD_catch_q_params_subset(flc_in, indices_min, indices_max)
    expect_that(unname(params@.Data), equals(unname(FLQuant(c(catch.q.params[indices_min[1]:indices_max[1],]), dim = c(indices_max - indices_min +1))@.Data)))

    # Params have year - all iters the same
    catch.q(flc_in) <- catch.q.year
    params <- test_FLCatchAD_catch_q_params(flc_in, indices)
    expect_that(c(catch.q.year[,indices[2]]), is_identical_to(c(params)))
    params <- test_FLCatchAD_catch_q_params_subset(flc_in, indices_min, indices_max)
    expect_that(dim(params), equals(indices_max - indices_min + 1))
    expect_that(unname(params@.Data), equals(unname(FLQuant(c(catch.q.year[indices_min[1]:indices_max[1],indices_min[2]:indices_max[2]]), dim = c(indices_max - indices_min +1))@.Data)))

    # Only params and iter - all other dims the same
    catch.q(flc_in) <- catch.q.iter
    params <- test_FLCatchAD_catch_q_params(flc_in, indices)
    expect_that(c(catch.q.iter[,indices[6]]), is_identical_to(c(params)))
    params <- test_FLCatchAD_catch_q_params_subset(flc_in, indices_min, indices_max)
    expect_that(dim(params), equals(indices_max - indices_min + 1))
    flq_in <- FLQuant(NA, dim=indices_max - indices_min + 1)
    for (iter_count in 1:(indices_max[6] - indices_min[6] + 1)){
        iter(flq_in,iter_count)[] <- c(catch.q.iter[indices_min[1]:indices_max[1],indices_min[6] + iter_count - 1])
    }
    expect_that(unname(params@.Data), equals(unname(flq_in@.Data)))

    # params, year and iter
    catch.q(flc_in) <- catch.q2
    params <- test_FLCatchAD_catch_q_params(flc_in, indices)
    expect_that(c(catch.q2[,indices[2],indices[6]]), is_identical_to(c(params)))
    params <- test_FLCatchAD_catch_q_params_subset(flc_in, indices_min, indices_max)
    expect_that(dim(params), equals(indices_max - indices_min + 1))
    # All units, seasons and areas the same
    catch.q2[indices_min[1]:indices_max[1], indices_min[2]: indices_max[2], indices_min[6]:indices_max[6]]


    # params, season and iter
    catch.q(flc_in) <- catch.q4
    params <- test_FLCatchAD_catch_q_params(flc_in, indices)
    expect_that(c(catch.q4[,indices[4],indices[6]]), is_identical_to(c(params)))
    # params, year, season and iter
    catch.q(flc_in) <- catch.q24
    params <- test_FLCatchAD_catch_q_params(flc_in, indices)
    expect_that(c(catch.q24[,indices[2],indices[4],indices[6]]), is_identical_to(c(params)))
})

test_that("FLCatch methods", {
    flc_in <- random_FLCatch_generator()
    # landings
    l_in <- landings(flc_in)
    l_out <- test_FLCatch_landings(flc_in)
    #expect_that(l_out, is_identical_to(l_in)) # units not dealt with correctly
    expect_that(dimnames(l_out), is_identical_to(dimnames(l_in))) # units not dealt with correctly
    expect_that(l_out@.Data, equals(l_in@.Data)) # quant sums causes numerical differences
    l_out <- test_FLCatchAD_landings(flc_in)
    #expect_that(l_out, is_identical_to(l_in)) # units not dealt with correctly
    expect_that(dimnames(l_out), is_identical_to(dimnames(l_in))) # units not dealt with correctly
    expect_that(l_out@.Data, equals(l_in@.Data)) # quant sums causes numerical differences

    # discards
    d_in <- discards(flc_in)
    d_out <- test_FLCatch_discards(flc_in)
    #expect_that(d_out, is_identical_to(d_in)) # units not dealt with correctly
    expect_that(dimnames(d_out), is_identical_to(dimnames(d_in))) # units not dealt with correctly
    expect_that(d_out@.Data, equals(d_in@.Data)) # quant sums causes numerical differences
    d_out <- test_FLCatchAD_discards(flc_in)
    #expect_that(d_out, is_identical_to(d_in)) # units not dealt with correctly
    expect_that(dimnames(d_out), is_identical_to(dimnames(d_in))) # units not dealt with correctly
    expect_that(d_out@.Data, equals(d_in@.Data)) # quant sums causes numerical differences

    # catch_n
    cn_in <- catch.n(flc_in)
    cn_out <- test_FLCatch_catch_n(flc_in)
    #expect_that(cn_out, is_identical_to(cn_in)) # units not dealt with correctly
    expect_that(dimnames(cn_out), is_identical_to(dimnames(cn_in))) # units not dealt with correctly
    expect_that(cn_out@.Data, equals(cn_in@.Data)) # quant sums causes numerical differences
    cn_out <- test_FLCatchAD_catch_n(flc_in)
    #expect_that(cn_out, is_identicad_to(cn_in)) # units not dealt with correctly
    expect_that(dimnames(cn_out), is_identical_to(dimnames(cn_in))) # units not dealt with correctly
    expect_that(cn_out@.Data, equals(cn_in@.Data)) # quant sums causes numerical differences

    # catches
    c_in <- catch(flc_in)
    c_out <- test_FLCatch_catches(flc_in)
    expect_that(dimnames(c_out), is_identical_to(dimnames(c_in))) # units not dealt with correctly
    expect_that(c_out@.Data, equals(c_in@.Data)) # quant sums causes numerical differences
    c_out <- test_FLCatchAD_catches(flc_in)
    expect_that(dimnames(c_out), is_identical_to(dimnames(c_in))) # units not dealt with correctly
    expect_that(c_out@.Data, equals(c_in@.Data)) # quant sums causes numerical differences

    # catch weight
    cw_in <- catch.wt(flc_in)
    cw_out <- test_FLCatch_catch_wt(flc_in)
    expect_that(dimnames(cw_out), is_identical_to(dimnames(cw_in))) # units not dealt with correctly
    expect_that(cw_out@.Data, equals(cw_in@.Data)) # quant sums causes numerical differences
    cw_out <- test_FLCatchAD_catch_wt(flc_in)
    expect_that(dimnames(cw_out), is_identical_to(dimnames(cw_in))) # units not dealt with correctly
    expect_that(cw_out@.Data, equals(cw_in@.Data)) # quant sums causes numerical differences

    # discards ratio
    dr_in <- discards.ratio(flc_in)
    dr_out <- test_FLCatch_discards_ratio(flc_in)
    expect_that(dimnames(dr_out), is_identical_to(dimnames(dr_in))) # units not dealt with correctly
    expect_that(dr_out@.Data, equals(dr_in@.Data)) # quant sums causes numerical differences
    dr_out <- test_FLCatchAD_discards_ratio(flc_in)
    expect_that(dimnames(dr_out), is_identical_to(dimnames(dr_in))) # units not dealt with correctly
    expect_that(dr_out@.Data, equals(dr_in@.Data)) # quant sums causes numerical differences

    # landings_sel
    ls_in <- landings.sel(flc_in)
    ls_out <- test_FLCatch_landings_sel(flc_in)
    expect_that(dimnames(ls_out), is_identical_to(dimnames(ls_in))) 
    expect_that(ls_out@.Data, is_identical_to(ls_in@.Data)) 
    ls_out <- test_FLCatchAD_landings_sel(flc_in)
    expect_that(dimnames(ls_out), is_identical_to(dimnames(ls_in))) 
    expect_that(ls_out@.Data, is_identical_to(ls_in@.Data)) 

    # discards_sel
    ds_in <- discards.sel(flc_in)
    ds_out <- test_FLCatch_discards_sel(flc_in)
    expect_that(dimnames(ds_out), is_identical_to(dimnames(ds_in))) 
    expect_that(ds_out@.Data, is_identical_to(ds_in@.Data)) 
    ds_out <- test_FLCatchAD_discards_sel(flc_in)
    expect_that(dimnames(ds_out), is_identical_to(dimnames(ds_in))) 
    expect_that(ds_out@.Data, is_identical_to(ds_in@.Data)) 
})


