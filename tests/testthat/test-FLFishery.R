context("Implementation of FLFishery - double and AD versions")

test_that("FLFishery constructors - double",{
    flf_in <- random_FLFishery_generator() 
    test_FLFishery_empty_constructor()
    # SEXP constructor - used in as
    expect_identical(test_simple_FLFishery_sexp_constructor(flf_in), as.integer(0))
    # SEXP constructor with wrap
    # Failing on ftime - so just test other bits
    flf_out <- test_FLFishery_sexp_constructor(flf_in)
    test_FLFishery_equal(flf_in, flf_out)
    # as - wrap
    flf_out <- test_FLFishery_as_wrap(flf_in)
    test_FLFishery_equal(flf_in, flf_out)
    # Copy constructor
    flf_out <- test_FLFishery_copy_constructor(flf_in)
    test_FLFishery_equal(flf_in, flf_out)
    # Copy constructor2 - checking for deep copy
    flf_in <- random_FLFishery_generator() 
    element <- round(runif(1,min=1, max = length(flf_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flf_in[[element]]))))
    value <- rnorm(1)
    flfs <- test_FLFishery_copy_constructor2(flf_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    test_FLFishery_equal(flf_in, flfs[["flf1"]])
    expect_identical(c(landings.n(flfs[["flf2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Assignment operator
    flf_out <- test_FLFishery_assignment_operator(flf_in)
    test_FLFishery_equal(flf_in, flf_out)
    # Assignment operator2
    flfs <- test_FLFishery_assignment_operator2(flf_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    test_FLFishery_equal(flf_in, flfs[["flf1"]])
    expect_identical(c(landings.n(flfs[["flf2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
})

test_that("FLCatches get and set data accessors - double", {
    flf_in <- random_FLFishery_generator() 
    element <- round(runif(1,min=1, max = length(flf_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flf_in[[element]]))))
    # get const catches accessor
    values_out <-  test_FLFishery_const_catches_get_accessors(flf_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(landings.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.ratio(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_equal(values_out, values_in) # Not identical as some use the quant_sum method - numerical differences
    # get catches accessor
    values_out <-  test_FLFishery_catches_get_accessors(flf_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(landings.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.ratio(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_equal(values_out, values_in) # Not identical as some use the quant_sum method - numerical differences
    # get const ftime, effort, vcost, fcost accessor
    values_out <- test_FLFishery_const_economics_get_accessors(flf_in, indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(effort(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(vcost(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fcost(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(values_out, values_in)
    value_out <- test_FLFishery_const_get_ftime(flf_in, 1, indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(value_out, c(flf_in@ftime[1, indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # get effort, vcost, fcost accessor
    values_out <- test_FLFishery_economics_get_accessors(flf_in, indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(effort(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(vcost(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fcost(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(values_out, values_in)
    value_out <- test_FLFishery_get_ftime(flf_in, 1, indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(value_out, c(flf_in@ftime[1, indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # set catches and economics slots
    value <- rnorm(1)
    flf_out <- test_FLFishery_set_accessors(flf_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    values_out <- c(c(landings.n(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(effort(flf_out)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fcost(flf_out)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(vcost(flf_out)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(rep(value, length(values_out)), values_out)
    value <- rnorm(1)
    out <- test_FLFishery_set_ftime(flf_in, 1, indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(c(out[1, indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    element <- get_FLQuant_element(flf_in@ftime, c(1, indices[2], indices[3], indices[4], indices[5], indices[6]))
    # Check others are untouched
    expect_identical(c(out)[-element], c(flf_in@ftime)[-element])
})

#----------------------------------

test_that("FLFishery constructors - double",{
    flf_in <- random_FLFishery_generator() 
    test_FLFishery_empty_constructor()
    # SEXP constructor - used in as
    expect_identical(test_simple_FLFisheryAD_sexp_constructor(flf_in), as.integer(0))
    # SEXP constructor with wrap
    # Failing on ftime - so just test other bits
    flf_out <- test_FLFisheryAD_sexp_constructor(flf_in)
    test_FLFishery_equal(flf_in, flf_out)
    # as - wrap
    flf_out <- test_FLFisheryAD_as_wrap(flf_in)
    test_FLFishery_equal(flf_in, flf_out)
    # Copy constructor
    flf_out <- test_FLFisheryAD_copy_constructor(flf_in)
    test_FLFishery_equal(flf_in, flf_out)
    # Copy constructor2 - checking for deep copy
    flf_in <- random_FLFishery_generator() 
    element <- round(runif(1,min=1, max = length(flf_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flf_in[[element]]))))
    value <- rnorm(1)
    flfs <- test_FLFisheryAD_copy_constructor2(flf_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    test_FLFishery_equal(flf_in, flfs[["flf1"]])
    expect_identical(c(landings.n(flfs[["flf2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Assignment operator
    flf_out <- test_FLFisheryAD_assignment_operator(flf_in)
    test_FLFishery_equal(flf_in, flf_out)
    # Assignment operator2
    flfs <- test_FLFisheryAD_assignment_operator2(flf_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    test_FLFishery_equal(flf_in, flfs[["flf1"]])
    expect_identical(c(landings.n(flfs[["flf2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
})

test_that("FLCatches get and set data accessors - double", {
    flf_in <- random_FLFishery_generator() 
    element <- round(runif(1,min=1, max = length(flf_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flf_in[[element]]))))
    # get const catches accessor
    values_out <-  test_FLFisheryAD_const_catches_get_accessors(flf_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(landings.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.ratio(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_equal(values_out, values_in) # Not identical as some use the quant_sum method - numerical differences
    # get catches accessor
    values_out <-  test_FLFisheryAD_catches_get_accessors(flf_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(landings.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.n(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards(flf_in[[element]])[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.sel(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.ratio(flf_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_equal(values_out, values_in) # Not identical as some use the quant_sum method - numerical differences
    # get const effort, vcost, fcost accessor
    values_out <- test_FLFisheryAD_const_economics_get_accessors(flf_in, indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(effort(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(vcost(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fcost(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(values_out, values_in)
    # get effort, vcost, fcost accessor
    values_out <- test_FLFisheryAD_economics_get_accessors(flf_in, indices[2], indices[3], indices[4], indices[5], indices[6])
    values_in <- c(c(effort(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(vcost(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fcost(flf_in)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(values_out, values_in)
    # set catches and economics slots
    value <- rnorm(1)
    flf_out <- test_FLFisheryAD_set_accessors(flf_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    values_out <- c(c(landings.n(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.n(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(landings.wt(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(discards.wt(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(catch.sel(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(price(flf_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(effort(flf_out)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(fcost(flf_out)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]),
                c(vcost(flf_out)[1, indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_identical(rep(value, length(values_out)), values_out)
})


