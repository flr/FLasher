# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Implementation of FLFisheries - double and AD versions")
source("expect_funs.R")

test_that("FLFisheries constructors - double",{
    flfs_in <- random_FLFisheries_generator() 
    flfs_out <- test_FLFisheries_sexp_constructor(flfs_in)
    expect_FLFisheries_equal(flfs_in, flfs_out)
    flfs_out <- test_FLFisheries_as_wrap(flfs_in)
    expect_FLFisheries_equal(flfs_in, flfs_out)
    # Copy constructor
    flfs_out <- test_FLFisheries_copy_constructor(flfs_in)
    expect_FLFisheries_equal(flfs_in, flfs_out)
    # Copy constructor2 - checking for deep copy
    fishery <- round(runif(1,min=1, max = length(flfs_in)))
    catches <- round(runif(1,min=1, max = length(flfs_in[[fishery]])))
    indices <- as.integer(round(runif(6,min=1, max = dim(landings.n(flfs_in[[fishery]][[catches]])))))
    value <- rnorm(1)
    # Makes a copy of flcs_in, changes a value of flcs_in, returns original and new FLQuant
    flfss <- test_FLFisheries_copy_constructor2(flfs_in, fishery, catches, indices, value)
    expect_FLFisheries_equal(flfs_in, flfss[["flfs1"]])
    expect_identical(c(landings.n(flfss[["flfs2"]][[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Assignment operator
    flfs_out <- test_FLFisheries_assignment_operator(flfs_in)
    expect_FLFisheries_equal(flfs_in, flfs_out)
    # Assignment operator2
    flfss <- test_FLFisheries_assignment_operator2(flfs_in, fishery, catches, indices, value)
    expect_FLFisheries_equal(flfs_in, flfss[["flfs1"]])
    expect_identical(c(landings.n(flfss[["flfs2"]][[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
})

test_that("FLFisheries get accessors - double",{
    flfs_in <- random_FLFisheries_generator()
    expect_identical(test_FLFisheries_get_nfisheries(flfs_in), length(flfs_in))
})

test_that("FLFisheries get and set data accessors - double", {
    flfs_in <- random_FLFisheries_generator()
    fishery <- round(runif(1,min=1, max = length(flfs_in)))
    catches <- round(runif(1,min=1, max = length(flfs_in[[fishery]])))
    indices <- as.integer(round(runif(6,min=1, max = dim(landings.n(flfs_in[[fishery]][[catches]])))))
    # single gets
    list_op <- test_FLFisheries_const_get_single(flfs_in, fishery, catches, indices)
    expect_FLFishery_equal(list_op[["flf"]], flfs_in[[fishery]])
    expect_identical(list_op[["flc"]], flfs_in[[fishery]][[catches]])
    expect_identical(list_op[["landings_n"]], landings.n(flfs_in[[fishery]][[catches]]))
    expect_identical(list_op[["value"]], c(landings.n(flfs_in[[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    list_op <- test_FLFisheries_get_single(flfs_in, fishery, catches, indices)
    expect_FLFishery_equal(list_op[["flf"]], flfs_in[[fishery]])
    expect_identical(list_op[["flc"]], flfs_in[[fishery]][[catches]])
    expect_identical(list_op[["landings_n"]], landings.n(flfs_in[[fishery]][[catches]]))
    expect_identical(list_op[["value"]], c(landings.n(flfs_in[[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # double gets
    list_op <- test_FLFisheries_const_get_double(flfs_in, fishery, catches, indices)
    expect_FLFishery_equal(list_op[["flf"]], flfs_in[[fishery]])
    expect_identical(list_op[["flc"]], flfs_in[[fishery]][[catches]])
    expect_identical(list_op[["landings_n"]], landings.n(flfs_in[[fishery]][[catches]]))
    expect_identical(list_op[["value"]], c(landings.n(flfs_in[[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # Bounds check
    expect_error(test_FLFisheries_get_double(flfs_in, fishery, length(flfs_in[[fishery]])+1, indices))
    list_op <- test_FLFisheries_get_double(flfs_in, fishery, catches, indices)
    expect_FLFishery_equal(list_op[["flf"]], flfs_in[[fishery]])
    expect_identical(list_op[["flc"]], flfs_in[[fishery]][[catches]])
    expect_identical(list_op[["landings_n"]], landings.n(flfs_in[[fishery]][[catches]]))
    expect_identical(list_op[["value"]], c(landings.n(flfs_in[[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # Bounds check
    expect_error(test_FLFisheries_get_double(flfs_in, fishery, length(flfs_in[[fishery]])+1, indices))
    # Set a value in landings_n
    value <- rnorm(1)
    flfs_out <- test_FLFisheries_set_single(flfs_in, fishery, catches, indices, value)
    expect_identical(c(landings.n(flfs_out[[fishery]][[catches]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])), value)
    flfs_out <- test_FLFisheries_set_double(flfs_in, fishery, catches, indices, value)
    expect_identical(c(landings.n(flfs_out[[fishery]][[catches]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])), value)
})

#----------------------------------

test_that("FLFisheriesAD constructors - double",{
    flfs_in <- random_FLFisheries_generator() 
    flfs_out <- test_FLFisheriesAD_sexp_constructor(flfs_in)
    expect_FLFisheries_equal(flfs_in, flfs_out)
    flfs_out <- test_FLFisheriesAD_as_wrap(flfs_in)
    expect_FLFisheries_equal(flfs_in, flfs_out)
    # Copy constructor
    flfs_out <- test_FLFisheriesAD_copy_constructor(flfs_in)
    expect_FLFisheries_equal(flfs_in, flfs_out)
    # Copy constructor2 - checking for deep copy
    fishery <- round(runif(1,min=1, max = length(flfs_in)))
    catches <- round(runif(1,min=1, max = length(flfs_in[[fishery]])))
    indices <- as.integer(round(runif(6,min=1, max = dim(landings.n(flfs_in[[fishery]][[catches]])))))
    value <- rnorm(1)
    # Makes a copy of flcs_in, changes a value of flcs_in, returns original and new FLQuant
    flfss <- test_FLFisheriesAD_copy_constructor2(flfs_in, fishery, catches, indices, value)
    expect_FLFisheries_equal(flfs_in, flfss[["flfs1"]])
    expect_identical(c(landings.n(flfss[["flfs2"]][[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Assignment operator
    flfs_out <- test_FLFisheriesAD_assignment_operator(flfs_in)
    expect_FLFisheries_equal(flfs_in, flfs_out)
    # Assignment operator2
    flfss <- test_FLFisheriesAD_assignment_operator2(flfs_in, fishery, catches, indices, value)
    expect_FLFisheries_equal(flfs_in, flfss[["flfs1"]])
    expect_identical(c(landings.n(flfss[["flfs2"]][[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
})

test_that("FLFisheries get accessors - double",{
    flfs_in <- random_FLFisheries_generator()
    expect_identical(test_FLFisheriesAD_get_nfisheries(flfs_in), length(flfs_in))
})

test_that("FLFisheries get and set data accessors - double", {
    flfs_in <- random_FLFisheries_generator()
    fishery <- round(runif(1,min=1, max = length(flfs_in)))
    catches <- round(runif(1,min=1, max = length(flfs_in[[fishery]])))
    indices <- as.integer(round(runif(6,min=1, max = dim(landings.n(flfs_in[[fishery]][[catches]])))))
    # single gets
    list_op <- test_FLFisheriesAD_const_get_single(flfs_in, fishery, catches, indices)
    expect_FLFishery_equal(list_op[["flf"]], flfs_in[[fishery]])
    expect_identical(list_op[["flc"]], flfs_in[[fishery]][[catches]])
    expect_identical(list_op[["landings_n"]], landings.n(flfs_in[[fishery]][[catches]]))
    expect_identical(list_op[["value"]], c(landings.n(flfs_in[[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    list_op <- test_FLFisheriesAD_get_single(flfs_in, fishery, catches, indices)
    expect_FLFishery_equal(list_op[["flf"]], flfs_in[[fishery]])
    expect_identical(list_op[["flc"]], flfs_in[[fishery]][[catches]])
    expect_identical(list_op[["landings_n"]], landings.n(flfs_in[[fishery]][[catches]]))
    expect_identical(list_op[["value"]], c(landings.n(flfs_in[[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # double gets
    list_op <- test_FLFisheriesAD_const_get_double(flfs_in, fishery, catches, indices)
    expect_FLFishery_equal(list_op[["flf"]], flfs_in[[fishery]])
    expect_identical(list_op[["flc"]], flfs_in[[fishery]][[catches]])
    expect_identical(list_op[["landings_n"]], landings.n(flfs_in[[fishery]][[catches]]))
    expect_identical(list_op[["value"]], c(landings.n(flfs_in[[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # Bounds check
    expect_error(test_FLFisheries_get_double(flfs_in, fishery, length(flfs_in[[fishery]])+1, indices))
    list_op <- test_FLFisheriesAD_get_double(flfs_in, fishery, catches, indices)
    expect_FLFishery_equal(list_op[["flf"]], flfs_in[[fishery]])
    expect_identical(list_op[["flc"]], flfs_in[[fishery]][[catches]])
    expect_identical(list_op[["landings_n"]], landings.n(flfs_in[[fishery]][[catches]]))
    expect_identical(list_op[["value"]], c(landings.n(flfs_in[[fishery]][[catches]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # Bounds check
    expect_error(test_FLFisheriesAD_get_double(flfs_in, fishery, length(flfs_in[[fishery]])+1, indices))
    # Set a value in landings_n
    value <- rnorm(1)
    flfs_out <- test_FLFisheriesAD_set_single(flfs_in, fishery, catches, indices, value)
    expect_identical(c(landings.n(flfs_out[[fishery]][[catches]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])), value)
    flfs_out <- test_FLFisheriesAD_set_double(flfs_in, fishery, catches, indices, value)
    expect_identical(c(landings.n(flfs_out[[fishery]][[catches]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])), value)
})

test_that("FLCatchesAD iterators",{
    fisheries <- random_FLFisheries_generator(min_fisheries = 2, max_fisheries = 5)
    effortin <- lapply(fisheries, function(x) return(x@effort))
    # Const - just pulls out n
    effortout <- test_FLFisheriesAD_const_iterator(fisheries)
    expect_identical(effortout, effortin@.Data)
    # Not const - sets a value in first element (fisheries can be different dims)
    value <- rnorm(1)
    fisheries_out <- test_FLFisheriesAD_iterator(fisheries, value)
    value_out <- unname(unlist(lapply(fisheries_out, function(x) return(effort(x)[1, 1, 1, 1, 1, 1]))))
    expect_identical(rep(value,length(fisheries)), value_out)
    # All others are OK
    for (i in 1:length(fisheries)){
        expect_identical(c(fisheries_out[[i]]@effort)[-1], c(fisheries[[i]]@effort)[-1])
    }
})

