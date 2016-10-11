# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("CPP implementation of FLQuantAD")
source("expect_funs.R")

test_that("FLQuantAD as and wrap",{
    flq_in <- random_FLQuant_generator()
    flq_out <- test_FLQuantAD_as_wrap(flq_in)
    expect_identical(flq_in, flq_out)
})

test_that("FLQuantAD constructors",{
    flq_in <- random_FLQuant_generator()
    # Empty constructor - doesn't do anything - but shouldn't fail
    test_FLQuantAD_basic_constructor()
    # SEXP constructor - used in as
    flq_out <- test_FLQuantAD_sexp_constructor(flq_in)
    expect_identical(flq_in, flq_out)
    # dim constructor
    dims <- round(runif(6, min = 1, max = 5))
    flq_out <- test_FLQuantAD_dim_constructor(dims[1], dims[2], dims[3], dims[4], dims[5], dims[6])
    expect_true(all(flq_out == 0))
    expect_equal(dim(flq_out), dims)
    # Copy constructor
    flq_out <- test_FLQuantAD_copy_constructor(flq_in)
    expect_identical(flq_in, flq_out)
    # Copy constructor2
    indices <- round(runif(6,min=1, max = dim(flq_in)))
    value <- rnorm(1)
    # Makes a copy of flq_in, changes a value of flq_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flqs <-  test_FLQuantAD_copy_constructor2(flq_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flq_in, flqs[["flq1"]])
    expect_identical(c(flqs[["flq2"]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Assignment operator
    flq_out <- test_FLQuantAD_assignment_operator(flq_in)
    expect_identical(flq_in, flq_out)
    # Assignment operator2
    flqs <-  test_FLQuantAD_assignment_operator2(flq_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flq_in, flqs[["flq1"]])
    expect_identical(c(flqs[["flq2"]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Make an FLQuantAD from an FLQuant
    flq_out <- test_FLQuantAD_from_FLQuant_constructor(flq_in)
    expect_identical(flq_in, flq_out)
})

test_that("FLQuantAD get accessors",{
    flq <- random_FLQuant_generator()
    expect_identical(test_FLQuantAD_get_units(flq), units(flq))
    expect_equal(test_FLQuantAD_get_dim(flq), dim(flq))
    expect_identical(test_FLQuantAD_get_dimnames(flq), dimnames(flq))
    expect_identical(test_FLQuantAD_get_size(flq), length(c(flq@.Data)))
    expect_identical(test_FLQuantAD_get_nquant(flq), dim(flq)[1])
    expect_identical(test_FLQuantAD_get_nyear(flq), dim(flq)[2])
    expect_identical(test_FLQuantAD_get_nunit(flq), dim(flq)[3])
    expect_identical(test_FLQuantAD_get_nseason(flq), dim(flq)[4])
    expect_identical(test_FLQuantAD_get_narea(flq), dim(flq)[5])
    expect_identical(test_FLQuantAD_get_niter(flq), dim(flq)[6])
    indices <- round(runif(6,min=1, max = dim(flq)))
    value <- rnorm(1)
    element <- test_FLQuantAD_get_data_element(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), c(flq)[element+1])
})

test_that("set",{
    # set_dimnames
    flq_in <- random_FLQuant_generator()
    new_dimnames <- dimnames(flq_in)
    new_dimnames[[1]][1] <- as.character(rnorm(1))
    flq_out <- test_FLQuantAD_set_dimnames(flq_in, new_dimnames)
    expect_identical(dimnames(flq_out)[[1]][1], new_dimnames[[1]][1])
    expect_identical(c(flq_out@.Data), c(flq_in@.Data))
    expect_identical(dim(flq_out), dim(flq_in))
    # Check falure
    new_dimnames[[3]] <- c(new_dimnames[[3]],"extra")
    expect_error(test_FLQuantAD_set_dimnames(flq_in, new_dimnames))
})

test_that("FLQuantAD get and set data accessors", {
    fixed_dims <- c(rep(NA,5),round(runif(1,min=2,max=10)))
    flq <- random_FLQuant_generator(fixed_dims = fixed_dims)
    indices <- round(runif(6,min=1, max = dim(flq)))
    index <- round(runif(1,min=1,max = prod(dim(flq))))
    value <- rnorm(1)
    # single index
    expect_identical(test_FLQuantAD_get_const_single_index_accessor(flq, index), c(flq@.Data)[index])
    expect_identical(test_FLQuantAD_get_single_index_accessor(flq, index), c(flq@.Data)[index])
    flq_out <- test_FLQuantAD_set_single_index_accessor(flq, index, value)
    expect_identical(c(flq_out)[index], value)
    # multiple indices
    value_out <- test_FLQuantAD_const_get_accessor(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value_out)
    value_out <- test_FLQuantAD_get_accessor(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value_out)
    flq_out <- test_FLQuantAD_set_accessor(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(c(flq_out[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Check outside bounds
    big_index <- prod(dim(flq))+1
    big_indices1 <- dim(flq) + c(1,0,0,0,0,0)
    big_indices2 <- dim(flq) + c(0,1,0,0,0,0)
    big_indices3 <- dim(flq) + c(0,0,1,0,0,0)
    big_indices4 <- dim(flq) + c(0,0,0,1,0,0)
    big_indices5 <- dim(flq) + c(0,0,0,0,1,0)
    big_indices6 <- dim(flq) + c(0,0,0,0,0,1)
    expect_error(test_FLQuantAD_get_const_single_index_accessor(flq, big_index))
    expect_error(test_FLQuantAD_get_single_index_accessor(flq, big_index))
    expect_error(test_FLQuantAD_set_single_index_accessor(flq, big_index, value))
    expect_error(test_FLQuantAD_const_get_accessor(flq, big_indices1[1], big_indices1[2], big_indices1[3], big_indices1[4], big_indices1[5], big_indices1[6]))
    expect_error(test_FLQuantAD_const_get_accessor(flq, big_indices2[1], big_indices2[2], big_indices2[3], big_indices2[4], big_indices2[5], big_indices2[6]))
    expect_error(test_FLQuantAD_const_get_accessor(flq, big_indices3[1], big_indices3[2], big_indices3[3], big_indices3[4], big_indices3[5], big_indices3[6]))
    expect_error(test_FLQuantAD_const_get_accessor(flq, big_indices4[1], big_indices4[2], big_indices4[3], big_indices4[4], big_indices4[5], big_indices4[6]))
    expect_error(test_FLQuantAD_const_get_accessor(flq, big_indices5[1], big_indices5[2], big_indices5[3], big_indices5[4], big_indices5[5], big_indices5[6]))
    expect_error(test_FLQuantAD_const_get_accessor(flq, big_indices6[1], big_indices6[2], big_indices6[3], big_indices6[4], big_indices6[5], big_indices6[6]))
    expect_error(test_FLQuantAD_get_accessor(flq, big_indices1[1], big_indices1[2], big_indices1[3], big_indices1[4], big_indices1[5], big_indices1[6]))
    expect_error(test_FLQuantAD_get_accessor(flq, big_indices2[1], big_indices2[2], big_indices2[3], big_indices2[4], big_indices2[5], big_indices2[6]))
    expect_error(test_FLQuantAD_get_accessor(flq, big_indices3[1], big_indices3[2], big_indices3[3], big_indices3[4], big_indices3[5], big_indices3[6]))
    expect_error(test_FLQuantAD_get_accessor(flq, big_indices4[1], big_indices4[2], big_indices4[3], big_indices4[4], big_indices4[5], big_indices4[6]))
    expect_error(test_FLQuantAD_get_accessor(flq, big_indices5[1], big_indices5[2], big_indices5[3], big_indices5[4], big_indices5[5], big_indices5[6]))
    expect_error(test_FLQuantAD_get_accessor(flq, big_indices6[1], big_indices6[2], big_indices6[3], big_indices6[4], big_indices6[5], big_indices6[6]))
    expect_error(test_FLQuantAD_set_accessor(flq, big_indices1[1], big_indices1[2], big_indices1[3], big_indices1[4], big_indices1[5], big_indices1[6], value))
    expect_error(test_FLQuantAD_set_accessor(flq, big_indices2[1], big_indices2[2], big_indices2[3], big_indices2[4], big_indices2[5], big_indices2[6], value))
    expect_error(test_FLQuantAD_set_accessor(flq, big_indices3[1], big_indices3[2], big_indices3[3], big_indices3[4], big_indices3[5], big_indices3[6], value))
    expect_error(test_FLQuantAD_set_accessor(flq, big_indices4[1], big_indices4[2], big_indices4[3], big_indices4[4], big_indices4[5], big_indices4[6], value))
    expect_error(test_FLQuantAD_set_accessor(flq, big_indices5[1], big_indices5[2], big_indices5[3], big_indices5[4], big_indices5[5], big_indices5[6], value))
    expect_error(test_FLQuantAD_set_accessor(flq, big_indices6[1], big_indices6[2], big_indices6[3], big_indices6[4], big_indices6[5], big_indices6[6], value))
    # indices accessor
    expect_identical(test_FLQuantAD_get_const_indices_accessor(flq, indices), c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_error(test_FLQuantAD_get_const_indices_accessor(flq, c(indices,1)))
    expect_error(test_FLQuantAD_get_const_indices_accessor(flq, indices[-1]))
    expect_identical(test_FLQuantAD_get_indices_accessor(flq, indices), c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_error(test_FLQuantAD_get_indices_accessor(flq, c(indices,1)))
    expect_error(test_FLQuantAD_get_indices_accessor(flq, indices[-1]))
    value <- rnorm(1)
    flq_out <- test_FLQuantAD_set_indices_accessor(flq, indices, value)
    expect_identical(c(flq_out[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
})

test_that("FLQuantAD match_dims method works", {
    flq <- random_FLQuant_generator()
    dim_flq <- dim(flq)
    dim_flq1 <- dim_flq + c(1,0,0,0,0,0)
    flq1 <- FLQuant(0, dim = dim_flq + c(1,0,0,0,0,0))
    flq2 <- FLQuant(0, dim = dim_flq + c(0,1,0,0,0,0)) 
    flq3 <- FLQuant(0, dim = dim_flq + c(0,0,1,0,0,0))
    flq4 <- FLQuant(0, dim = dim_flq + c(0,0,0,1,0,0))
    flq5 <- FLQuant(0, dim = dim_flq + c(0,0,0,0,1,0))
    flq6 <- FLQuant(0, dim = dim_flq + c(0,0,0,0,0,1))
    expect_identical(test_FLQuantAD_FLQuantAD_match_dims(flq, flq), 1L)
    expect_identical(test_FLQuantAD_FLQuantAD_match_dims(flq, flq1), -1L)
    expect_identical(test_FLQuantAD_FLQuantAD_match_dims(flq, flq2), -2L)
    expect_identical(test_FLQuantAD_FLQuantAD_match_dims(flq, flq3), -3L)
    expect_identical(test_FLQuantAD_FLQuantAD_match_dims(flq, flq4), -4L)
    expect_identical(test_FLQuantAD_FLQuantAD_match_dims(flq, flq5), -5L)
    expect_identical(test_FLQuantAD_FLQuantAD_match_dims(flq, flq6), -6L)
    expect_identical(test_FLQuantAD_FLQuant_match_dims(flq, flq), 1L)
    expect_identical(test_FLQuantAD_FLQuant_match_dims(flq, flq1), -1L)
    expect_identical(test_FLQuantAD_FLQuant_match_dims(flq, flq2), -2L)
    expect_identical(test_FLQuantAD_FLQuant_match_dims(flq, flq3), -3L)
    expect_identical(test_FLQuantAD_FLQuant_match_dims(flq, flq4), -4L)
    expect_identical(test_FLQuantAD_FLQuant_match_dims(flq, flq5), -5L)
    expect_identical(test_FLQuantAD_FLQuant_match_dims(flq, flq6), -6L)
    expect_identical(test_FLQuant_FLQuantAD_match_dims(flq, flq), 1L)
    expect_identical(test_FLQuant_FLQuantAD_match_dims(flq, flq1), -1L)
    expect_identical(test_FLQuant_FLQuantAD_match_dims(flq, flq2), -2L)
    expect_identical(test_FLQuant_FLQuantAD_match_dims(flq, flq3), -3L)
    expect_identical(test_FLQuant_FLQuantAD_match_dims(flq, flq4), -4L)
    expect_identical(test_FLQuant_FLQuantAD_match_dims(flq, flq5), -5L)
    expect_identical(test_FLQuant_FLQuantAD_match_dims(flq, flq6), -6L)
})




