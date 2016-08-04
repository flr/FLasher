# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Implementation of FLQuant7 - double and AD versions")
source("expect_funs.R")

test_that("FLQuant7 as and wrap - double",{
    flq7_in <- random_FLQuant_list_generator() 
    flq7_out <- test_FLQuant7_as_wrap(flq7_in)
    expect_that(flq7_in, is_identical_to(flq7_out))
    flq7_out <- test_FLQuant7_empty_wrap()
    expect_that(list(), is_identical_to(flq7_out))
})

test_that("FLQuant7 constructors - double",{
    flq7_in <- random_FLQuant_list_generator() 
    flq_in <- random_FLQuant_generator() 
    # Empty constructor - doesn't do anything - but shouldn't fail
    test_FLQuant7_basic_constructor()
    # SEXP constructor - used in as
    flq7_out <- test_FLQuant7_sexp_constructor(flq7_in)
    expect_that(flq7_in, is_identical_to(flq7_out))
    # FLQuant constructor
    flq7_out <- test_FLQuant7_FLQuant_constructor(flq_in)
    expect_that(flq_in, is_identical_to(flq7_out[[1]]))
    # Copy constructor
    flq7_out <- test_FLQuant7_copy_constructor(flq7_in)
    expect_that(flq7_in, is_identical_to(flq7_out))
    # Copy constructor2
    element <- round(runif(1,min=1, max = length(flq7_in)))
    indices <- round(runif(6,min=1, max = dim(flq7_in[[element]])))
    value <- rnorm(1)
    # Makes a copy of flq_in, changes a value of flq_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flq7s <-  test_FLQuant7_copy_constructor2(flq7_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flq7_in, is_identical_to(flq7s[["flq71"]]))
    expect_that(c(flq7s[["flq72"]][[element]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Assignment operator
    flq7_out <- test_FLQuant7_assignment_operator(flq7_in)
    expect_that(flq7_in, is_identical_to(flq7_out))
    # Assignment operator2
    flq7s <-  test_FLQuant7_assignment_operator2(flq7_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flq7_in, is_identical_to(flq7s[["flq71"]]))
    expect_that(c(flq7s[["flq72"]][[element]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Testing function operator - adds another FLQuant in
    flq7_out <-  test_FLQuant7_function_operator(flq7_in, flq_in)
    expect_that(length(flq7_in)+as.integer(1), is_identical_to(length(flq7_out)))
    expect_that(flq7_in, is_identical_to(flq7_out[-length(flq7_out)]))
    expect_that(flq_in, is_identical_to(flq7_out[[length(flq7_out)]]))

})

test_that("FLQuant7 get accessors - double",{
    flq7_in <- random_FLQuant_list_generator()
    expect_that(test_FLQuant7_get_ndim7(flq7_in), is_identical_to(length(flq7_in)))
})

test_that("FLQuant7 get and set data accessors - double", {
    flq7_in <- random_FLQuant_list_generator()
    flq_in <- random_FLQuant_generator()
    element <- round(runif(1,min=1, max = length(flq7_in)))
    indices <- round(runif(6,min=1, max = dim(flq7_in[[element]])))
    value <- rnorm(1)
    # single index
    expect_that(test_FLQuant7_const_get_single_index_accessor(flq7_in, element), is_identical_to(flq7_in[[element]]))
    expect_that(test_FLQuant7_get_single_index_accessor(flq7_in, element), is_identical_to(flq7_in[[element]]))
    flq7_out <- test_FLQuant7_set_single_index_accessor(flq7_in, element, flq_in)
    expect_that(flq7_out[[element]], is_identical_to(flq_in))
    expect_that(flq7_out[-element], is_identical_to(flq7_in[-element]))
    # multiple indices
    value_out <- test_FLQuant7_const_get_accessor(flq7_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(value_out, is_identical_to(c(flq7_in[[element]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    value_out <- test_FLQuant7_get_accessor(flq7_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(value_out, is_identical_to(c(flq7_in[[element]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    flq7_out <- test_FLQuant7_set_accessor(flq7_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(c(flq7_out[[element]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Check outside bounds
    expect_that(test_FLQuant7_const_get_single_index_accessor(flq7_in, length(flq7_in)+1), throws_error())
    expect_that(test_FLQuant7_get_single_index_accessor(flq7_in, length(flq7_in)+1), throws_error())
    expect_that(test_FLQuant7_set_single_index_accessor(flq7_in, length(flq7_in)+1, flq_in), throws_error())
    # multiple indices
    expect_that(test_FLQuant7_const_get_accessor(flq7_in, length(flq7_in)+1, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]), throws_error())
    expect_that(test_FLQuant7_get_accessor(flq7_in, length(flq7_in)+1, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]), throws_error())
    expect_that(test_FLQuant7_set_accessor(flq7_in, length(flq7_in)+1, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value), throws_error())
    # default dim7 is 1 single
    flq_out <- test_FLQuant7_const_default_dim7_get_accessor(flq7_in)
    expect_that(flq_out, is_identical_to(flq7_in[[1]]))
    flq_out <- test_FLQuant7_default_dim7_get_accessor(flq7_in)
    expect_that(flq_out, is_identical_to(flq7_in[[1]]))
    flq7_out <- test_FLQuant7_default_dim7_set_accessor(flq7_in, flq_in)
    expect_that(flq7_out[[1]], is_identical_to(flq_in))
    expect_that(flq7_out[-1], is_identical_to(flq7_in[-1]))
    # default dim7 is 1 multi
    indices <- round(runif(6,min=1, max = dim(flq7_in[[1]])))
    value_out <- test_FLQuant7_const_default_dim7_get_accessor_multi(flq7_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]) 
    expect_that(value_out, is_identical_to(c(flq7_in[[1]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    value_out <- test_FLQuant7_default_dim7_get_accessor_multi(flq7_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]) 
    expect_that(value_out, is_identical_to(c(flq7_in[[1]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    flq7_out <- test_FLQuant7_default_dim7_set_accessor_multi(flq7_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value) 
    expect_that(c(flq7_out[[1]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    expect_that(flq7_out[-1], is_identical_to(flq7_in[-1]))

})

test_that("FLQuant7AD as and wrap - AD",{
    flq7_in <- random_FLQuant_list_generator() 
    flq7_out <- test_FLQuant7AD_as_wrap(flq7_in)
    expect_that(flq7_in, is_identical_to(flq7_out))
    flq7_out <- test_FLQuant7AD_empty_wrap()
    expect_that(list(), is_identical_to(flq7_out))
})

test_that("FLQuant7AD constructors - AD",{
    flq7_in <- random_FLQuant_list_generator() 
    flq_in <- random_FLQuant_generator() 
    # Empty constructor - doesn't do anything - but shouldn't fail
    test_FLQuant7AD_basic_constructor()
    # SEXP constructor - used in as
    flq7_out <- test_FLQuant7AD_sexp_constructor(flq7_in)
    expect_that(flq7_in, is_identical_to(flq7_out))
    # FLQuant constructor
    flq7_out <- test_FLQuant7AD_FLQuant_constructor(flq_in)
    expect_that(flq_in, is_identical_to(flq7_out[[1]]))
    # Copy constructor
    flq7_out <- test_FLQuant7AD_copy_constructor(flq7_in)
    expect_that(flq7_in, is_identical_to(flq7_out))
    # Copy constructor2
    element <- round(runif(1,min=1, max = length(flq7_in)))
    indices <- round(runif(6,min=1, max = dim(flq7_in[[element]])))
    value <- rnorm(1)
    # Makes a copy of flq_in, changes a value of flq_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flq7s <-  test_FLQuant7AD_copy_constructor2(flq7_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flq7_in, is_identical_to(flq7s[["flq71"]]))
    expect_that(c(flq7s[["flq72"]][[element]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Assignment operator
    flq7_out <- test_FLQuant7AD_assignment_operator(flq7_in)
    expect_that(flq7_in, is_identical_to(flq7_out))
    # Assignment operator2
    flq7s <-  test_FLQuant7AD_assignment_operator2(flq7_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flq7_in, is_identical_to(flq7s[["flq71"]]))
    expect_that(c(flq7s[["flq72"]][[element]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Testing function operator - adds another FLQuant in
    flq7_out <-  test_FLQuant7AD_function_operator(flq7_in, flq_in)
    expect_that(length(flq7_in)+as.integer(1), is_identical_to(length(flq7_out)))
    expect_that(flq7_in, is_identical_to(flq7_out[-length(flq7_out)]))
    expect_that(flq_in, is_identical_to(flq7_out[[length(flq7_out)]]))

})

test_that("FLQuant7AD get accessors - AD",{
    flq7_in <- random_FLQuant_list_generator()
    expect_that(test_FLQuant7AD_get_ndim7(flq7_in), is_identical_to(length(flq7_in)))
})

test_that("FLQuant7AD get and set data accessors - AD", {
    flq7_in <- random_FLQuant_list_generator()
    flq_in <- random_FLQuant_generator()
    element <- round(runif(1,min=1, max = length(flq7_in)))
    indices <- round(runif(6,min=1, max = dim(flq7_in[[element]])))
    value <- rnorm(1)
    # single index
    expect_that(test_FLQuant7AD_const_get_single_index_accessor(flq7_in, element), is_identical_to(flq7_in[[element]]))
    expect_that(test_FLQuant7AD_get_single_index_accessor(flq7_in, element), is_identical_to(flq7_in[[element]]))
    flq7_out <- test_FLQuant7AD_set_single_index_accessor(flq7_in, element, flq_in)
    expect_that(flq7_out[[element]], is_identical_to(flq_in))
    expect_that(flq7_out[-element], is_identical_to(flq7_in[-element]))
    # multiple indices
    value_out <- test_FLQuant7AD_const_get_accessor(flq7_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(value_out, is_identical_to(c(flq7_in[[element]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    value_out <- test_FLQuant7AD_get_accessor(flq7_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(value_out, is_identical_to(c(flq7_in[[element]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    flq7_out <- test_FLQuant7AD_set_accessor(flq7_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(c(flq7_out[[element]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Check outside bounds
    expect_that(test_FLQuant7AD_const_get_single_index_accessor(flq7_in, length(flq7_in)+1), throws_error())
    expect_that(test_FLQuant7AD_get_single_index_accessor(flq7_in, length(flq7_in)+1), throws_error())
    expect_that(test_FLQuant7AD_set_single_index_accessor(flq7_in, length(flq7_in)+1, flq_in), throws_error())
    # multiple indices
    expect_that(test_FLQuant7AD_const_get_accessor(flq7_in, length(flq7_in)+1, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]), throws_error())
    expect_that(test_FLQuant7AD_get_accessor(flq7_in, length(flq7_in)+1, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]), throws_error())
    expect_that(test_FLQuant7AD_set_accessor(flq7_in, length(flq7_in)+1, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value), throws_error())
    # default dim7 is 1 single
    flq_out <- test_FLQuant7AD_const_default_dim7_get_accessor(flq7_in)
    expect_that(flq_out, is_identical_to(flq7_in[[1]]))
    flq_out <- test_FLQuant7AD_default_dim7_get_accessor(flq7_in)
    expect_that(flq_out, is_identical_to(flq7_in[[1]]))
    flq7_out <- test_FLQuant7AD_default_dim7_set_accessor(flq7_in, flq_in)
    expect_that(flq7_out[[1]], is_identical_to(flq_in))
    expect_that(flq7_out[-1], is_identical_to(flq7_in[-1]))
    # default dim7 is 1 multi
    indices <- round(runif(6,min=1, max = dim(flq7_in[[1]])))
    value_out <- test_FLQuant7AD_const_default_dim7_get_accessor_multi(flq7_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]) 
    expect_that(value_out, is_identical_to(c(flq7_in[[1]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    value_out <- test_FLQuant7AD_default_dim7_get_accessor_multi(flq7_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]) 
    expect_that(value_out, is_identical_to(c(flq7_in[[1]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    flq7_out <- test_FLQuant7AD_default_dim7_set_accessor_multi(flq7_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value) 
    expect_that(c(flq7_out[[1]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    expect_that(flq7_out[-1], is_identical_to(flq7_in[-1]))
})

