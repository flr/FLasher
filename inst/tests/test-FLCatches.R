context("Implementation of FLCatches - double and AD versions")

test_that("FLCatches as and wrap - double",{
    flcs_in <- random_FLCatches_generator() 
    flcs_out <- test_FLCatches_as_wrap(flcs_in)
    expect_that(flcs_in, is_identical_to(flcs_out))

    flcs_in[["Catch 1"]]
    slotNames(flcs_in[["Catch 1"]])
    flcs_in[["Catch 1"]]@catch.q
    flcs_out[["Catch 1"]]@catch.q



    # What is up with catch.q?
    expect_that(flcs_in[["Catch 1"]]@catch.q, is_identical_to(flcs_out[["Catch 1"]]@catch.q))
    expect_that(flcs_in[["Catch 1"]]@catch.q@.Data, is_identical_to(flcs_out[["Catch 1"]]@catch.q@.Data))
    expect_that(c(flcs_in[["Catch 1"]]@catch.q@.Data), is_identical_to(c(flcs_out[["Catch 1"]]@catch.q@.Data)))
                c(flcs_in[["Catch 1"]]@catch.q@.Data)
                c(flcs_out[["Catch 1"]]@catch.q@.Data)
    expect_that(flcs_in[["Catch 1"]]@landings.wt, is_identical_to(flcs_out[["Catch 1"]]@landings.wt))

})

test_that("FLCatches constructors - double",{
    flcs_in <- random_FLCatches_generator() 
    flc_in <- random_FLCatch_generator() 
    # SEXP constructor - used in as
    flcs_out <- test_FLCatches_sexp_constructor(flcs_in)
    expect_that(flcs_in, is_identical_to(flcs_out))
    # FLCatch constructor
    flcs_out <- test_FLCatches_FLCatch_constructor(flc_in)
    expect_that(flc_in, is_identical_to(flcs_out[[1]]))
    # flcs <- FLCatches(list(flc_in)) # names not set right - should be NA
    # Copy constructor
    flcs_out <- test_FLCatches_copy_constructor(flcs_in)
    expect_that(flcs_in, is_identical_to(flcs_out))
    # Copy constructor2 - checking for deep copy
    element <- round(runif(1,min=1, max = length(flcs_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flcs_in[[element]]))))
    value <- rnorm(1)
    # Makes a copy of flcs_in, changes a value of flcs_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flcss <-  test_FLCatches_copy_constructor2(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flcs_in, is_identical_to(flcss[["flcs1"]]))
    expect_that(c(landings.n(flcss[["flcs2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Assignment operator
    flcs_out <- test_FLCatches_assignment_operator(flcs_in)
    expect_that(flcs_in, is_identical_to(flcs_out))
    # Assignment operator2
    flcss <-  test_FLCatches_assignment_operator2(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flcs_in, is_identical_to(flcss[["flcs1"]]))
    expect_that(c(landings.n(flcss[["flcs2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Testing function operator - adds another FLCatch in
    flcs_out <-  test_FLCatches_function_operator(flcs_in, flc_in)
    expect_that(length(flcs_in)+as.integer(1), is_identical_to(length(flcs_out)))
    for (i in 1:(length(flcs_in)-1)){
        expect_that(flcs_out[i], is_identical_to(flcs_in[i]))
        expect_that(flcs_out[[i]], is_identical_to(flcs_in[[i]]))
    }
    expect_that(flc_in, is_identical_to(flcs_out[[length(flcs_in)+1]]))
    expect_that(flcs_in@desc, is_identical_to(flcs_out@desc))
    # not check names because we haven't given the new one a name
})

test_that("FLCatches get accessors - double",{
    flcs_in <- random_FLCatches_generator()
    expect_that(test_FLCatches_get_ncatches(flcs_in), is_identical_to(length(flcs_in)))
})

test_that("FLCatches get and set data accessors - double", {
    flcs_in <- random_FLCatches_generator()
    flc_in <- random_FLCatch_generator()
    element <- round(runif(1,min=1, max = length(flcs_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flcs_in[[element]]))))
    value <- rnorm(1)
    # single index
    expect_that(test_FLCatches_const_get_single_index_accessor(flcs_in, element), is_identical_to(flcs_in[[element]]))
    expect_that(test_FLCatches_get_single_index_accessor(flcs_in, element), is_identical_to(flcs_in[[element]]))
    # get a value in landings_n in an FLCatch in the FLCatches
    value_out <- test_FLCatches_const_get_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(value_out, is_identical_to(c(landings.n(flcs_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    value_out <- test_FLCatches_get_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(value_out, is_identical_to(c(landings.n(flcs_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    # set a catch
    flcs_out <- test_FLCatches_set_single_index_accessor(flcs_in, element, flc_in)
    expect_that(flcs_out[[element]], is_identical_to(flc_in))
    expect_that(flcs_out[-element], is_identical_to(flcs_in[-element]))
    # set a value in landings_n in an FLCatch in the FLCatches
    flcs_out <- test_FLCatches_set_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flcs_out[-element], is_identical_to(flcs_in[-element]))
    expect_that(c(landings.n(flcs_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    expect_that(landings.wt(flcs_out[[element]]), is_identical_to(landings.wt(flcs_in[[element]]))) # just checking other slots are not affected
    # single index - default position is 1
    expect_that(test_FLCatches_const_get_single_index_default_accessor(flcs_in), is_identical_to(flcs_in[[1]]))
    expect_that(test_FLCatches_get_single_index_default_accessor(flcs_in), is_identical_to(flcs_in[[1]]))
    flcs_out <- test_FLCatches_set_single_index_default_accessor(flcs_in, flc_in)
    expect_that(flcs_out[[1]], is_identical_to(flc_in))
    expect_that(flcs_out[-1], is_identical_to(flcs_in[-1]))
    # Check outside bounds
    expect_that(test_FLCatches_const_get_single_index_accessor(flcs_in, length(flcs_in)+1), throws_error())
    expect_that(test_FLCatches_get_single_index_accessor(flcs_in, length(flcs_in)+1), throws_error())
    expect_that(test_FLCatches_set_single_index_accessor(flcs_in, length(flcs_in)+1, flc_in), throws_error())
})
#----------------------------------
test_that("FLCatches as and wrap - adouble",{
    flcs_in <- random_FLCatches_generator() 
    flcs_out <- test_FLCatchesAD_as_wrap(flcs_in)
    expect_that(flcs_in, is_identical_to(flcs_out))
})

test_that("FLCatchesAD constructors - adouble",{
    flcs_in <- random_FLCatches_generator() 
    flc_in <- random_FLCatch_generator() 
    # SEXP constructor - used in as
    flcs_out <- test_FLCatchesAD_sexp_constructor(flcs_in)
    expect_that(flcs_in, is_identical_to(flcs_out))
    # FLCatch constructor
    flcs_out <- test_FLCatchesAD_FLCatchAD_constructor(flc_in)
    expect_that(flc_in, is_identical_to(flcs_out[[1]]))
    # flcs <- FLCatchesAD(list(flc_in)) # names not set right - should be NA
    # Copy constructor
    flcs_out <- test_FLCatchesAD_copy_constructor(flcs_in)
    expect_that(flcs_in, is_identical_to(flcs_out))
    # Copy constructor2 - checking for deep copy
    element <- round(runif(1,min=1, max = length(flcs_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flcs_in[[element]]))))
    value <- rnorm(1)
    # Makes a copy of flcs_in, changes a value of flcs_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flcss <-  test_FLCatchesAD_copy_constructor2(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flcs_in, is_identical_to(flcss[["flcs1"]]))
    expect_that(c(landings.n(flcss[["flcs2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Assignment operator
    flcs_out <- test_FLCatchesAD_assignment_operator(flcs_in)
    expect_that(flcs_in, is_identical_to(flcs_out))
    # Assignment operator2
    flcss <-  test_FLCatchesAD_assignment_operator2(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flcs_in, is_identical_to(flcss[["flcs1"]]))
    expect_that(c(landings.n(flcss[["flcs2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Testing function operator - adds another FLCatch in
    flcs_out <-  test_FLCatchesAD_function_operator(flcs_in, flc_in)
    expect_that(length(flcs_in)+as.integer(1), is_identical_to(length(flcs_out)))
    for (i in 1:(length(flcs_in)-1)){
        expect_that(flcs_out[i], is_identical_to(flcs_in[i]))
        expect_that(flcs_out[[i]], is_identical_to(flcs_in[[i]]))
    }
    expect_that(flc_in, is_identical_to(flcs_out[[length(flcs_in)+1]]))
    expect_that(flcs_in@desc, is_identical_to(flcs_out@desc))
    # not check names because we haven't given the new one a name
})

test_that("FLCatchesAD get accessors - double",{
    flcs_in <- random_FLCatches_generator()
    expect_that(test_FLCatchesAD_get_ncatches(flcs_in), is_identical_to(length(flcs_in)))
})

test_that("FLCatchesAD get and set data accessors - double", {
    flcs_in <- random_FLCatches_generator()
    flc_in <- random_FLCatch_generator()
    element <- round(runif(1,min=1, max = length(flcs_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flcs_in[[element]]))))
    value <- rnorm(1)
    # single index
    expect_that(test_FLCatchesAD_const_get_single_index_accessor(flcs_in, element), is_identical_to(flcs_in[[element]]))
    expect_that(test_FLCatchesAD_get_single_index_accessor(flcs_in, element), is_identical_to(flcs_in[[element]]))
    # get a value in landings_n in an FLCatch in the FLCatchesAD
    value_out <- test_FLCatchesAD_const_get_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(value_out, is_identical_to(c(landings.n(flcs_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    value_out <- test_FLCatchesAD_get_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(value_out, is_identical_to(c(landings.n(flcs_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    # set a catch
    flcs_out <- test_FLCatchesAD_set_single_index_accessor(flcs_in, element, flc_in)
    expect_that(flcs_out[[element]], is_identical_to(flc_in))
    expect_that(flcs_out[-element], is_identical_to(flcs_in[-element]))
    # set a value in landings_n in an FLCatch in the FLCatchesAD
    flcs_out <- test_FLCatchesAD_set_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flcs_out[-element], is_identical_to(flcs_in[-element]))
    expect_that(c(landings.n(flcs_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    expect_that(landings.wt(flcs_out[[element]]), is_identical_to(landings.wt(flcs_in[[element]]))) # just checking other slots are not affected
    # single index - default position is 1
    expect_that(test_FLCatchesAD_const_get_single_index_default_accessor(flcs_in), is_identical_to(flcs_in[[1]]))
    expect_that(test_FLCatchesAD_get_single_index_default_accessor(flcs_in), is_identical_to(flcs_in[[1]]))
    flcs_out <- test_FLCatchesAD_set_single_index_default_accessor(flcs_in, flc_in)
    expect_that(flcs_out[[1]], is_identical_to(flc_in))
    expect_that(flcs_out[-1], is_identical_to(flcs_in[-1]))
    # Check outside bounds
    expect_that(test_FLCatchesAD_const_get_single_index_accessor(flcs_in, length(flcs_in)+1), throws_error())
    expect_that(test_FLCatchesAD_get_single_index_accessor(flcs_in, length(flcs_in)+1), throws_error())
    expect_that(test_FLCatchesAD_set_single_index_accessor(flcs_in, length(flcs_in)+1, flc_in), throws_error())
})

