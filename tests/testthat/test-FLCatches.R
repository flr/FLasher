context("Implementation of FLCatches - double and AD versions")

test_that("FLCatches as and wrap - double",{
    flcs_in <- random_FLCatches_generator() 
    flcs_out <- test_FLCatches_as_wrap(flcs_in)
    expect_identical(flcs_in, flcs_out)
})

test_that("FLCatches constructors - double",{
    flcs_in <- random_FLCatches_generator() 
    flc_in <- random_FLCatch_generator() 
    # SEXP constructor - used in as
    flcs_out <- test_FLCatches_sexp_constructor(flcs_in)
    expect_identical(flcs_in, flcs_out)
    # FLCatch constructor
    flcs_out <- test_FLCatches_FLCatch_constructor(flc_in)
    expect_identical(flc_in, flcs_out[[1]])
    # Copy constructor
    flcs_out <- test_FLCatches_copy_constructor(flcs_in)
    expect_identical(flcs_in, flcs_out)
    # Copy constructor2 - checking for deep copy
    element <- round(runif(1,min=1, max = length(flcs_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flcs_in[[element]]))))
    value <- rnorm(1)
    # Makes a copy of flcs_in, changes a value of flcs_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flcss <-  test_FLCatches_copy_constructor2(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flcs_in, flcss[["flcs1"]])
    expect_identical(c(landings.n(flcss[["flcs2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Assignment operator
    flcs_out <- test_FLCatches_assignment_operator(flcs_in)
    expect_identical(flcs_in, flcs_out)
    # Assignment operator2
    flcss <-  test_FLCatches_assignment_operator2(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flcs_in, flcss[["flcs1"]])
    expect_identical(c(landings.n(flcss[["flcs2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Testing function operator - adds another FLCatch in
    flcs_out <-  test_FLCatches_function_operator(flcs_in, flc_in)
    expect_identical(length(flcs_in)+as.integer(1), length(flcs_out))
    for (i in 1:(length(flcs_in)-1)){
        expect_identical(flcs_out[i], flcs_in[i])
        expect_identical(flcs_out[[i]], flcs_in[[i]])
    }
    expect_identical(flc_in, flcs_out[[length(flcs_in)+1]])
    expect_identical(flcs_in@desc, flcs_out@desc)
    # not check names because we haven't given the new one a name
})

test_that("FLCatches get accessors - double",{
    flcs_in <- random_FLCatches_generator()
    expect_identical(test_FLCatches_get_ncatches(flcs_in), length(flcs_in))
})

test_that("FLCatches get and set data accessors - double", {
    flcs_in <- random_FLCatches_generator()
    flc_in <- random_FLCatch_generator()
    element <- round(runif(1,min=1, max = length(flcs_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flcs_in[[element]]))))
    value <- rnorm(1)
    # single index
    expect_identical(test_FLCatches_const_get_single_index_accessor(flcs_in, element), flcs_in[[element]])
    expect_identical(test_FLCatches_get_single_index_accessor(flcs_in, element), flcs_in[[element]])
    # get a value in landings_n in an FLCatch in the FLCatches
    value_out <- test_FLCatches_const_get_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(value_out, c(landings.n(flcs_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    value_out <- test_FLCatches_get_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(value_out, c(landings.n(flcs_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # set a catch
    flcs_out <- test_FLCatches_set_single_index_accessor(flcs_in, element, flc_in)
    expect_identical(flcs_out[[element]], flc_in)
    expect_identical(flcs_out[-element], flcs_in[-element])
    # set a value in landings_n in an FLCatch in the FLCatches
    flcs_out <- test_FLCatches_set_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flcs_out[-element], flcs_in[-element])
    expect_identical(c(landings.n(flcs_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    expect_identical(landings.wt(flcs_out[[element]]), landings.wt(flcs_in[[element]])) # just checking other slots are not affected

    # Check outside bounds
    expect_error(test_FLCatches_const_get_single_index_accessor(flcs_in, length(flcs_in)+1))
    expect_error(test_FLCatches_get_single_index_accessor(flcs_in, length(flcs_in)+1))
    expect_error(test_FLCatches_set_single_index_accessor(flcs_in, length(flcs_in)+1, flc_in))
})
#----------------------------------
test_that("FLCatches as and wrap - adouble",{
    flcs_in <- random_FLCatches_generator() 
    flcs_out <- test_FLCatchesAD_as_wrap(flcs_in)
    expect_identical(flcs_in, flcs_out)
})

test_that("FLCatchesAD constructors - adouble",{
    flcs_in <- random_FLCatches_generator() 
    flc_in <- random_FLCatch_generator() 
    # SEXP constructor - used in as
    flcs_out <- test_FLCatchesAD_sexp_constructor(flcs_in)
    expect_identical(flcs_in, flcs_out)
    # FLCatch constructor
    flcs_out <- test_FLCatchesAD_FLCatchAD_constructor(flc_in)
    expect_identical(flc_in, flcs_out[[1]])
    # flcs <- FLCatchesAD(list(flc_in)) # names not set right - should be NA
    # Copy constructor
    flcs_out <- test_FLCatchesAD_copy_constructor(flcs_in)
    expect_identical(flcs_in, flcs_out)
    # Copy constructor2 - checking for deep copy
    element <- round(runif(1,min=1, max = length(flcs_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flcs_in[[element]]))))
    value <- rnorm(1)
    # Makes a copy of flcs_in, changes a value of flcs_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flcss <-  test_FLCatchesAD_copy_constructor2(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flcs_in, flcss[["flcs1"]])
    expect_identical(c(landings.n(flcss[["flcs2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Assignment operator
    flcs_out <- test_FLCatchesAD_assignment_operator(flcs_in)
    expect_identical(flcs_in, flcs_out)
    # Assignment operator2
    flcss <-  test_FLCatchesAD_assignment_operator2(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flcs_in, flcss[["flcs1"]])
    expect_identical(c(landings.n(flcss[["flcs2"]][[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Testing function operator - adds another FLCatch in
    flcs_out <-  test_FLCatchesAD_function_operator(flcs_in, flc_in)
    expect_identical(length(flcs_in)+as.integer(1), length(flcs_out))
    for (i in 1:(length(flcs_in)-1)){
        expect_identical(flcs_out[i], flcs_in[i])
        expect_identical(flcs_out[[i]], flcs_in[[i]])
    }
    expect_identical(flc_in, flcs_out[[length(flcs_in)+1]])
    expect_identical(flcs_in@desc, flcs_out@desc)
    # not check names because we haven't given the new one a name
})

test_that("FLCatchesAD get accessors - double",{
    flcs_in <- random_FLCatches_generator()
    expect_identical(test_FLCatchesAD_get_ncatches(flcs_in), length(flcs_in))
})

test_that("FLCatchesAD get and set data accessors - double", {
    flcs_in <- random_FLCatches_generator()
    flc_in <- random_FLCatch_generator()
    element <- round(runif(1,min=1, max = length(flcs_in)))
    indices <- round(runif(6,min=1, max = dim(landings.n(flcs_in[[element]]))))
    value <- rnorm(1)
    # single index
    expect_identical(test_FLCatchesAD_const_get_single_index_accessor(flcs_in, element), flcs_in[[element]])
    expect_identical(test_FLCatchesAD_get_single_index_accessor(flcs_in, element), flcs_in[[element]])
    # get a value in landings_n in an FLCatch in the FLCatchesAD
    value_out <- test_FLCatchesAD_const_get_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(value_out, c(landings.n(flcs_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    value_out <- test_FLCatchesAD_get_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(value_out, c(landings.n(flcs_in[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    # set a catch
    flcs_out <- test_FLCatchesAD_set_single_index_accessor(flcs_in, element, flc_in)
    expect_identical(flcs_out[[element]], flc_in)
    expect_identical(flcs_out[-element], flcs_in[-element])
    # set a value in landings_n in an FLCatch in the FLCatchesAD
    flcs_out <- test_FLCatchesAD_set_value_accessor(flcs_in, element, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flcs_out[-element], flcs_in[-element])
    expect_identical(c(landings.n(flcs_out[[element]])[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    expect_identical(landings.wt(flcs_out[[element]]), landings.wt(flcs_in[[element]])) # just checking other slots are not affected
    # single index - default position is 1
    # Check outside bounds
    expect_error(test_FLCatchesAD_const_get_single_index_accessor(flcs_in, length(flcs_in)+1))
    expect_error(test_FLCatchesAD_get_single_index_accessor(flcs_in, length(flcs_in)+1))
    expect_error(test_FLCatchesAD_set_single_index_accessor(flcs_in, length(flcs_in)+1, flc_in))
})

test_that("FLCatchesAD iterators",{
    catches <- random_FLCatches_generator(min_catches = 2, max_catches = 5)
    landingsnin <- lapply(catches, function(x) return(landings.n(x)))
    # Const - just pulls out n
    landingsnout <- test_FLCatchesAD_const_iterator(catches)
    expect_identical(landingsnout, landingsnin@.Data)
    # Not const - sets a value
    indices <- round(runif(6, min=1, max=dim(landings.n(catches[[1]]))))
    value <- rnorm(1)
    catches_out <- test_FLCatchesAD_iterator(catches, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    value_out <- unname(unlist(lapply(catches_out, function(x) return(landings.n(x)[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))))
    expect_identical(rep(value,length(catches)), value_out)
    # All others are OK
    element <- get_FLQuant_element(landings.n(catches[[1]]), indices)
    for (i in 1:length(catches)){
        expect_identical(c(landings.n(catches_out[[i]]))[-element], c(landings.n(catches[[i]]))[-element])
    }
})

