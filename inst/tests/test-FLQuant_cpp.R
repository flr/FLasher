context("CPP implementation of FLQuant - double")

test_that("FLQuant as and wrap",{
    flq_in <- random_FLQuant_generator()
    flq_out <- test_FLQuant_as_wrap(flq_in)
    expect_identical(flq_in, flq_out)
})

test_that("FLQuant constructors",{
    flq_in <- random_FLQuant_generator()
    # Empty constructor - doesn't do anything - but shouldn't fail
    test_FLQuant_basic_constructor()
    # SEXP constructor - used in as
    flq_out <- test_FLQuant_sexp_constructor(flq_in)
    expect_identical(flq_in, flq_out)
    # dim constructor
    dims <- round(runif(6, min = 1, max = 5))
    flq_out <- test_FLQuant_dim_constructor(dims[1], dims[2], dims[3], dims[4], dims[5], dims[6])
    expect_true(all(flq_out == 0)) 
    expect_equal(dim(flq_out), dims)
    # Copy constructor
    flq_out <- test_FLQuant_copy_constructor(flq_in)
    expect_identical(flq_in, flq_out)
    # Copy constructor2
    indices <- round(runif(6,min=1, max = dim(flq_in)))
    value <- rnorm(1)
    # Makes a copy of flq_in, changes a value of flq_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flqs <-  test_FLQuant_copy_constructor2(flq_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flq_in, flqs[["flq1"]])
    expect_identical(c(flqs[["flq2"]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Assignment operator
    flq_out <- test_FLQuant_assignment_operator(flq_in)
    expect_identical(flq_in, flq_out)
    # Assignment operator2
    flqs <-  test_FLQuant_assignment_operator2(flq_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(flq_in, flqs[["flq1"]])
    expect_identical(c(flqs[["flq2"]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
})

test_that("get accessors",{
    flq <- random_FLQuant_generator()
    expect_identical(test_FLQuant_get_data(flq), c(flq@.Data))
    expect_identical(test_FLQuant_get_units(flq), units(flq))
    expect_equal(test_FLQuant_get_dim(flq), dim(flq))
    expect_identical(test_FLQuant_get_dimnames(flq), dimnames(flq))
    # Test deep copy is returned with dimnames are got
    dmns_out <- test_FLQuant_get_dimnames2(flq)
    expect_identical(dimnames(dmns_out[[1]]), dimnames(flq))
    expect_identical(dmns_out[[2]][[1]], "all")
    expect_identical(test_FLQuant_get_size(flq), length(c(flq@.Data)))
    expect_identical(test_FLQuant_get_nquant(flq), dim(flq)[1])
    expect_identical(test_FLQuant_get_nyear(flq), dim(flq)[2])
    expect_identical(test_FLQuant_get_nunit(flq), dim(flq)[3])
    expect_identical(test_FLQuant_get_nseason(flq), dim(flq)[4])
    expect_identical(test_FLQuant_get_narea(flq), dim(flq)[5])
    expect_identical(test_FLQuant_get_niter(flq), dim(flq)[6])
    indices <- round(runif(6,min=1, max = dim(flq)))
    value <- rnorm(1)
    element <- test_FLQuant_get_data_element(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), c(flq)[element+1])
    flq <- random_FLQuant_generator()
    indices <- round(runif(6,min=1, max = dim(flq)))
    out <- test_FLQuant_get_all_iters(flq, indices[1], indices[2], indices[3], indices[4], indices[5])
    expect_identical(out, flq[indices[1], indices[2], indices[3], indices[4], indices[5],])
})

test_that("set",{
    #set_data
    flq1 <- random_FLQuant_generator()
    flq2 <- flq1
    flq2[] <- rnorm(prod(dim(flq2)))
    flq_out <- test_FLQuant_set_data(flq1, flq2@.Data)
    expect_identical(flq2, flq_out)
    # set_dimnames
    flq_in <- random_FLQuant_generator()
    new_dimnames <- dimnames(flq_in)
    new_dimnames[[1]][1] <- as.character(rnorm(1))
    flq_out <- test_FLQuant_set_dimnames(flq_in, new_dimnames)
    expect_identical(dimnames(flq_out)[[1]][1], new_dimnames[[1]][1])
    expect_identical(c(flq_out@.Data), c(flq_in@.Data))
    expect_identical(dim(flq_out), dim(flq_in))
    # Check failure
    new_dimnames[[3]] <- c(new_dimnames[[3]],"extra")
    expect_error(test_FLQuant_set_dimnames(flq_in, new_dimnames))
    # set units
    flq_in <- random_FLQuant_generator()
    new_units <- as.character(rnorm(1))
    flq_out <- test_FLQuant_set_units(flq_in, new_units)
    expect_identical(units(flq_out), new_units)
    # fill
    flq <- random_FLQuant_generator()
    value <- rnorm(1)
    flq_out <- test_FLQuant_fill(flq, value)
    expect_true(all(flq_out == value))
    flq_out <- test_FLQuantAD_fill(flq, value)
    expect_true(all(flq_out == value))
    flq_out <- test_FLQuantAD_fill_double(flq, value)
    expect_true(all(flq_out == value))
})

test_that("FLQuant get and set data accessors", {
    flq <- random_FLQuant_generator()
    indices <- round(runif(6,min=1, max = dim(flq)))
    index <- round(runif(1,min=1,max = prod(dim(flq))))
    value <- rnorm(1)
    # single index
    expect_identical(test_FLQuant_get_const_single_index_accessor(flq, index), c(flq@.Data)[index])
    expect_identical(test_FLQuant_get_single_index_accessor(flq, index), c(flq@.Data)[index])
    flq_out <- test_FLQuant_set_single_index_accessor(flq, index, value)
    expect_identical(c(flq_out)[index], value)
    # multiple indices
    value_out <- test_FLQuant_const_get_accessor(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value_out)
    value_out <- test_FLQuant_get_accessor(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value_out)
    flq_out <- test_FLQuant_set_accessor(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_identical(c(flq_out[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
    # Check outside bounds
    big_index <- prod(dim(flq))+1
    big_indices1 <- dim(flq) + c(1,0,0,0,0,0)
    big_indices2 <- dim(flq) + c(0,1,0,0,0,0)
    big_indices3 <- dim(flq) + c(0,0,1,0,0,0)
    big_indices4 <- dim(flq) + c(0,0,0,1,0,0)
    big_indices5 <- dim(flq) + c(0,0,0,0,1,0)
    big_indices6 <- dim(flq) + c(0,0,0,0,0,1)
    expect_error(test_FLQuant_get_const_single_index_accessor(flq, big_index))
    expect_error(test_FLQuant_get_single_index_accessor(flq, big_index))
    expect_error(test_FLQuant_set_single_index_accessor(flq, big_index, value))
    expect_error(test_FLQuant_const_get_accessor(flq, big_indices1[1], big_indices1[2], big_indices1[3], big_indices1[4], big_indices1[5], big_indices1[6]))
    expect_error(test_FLQuant_const_get_accessor(flq, big_indices2[1], big_indices2[2], big_indices2[3], big_indices2[4], big_indices2[5], big_indices2[6]))
    expect_error(test_FLQuant_const_get_accessor(flq, big_indices3[1], big_indices3[2], big_indices3[3], big_indices3[4], big_indices3[5], big_indices3[6]))
    expect_error(test_FLQuant_const_get_accessor(flq, big_indices4[1], big_indices4[2], big_indices4[3], big_indices4[4], big_indices4[5], big_indices4[6]))
    expect_error(test_FLQuant_const_get_accessor(flq, big_indices5[1], big_indices5[2], big_indices5[3], big_indices5[4], big_indices5[5], big_indices5[6]))
    expect_error(test_FLQuant_const_get_accessor(flq, big_indices6[1], big_indices6[2], big_indices6[3], big_indices6[4], big_indices6[5], big_indices6[6]))
    expect_error(test_FLQuant_get_accessor(flq, big_indices1[1], big_indices1[2], big_indices1[3], big_indices1[4], big_indices1[5], big_indices1[6]))
    expect_error(test_FLQuant_get_accessor(flq, big_indices2[1], big_indices2[2], big_indices2[3], big_indices2[4], big_indices2[5], big_indices2[6]))
    expect_error(test_FLQuant_get_accessor(flq, big_indices3[1], big_indices3[2], big_indices3[3], big_indices3[4], big_indices3[5], big_indices3[6]))
    expect_error(test_FLQuant_get_accessor(flq, big_indices4[1], big_indices4[2], big_indices4[3], big_indices4[4], big_indices4[5], big_indices4[6]))
    expect_error(test_FLQuant_get_accessor(flq, big_indices5[1], big_indices5[2], big_indices5[3], big_indices5[4], big_indices5[5], big_indices5[6]))
    expect_error(test_FLQuant_get_accessor(flq, big_indices6[1], big_indices6[2], big_indices6[3], big_indices6[4], big_indices6[5], big_indices6[6]))
    expect_error(test_FLQuant_set_accessor(flq, big_indices1[1], big_indices1[2], big_indices1[3], big_indices1[4], big_indices1[5], big_indices1[6], value))
    expect_error(test_FLQuant_set_accessor(flq, big_indices2[1], big_indices2[2], big_indices2[3], big_indices2[4], big_indices2[5], big_indices2[6], value))
    expect_error(test_FLQuant_set_accessor(flq, big_indices3[1], big_indices3[2], big_indices3[3], big_indices3[4], big_indices3[5], big_indices3[6], value))
    expect_error(test_FLQuant_set_accessor(flq, big_indices4[1], big_indices4[2], big_indices4[3], big_indices4[4], big_indices4[5], big_indices4[6], value))
    expect_error(test_FLQuant_set_accessor(flq, big_indices5[1], big_indices5[2], big_indices5[3], big_indices5[4], big_indices5[5], big_indices5[6], value))
    expect_error(test_FLQuant_set_accessor(flq, big_indices6[1], big_indices6[2], big_indices6[3], big_indices6[4], big_indices6[5], big_indices6[6], value))
    # indices accessor
    expect_identical(test_FLQuant_get_const_indices_accessor(flq, indices), c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_error(test_FLQuant_get_const_indices_accessor(flq, c(indices,1)))
    expect_error(test_FLQuant_get_const_indices_accessor(flq, indices[-1]))
    expect_identical(test_FLQuant_get_indices_accessor(flq, indices), c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]))
    expect_error(test_FLQuant_get_indices_accessor(flq, c(indices,1)))
    expect_error(test_FLQuant_get_indices_accessor(flq, indices[-1]))
    value <- rnorm(1)
    flq_out <- test_FLQuant_set_indices_accessor(flq, indices, value)
    expect_identical(c(flq_out[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), value)
})

test_that("FLQuant match_dims method works", {
    flq <- random_FLQuant_generator()
    dim_flq <- dim(flq)
    dim_flq1 <- dim_flq + c(1,0,0,0,0,0)
    flq1 <- FLQuant(0, dim = dim_flq + c(1,0,0,0,0,0))
    flq2 <- FLQuant(0, dim = dim_flq + c(0,1,0,0,0,0)) 
    flq3 <- FLQuant(0, dim = dim_flq + c(0,0,1,0,0,0))
    flq4 <- FLQuant(0, dim = dim_flq + c(0,0,0,1,0,0))
    flq5 <- FLQuant(0, dim = dim_flq + c(0,0,0,0,1,0))
    flq6 <- FLQuant(0, dim = dim_flq + c(0,0,0,0,0,1))
    expect_identical(test_FLQuant_FLQuant_match_dims(flq, flq), 1L)
    expect_identical(test_FLQuant_FLQuant_match_dims(flq, flq1), -1L)
    expect_identical(test_FLQuant_FLQuant_match_dims(flq, flq2), -2L)
    expect_identical(test_FLQuant_FLQuant_match_dims(flq, flq3), -3L)
    expect_identical(test_FLQuant_FLQuant_match_dims(flq, flq4), -4L)
    expect_identical(test_FLQuant_FLQuant_match_dims(flq, flq5), -5L)
    expect_identical(test_FLQuant_FLQuant_match_dims(flq, flq6), -6L)
})

test_that("FLQuant subsetter works",{
    fixed_dims <- round(runif(6, min=5, max = 10))
    flq <- random_FLQuant_generator(fixed_dims = fixed_dims)
    sub_dims_start <- round(runif(6, min=1, max = 2))
    sub_dims_end <- fixed_dims - round(runif(6, min=1, max = 2))
    sub_flq_out <-  test_FLQuant_subset(flq, sub_dims_start[1], sub_dims_end[1], sub_dims_start[2], sub_dims_end[2], sub_dims_start[3], sub_dims_end[3], sub_dims_start[4], sub_dims_end[4], sub_dims_start[5], sub_dims_end[5], sub_dims_start[6], sub_dims_end[6])
    sub_flq_in <- flq[sub_dims_start[1]:sub_dims_end[1], sub_dims_start[2]:sub_dims_end[2], sub_dims_start[3]:sub_dims_end[3], sub_dims_start[4]:sub_dims_end[4], sub_dims_start[5]:sub_dims_end[5], sub_dims_start[6]:sub_dims_end[6]]
    expect_identical(c(sub_flq_in@.Data), c(sub_flq_out@.Data))
    expect_identical(sub_flq_in@.Data, sub_flq_out@.Data)
    # min < max check
    sub_dims_wrong <- sub_dims_end
    expect_error(test_FLQuant_subset(flq, sub_dims_wrong[1], sub_dims_start[1], sub_dims_wrong[2], sub_dims_start[2], sub_dims_wrong[3], sub_dims_start[3], sub_dims_wrong[4], sub_dims_start[4], sub_dims_wrong[5], sub_dims_start[5], sub_dims_wrong[6], sub_dims_start[6]))
    # Test std::vector<unsigned int> subsetter
    flq <- random_FLQuant_generator()
    dims_max <- dim(flq)
    dims_min <- round(runif(6, min=1,max=dims_max))
    flq_out <- test_FLQuant_neat_subset(flq, dims_min, dims_max)
    expect_equal(flq[dims_min[1]:dims_max[1], dims_min[2]:dims_max[2], dims_min[3]:dims_max[3], dims_min[4]:dims_max[4], dims_min[5]:dims_max[5], dims_min[6]:dims_max[6]], flq_out)
    # indices wrong - should throw error
    expect_error(test_FLQuant_neat_subset(flq, dims_min[-1], dims_max))
    expect_error(test_FLQuant_neat_subset(flq, dims_min, dims_max[-1]))
    expect_error(test_FLQuant_neat_subset(flq, c(1,dims_min), dims_max))
    expect_error(test_FLQuant_neat_subset(flq, dims_min, c(1,dims_max)))
})

test_that("Accessing FLQuant iter = 1 or n works",{
    niters <- round(runif(1,min=5,max=10))
    flq <- random_FLQuant_generator(fixed_dim=c(NA,NA,NA,NA,NA,niters))
    dim_flq <- dim(flq)
    dim_flq[6] <- 1
    single_iter_flq <- random_FLQuant_generator(fixed_dim = dim_flq)
    indices <- round(runif(6,min=1, max = dim(flq)))
    index <- round(runif(1,min=1,max = prod(dim(flq))))
    value <- rnorm(1)
    # multiple indices
    value_out <- test_FLQuant_const_get_accessor(single_iter_flq, indices[1], indices[2], indices[3], indices[4], indices[5], 1)
    expect_identical(c(single_iter_flq[indices[1], indices[2], indices[3], indices[4], indices[5], 1]), value_out)
    # Accessing more iters than you can
    value_out2 <- test_FLQuant_const_get_accessor(single_iter_flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_identical(c(single_iter_flq[indices[1], indices[2], indices[3], indices[4], indices[5], 1]), value_out)
    expect_error(test_FLQuant_const_get_accessor(flq, indices[1], indices[2], indices[3], indices[4], indices[5], dim(flq)[6]+1))
})

test_that("Propagating FLQuant iters", {
    flq <- random_FLQuant_generator(fixed_dim=c(NA,NA,NA,NA,NA,1))
    niters <- round(runif(1, min = 5, max = 10))
    flq_out <- test_FLQuant_propagate_iters(flq, niters)
    flq_prop <- propagate(flq,niters)
    # Check original is unchanged
    expect_identical(flq, flq_out[["flq"]])
    expect_identical(flq_prop, flq_out[["flq2"]])
})

test_that("FLPar_to_FLQuant", {
    # Need to check dimnames of FLPar before dispatch that dimnames are only FLQuant dimnames
    # Make some random FLPars
    flq <- random_FLQuant_generator()
    names(dimnames(flq))[1] <- "params"
    flp2D <- FLPar(rnorm(dim(flq)[1] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,6)])
    flp3D1 <- FLPar(rnorm(dim(flq)[1] * dim(flq)[2] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,2,6)])
    flp3D2 <- FLPar(rnorm(dim(flq)[1] * dim(flq)[4] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,4,6)])
    flp4D <- FLPar(rnorm(dim(flq)[1] * dim(flq)[2] * dim(flq)[4] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,2,4,6)])
    flp5D1 <- FLPar(rnorm(dim(flq)[1] * dim(flq)[2] * dim(flq)[3] * dim(flq)[4] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,2,3,4,6)])
    flp5D2 <- FLPar(rnorm(dim(flq)[1] * dim(flq)[2] * dim(flq)[4] * dim(flq)[5] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,2,4,5,6)])
    flp6D <- FLPar(rnorm(prod(dim(flq))), dimnames=dimnames(flq))
    flp7D <- FLPar(rnorm(dim(flq)[1] * dim(flq)[2] * dim(flq)[4] * dim(flq)[5] * 3 * dim(flq)[6]), dimnames=c(dimnames(flq)[1:5], other=list(c("a","b","c")), dimnames(flq)[6]))
    # 2D
    flq_out <- test_FLPar_to_FLQuant(flp2D)
    expect_equal(dim(flq_out), unname(c(dim(flp2D)[1],1,1,1,1,dim(flp2D)[2])))
    expect_equal(c(flq_out), c(flp2D))
    # 3D
    flq_out <- test_FLPar_to_FLQuant(flp3D1)
    expect_equal(dim(flq_out), unname(c(dim(flp3D1)[1],dim(flp3D1)[2],1,1,1,dim(flp3D1)[3])))
    expect_equal(c(flq_out), c(flp3D1))
    flq_out <- test_FLPar_to_FLQuant(flp3D2)
    expect_equal(dim(flq_out), unname(c(dim(flp3D2)[1],1,1,dim(flp3D2)[2],1,dim(flp3D2)[3])))
    expect_equal(c(flq_out), c(flp3D2))
    # 4D
    flq_out <- test_FLPar_to_FLQuant(flp4D)
    expect_equal(dim(flq_out), unname(c(dim(flp4D)[1],dim(flp4D)[2],1,dim(flp4D)[3],1,dim(flp4D)[4])))
    expect_equal(c(flq_out), c(flp4D))
    # 5D
    flq_out <- test_FLPar_to_FLQuant(flp5D1)
    expect_equal(dim(flq_out), unname(c(dim(flp5D1)[1],dim(flp5D1)[2],dim(flp5D1)[3],dim(flp5D1)[4],1,dim(flp5D1)[5])))
    expect_equal(c(flq_out), c(flp5D1))
    flq_out <- test_FLPar_to_FLQuant(flp5D2)
    expect_equal(dim(flq_out), unname(c(dim(flp5D2)[1],dim(flp5D2)[2],1,dim(flp5D2)[3],dim(flp5D2)[4],dim(flp5D2)[5])))
    expect_equal(c(flq_out), c(flp5D2))
    # 6D
    flq_out <- test_FLPar_to_FLQuant(flp6D)
    expect_equal(dim(flq_out), unname(c(dim(flp6D))))
    expect_equal(c(flq_out), c(flp6D))
    # > 7D (fail)
    expect_error(test_FLPar_to_FLQuant(flp7D))
    # Wrong dimname - fail
    flp2D2 <- FLPar(rnorm(dim(flq)[1] * dim(flq)[6] * 5), dimnames=list(params = dimnames(flq)[[1]], other_name = c("a","b","c","d","e"), iter = dimnames(flq)[[6]]))
    expect_error(test_FLPar_to_FLQuant(flp2D2))
})

test_that("iterators", {
    flq_in <- random_FLQuant_generator()
    rn <- rnorm(1)
    # for_range (uses begin and end, const and non const versions)
    flq_out <- test_for_range(flq_in, rn)
    expect_equal(flq_in * rn, flq_out)
    out <- test_for_range_const(flq_in, rn)
    expect_equal(out, sum(flq_in * rn))
    # for loop with an iterator, const and otherwise
    flq_out <- test_FLQuant_for_iterator(flq_in, rn)
    expect_equal(flq_in * rn, flq_out)
    out <- test_FLQuant_for_iterator_const(flq_in, rn)
    expect_equal(out, sum(flq_in * rn))
    # transform lambda function
    flq1 <- random_FLQuant_generator()
    flq2 <- random_FLQuant_generator(fixed_dims = dim(flq1))
    flq_out <- test_FLQuant_lambda(flq1, flq2)
    expect_equal(flq_out@.Data, sqrt(flq1^2 + (flq2^2))@.Data)
})
