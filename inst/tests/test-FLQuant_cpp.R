context("CPP implementation of FLQuant")

test_that("FLQuant as and wrap",{
    flq_in <- random_FLQuant_generator()
    flq_out <- test_FLQuant_as_wrap(flq_in)
    expect_that(flq_in, is_identical_to(flq_out))
})

test_that("FLQuant constructors",{
    flq_in <- random_FLQuant_generator()
    # Empty constructor - doesn't do anything - but shouldn't fail
    test_FLQuant_basic_constructor()
    # SEXP constructor - used in as
    flq_out <- test_FLQuant_sexp_constructor(flq_in)
    expect_that(flq_in, is_identical_to(flq_out))
    # dim constructor
    dims <- round(runif(6, min = 1, max = 5))
    flq_out <- test_FLQuant_dim_constructor(dims[1], dims[2], dims[3], dims[4], dims[5], dims[6])
    expect_that(all(flq_out == 0), is_true())
    expect_that(dim(flq_out), equals(dims))
    # Copy constructor
    flq_out <- test_FLQuant_copy_constructor(flq_in)
    expect_that(flq_in, is_identical_to(flq_out))
    # Copy constructor2
    indices <- round(runif(6,min=1, max = dim(flq_in)))
    value <- rnorm(1)
    # Makes a copy of flq_in, changes a value of flq_in, returns original and new FLQuant
    # Checks that the copy constuctor makes a 'deep' copy else changing a value in the copy FLQ will also change a value in the original FLQ
    flqs <-  test_FLQuant_copy_constructor2(flq_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flq_in, is_identical_to(flqs[["flq1"]]))
    expect_that(c(flqs[["flq2"]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Assignment operator
    flq_out <- test_FLQuant_assignment_operator(flq_in)
    expect_that(flq_in, is_identical_to(flq_out))
    # Assignment operator2
    flqs <-  test_FLQuant_assignment_operator2(flq_in, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(flq_in, is_identical_to(flqs[["flq1"]]))
    expect_that(c(flqs[["flq2"]][indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
})

test_that("get accessors",{
    flq <- random_FLQuant_generator()
    expect_that(test_FLQuant_get_data(flq), is_identical_to(c(flq@.Data)))
    expect_that(test_FLQuant_get_units(flq), is_identical_to(units(flq)))
    expect_that(test_FLQuant_get_dim(flq), is_identical_to(dim(flq)))
    expect_that(test_FLQuant_get_dimnames(flq), is_identical_to(dimnames(flq)))
    # Test deep copy is returned with dimnames are got
    dmns_out <- test_FLQuant_get_dimnames2(flq)
    expect_that(dimnames(dmns_out[[1]]), is_identical_to(dimnames(flq)))
    expect_that(dmns_out[[2]][[1]], is_identical_to("all"))
    expect_that(test_FLQuant_get_size(flq), is_identical_to(length(c(flq@.Data))))
    expect_that(test_FLQuant_get_nquant(flq), is_identical_to(dim(flq)[1]))
    expect_that(test_FLQuant_get_nyear(flq), is_identical_to(dim(flq)[2]))
    expect_that(test_FLQuant_get_nunit(flq), is_identical_to(dim(flq)[3]))
    expect_that(test_FLQuant_get_nseason(flq), is_identical_to(dim(flq)[4]))
    expect_that(test_FLQuant_get_narea(flq), is_identical_to(dim(flq)[5]))
    expect_that(test_FLQuant_get_niter(flq), is_identical_to(dim(flq)[6]))
    indices <- round(runif(6,min=1, max = dim(flq)))
    value <- rnorm(1)
    element <- test_FLQuant_get_data_element(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(c(flq)[element+1]))
    flq <- random_FLQuant_generator()
    indices <- round(runif(6,min=1, max = dim(flq)))
    out <- test_FLQuant_get_all_iters(flq, indices[1], indices[2], indices[3], indices[4], indices[5])
    expect_that(out, is_identical_to(flq[indices[1], indices[2], indices[3], indices[4], indices[5],]))

})

test_that("set",{
    #set_data
    flq1 <- random_FLQuant_generator()
    flq2 <- flq1
    flq2[] <- rnorm(prod(dim(flq2)))
    flq_out <- test_FLQuant_set_data(flq1, flq2@.Data)
    expect_that(flq2, is_identical_to(flq_out))
    # set_dimnames
    flq_in <- random_FLQuant_generator()
    new_dimnames <- dimnames(flq_in)
    new_dimnames[[1]][1] <- as.character(rnorm(1))
    flq_out <- test_FLQuant_set_dimnames(flq_in, new_dimnames)
    expect_that(dimnames(flq_out)[[1]][1], is_identical_to(new_dimnames[[1]][1]))
    expect_that(c(flq_out@.Data), is_identical_to(c(flq_in@.Data)))
    expect_that(dim(flq_out), is_identical_to(dim(flq_in)))
    # Check falure
    new_dimnames[[3]] <- c(new_dimnames[[3]],"extra")
    expect_that(test_FLQuant_set_dimnames(flq_in, new_dimnames), throws_error())
    # set units
    flq_in <- random_FLQuant_generator()
    new_units <- as.character(rnorm(1))
    flq_out <- test_FLQuant_set_units(flq_in, new_units)
    expect_that(units(flq_out), is_identical_to(new_units))


})

test_that("FLQuant get and set data accessors", {
    flq <- random_FLQuant_generator()
    indices <- round(runif(6,min=1, max = dim(flq)))
    index <- round(runif(1,min=1,max = prod(dim(flq))))
    value <- rnorm(1)
    # single index
    expect_that(test_FLQuant_get_const_single_index_accessor(flq, index), is_identical_to(c(flq@.Data)[index]))
    expect_that(test_FLQuant_get_single_index_accessor(flq, index), is_identical_to(c(flq@.Data)[index]))
    flq_out <- test_FLQuant_set_single_index_accessor(flq, index, value)
    expect_that(c(flq_out)[index], is_identical_to(value))
    # multiple indices
    value_out <- test_FLQuant_const_get_accessor(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value_out))
    value_out <- test_FLQuant_get_accessor(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value_out))
    flq_out <- test_FLQuant_set_accessor(flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], value)
    expect_that(c(flq_out[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
    # Check outside bounds
    big_index <- prod(dim(flq))+1
    big_indices1 <- dim(flq) + c(1,0,0,0,0,0)
    big_indices2 <- dim(flq) + c(0,1,0,0,0,0)
    big_indices3 <- dim(flq) + c(0,0,1,0,0,0)
    big_indices4 <- dim(flq) + c(0,0,0,1,0,0)
    big_indices5 <- dim(flq) + c(0,0,0,0,1,0)
    big_indices6 <- dim(flq) + c(0,0,0,0,0,1)
    expect_that(test_FLQuant_get_const_single_index_accessor(flq, big_index), throws_error())
    expect_that(test_FLQuant_get_single_index_accessor(flq, big_index), throws_error())
    expect_that(test_FLQuant_set_single_index_accessor(flq, big_index, value), throws_error())
    expect_that(test_FLQuant_const_get_accessor(flq, big_indices1[1], big_indices1[2], big_indices1[3], big_indices1[4], big_indices1[5], big_indices1[6]), throws_error())
    expect_that(test_FLQuant_const_get_accessor(flq, big_indices2[1], big_indices2[2], big_indices2[3], big_indices2[4], big_indices2[5], big_indices2[6]), throws_error())
    expect_that(test_FLQuant_const_get_accessor(flq, big_indices3[1], big_indices3[2], big_indices3[3], big_indices3[4], big_indices3[5], big_indices3[6]), throws_error())
    expect_that(test_FLQuant_const_get_accessor(flq, big_indices4[1], big_indices4[2], big_indices4[3], big_indices4[4], big_indices4[5], big_indices4[6]), throws_error())
    expect_that(test_FLQuant_const_get_accessor(flq, big_indices5[1], big_indices5[2], big_indices5[3], big_indices5[4], big_indices5[5], big_indices5[6]), throws_error())
    expect_that(test_FLQuant_const_get_accessor(flq, big_indices6[1], big_indices6[2], big_indices6[3], big_indices6[4], big_indices6[5], big_indices6[6]), throws_error())
    expect_that(test_FLQuant_get_accessor(flq, big_indices1[1], big_indices1[2], big_indices1[3], big_indices1[4], big_indices1[5], big_indices1[6]), throws_error())
    expect_that(test_FLQuant_get_accessor(flq, big_indices2[1], big_indices2[2], big_indices2[3], big_indices2[4], big_indices2[5], big_indices2[6]), throws_error())
    expect_that(test_FLQuant_get_accessor(flq, big_indices3[1], big_indices3[2], big_indices3[3], big_indices3[4], big_indices3[5], big_indices3[6]), throws_error())
    expect_that(test_FLQuant_get_accessor(flq, big_indices4[1], big_indices4[2], big_indices4[3], big_indices4[4], big_indices4[5], big_indices4[6]), throws_error())
    expect_that(test_FLQuant_get_accessor(flq, big_indices5[1], big_indices5[2], big_indices5[3], big_indices5[4], big_indices5[5], big_indices5[6]), throws_error())
    expect_that(test_FLQuant_get_accessor(flq, big_indices6[1], big_indices6[2], big_indices6[3], big_indices6[4], big_indices6[5], big_indices6[6]), throws_error())
    expect_that(test_FLQuant_set_accessor(flq, big_indices1[1], big_indices1[2], big_indices1[3], big_indices1[4], big_indices1[5], big_indices1[6], value), throws_error())
    expect_that(test_FLQuant_set_accessor(flq, big_indices2[1], big_indices2[2], big_indices2[3], big_indices2[4], big_indices2[5], big_indices2[6], value), throws_error())
    expect_that(test_FLQuant_set_accessor(flq, big_indices3[1], big_indices3[2], big_indices3[3], big_indices3[4], big_indices3[5], big_indices3[6], value), throws_error())
    expect_that(test_FLQuant_set_accessor(flq, big_indices4[1], big_indices4[2], big_indices4[3], big_indices4[4], big_indices4[5], big_indices4[6], value), throws_error())
    expect_that(test_FLQuant_set_accessor(flq, big_indices5[1], big_indices5[2], big_indices5[3], big_indices5[4], big_indices5[5], big_indices5[6], value), throws_error())
    expect_that(test_FLQuant_set_accessor(flq, big_indices6[1], big_indices6[2], big_indices6[3], big_indices6[4], big_indices6[5], big_indices6[6], value), throws_error())
    # indices accessor
    expect_that(test_FLQuant_get_const_indices_accessor(flq, indices), is_identical_to(c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    expect_that(test_FLQuant_get_const_indices_accessor(flq, c(indices,1)), throws_error())
    expect_that(test_FLQuant_get_const_indices_accessor(flq, indices[-1]), throws_error())
    expect_that(test_FLQuant_get_indices_accessor(flq, indices), is_identical_to(c(flq[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]])))
    expect_that(test_FLQuant_get_indices_accessor(flq, c(indices,1)), throws_error())
    expect_that(test_FLQuant_get_indices_accessor(flq, indices[-1]), throws_error())
    value <- rnorm(1)
    flq_out <- test_FLQuant_set_indices_accessor(flq, indices, value)
    expect_that(c(flq_out[indices[1], indices[2], indices[3], indices[4], indices[5], indices[6]]), is_identical_to(value))
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
    expect_that(test_FLQuant_FLQuant_match_dims(flq, flq), is_identical_to(1L))
    expect_that(test_FLQuant_FLQuant_match_dims(flq, flq1), is_identical_to(-1L))
    expect_that(test_FLQuant_FLQuant_match_dims(flq, flq2), is_identical_to(-2L))
    expect_that(test_FLQuant_FLQuant_match_dims(flq, flq3), is_identical_to(-3L))
    expect_that(test_FLQuant_FLQuant_match_dims(flq, flq4), is_identical_to(-4L))
    expect_that(test_FLQuant_FLQuant_match_dims(flq, flq5), is_identical_to(-5L))
    expect_that(test_FLQuant_FLQuant_match_dims(flq, flq6), is_identical_to(-6L))
})

test_that("FLQuant subsetter works",{
    fixed_dims <- round(runif(6, min=5, max = 10))
    flq <- random_FLQuant_generator(fixed_dims = fixed_dims)
    sub_dims_start <- round(runif(6, min=1, max = 2))
    sub_dims_end <- fixed_dims - round(runif(6, min=1, max = 2))
    sub_flq_out <-  test_FLQuant_subset(flq, sub_dims_start[1], sub_dims_end[1], sub_dims_start[2], sub_dims_end[2], sub_dims_start[3], sub_dims_end[3], sub_dims_start[4], sub_dims_end[4], sub_dims_start[5], sub_dims_end[5], sub_dims_start[6], sub_dims_end[6])
    sub_flq_in <- flq[sub_dims_start[1]:sub_dims_end[1], sub_dims_start[2]:sub_dims_end[2], sub_dims_start[3]:sub_dims_end[3], sub_dims_start[4]:sub_dims_end[4], sub_dims_start[5]:sub_dims_end[5], sub_dims_start[6]:sub_dims_end[6]]
    expect_that(c(sub_flq_in@.Data), is_identical_to(c(sub_flq_out@.Data)))
    expect_that(sub_flq_in@.Data, is_identical_to(sub_flq_out@.Data))
    # min < max check
    sub_dims_wrong <- sub_dims_end
    expect_that(test_FLQuant_subset(flq, sub_dims_wrong[1], sub_dims_start[1], sub_dims_wrong[2], sub_dims_start[2], sub_dims_wrong[3], sub_dims_start[3], sub_dims_wrong[4], sub_dims_start[4], sub_dims_wrong[5], sub_dims_start[5], sub_dims_wrong[6], sub_dims_start[6]), throws_error())

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
    expect_that(c(single_iter_flq[indices[1], indices[2], indices[3], indices[4], indices[5], 1]), is_identical_to(value_out))
    # Accessing more iters than you can
    value_out2 <- test_FLQuant_const_get_accessor(single_iter_flq, indices[1], indices[2], indices[3], indices[4], indices[5], indices[6])
    expect_that(c(single_iter_flq[indices[1], indices[2], indices[3], indices[4], indices[5], 1]), is_identical_to(value_out))
    expect_that(test_FLQuant_const_get_accessor(flq, indices[1], indices[2], indices[3], indices[4], indices[5], dim(flq)[6]+1), throws_error())
})

test_that("Propagating FLQuant iters", {
    flq <- random_FLQuant_generator(fixed_dim=c(NA,NA,NA,NA,NA,1))
    niters <- round(runif(1, min = 5, max = 10))
    flq_out <- test_FLQuant_propagate_iters(flq, niters)
    flq_prop <- propagate(flq,niters)
    # Check original is unchanged
    expect_that(flq, is_identical_to(flq_out[["flq"]]))
    expect_that(flq_prop, is_identical_to(flq_out[["flq2"]]))
})

test_that("FLPar_to_FLQuant", {
    # Need to check dimnames of FLPar before dispatch that dimnames are only FLQuant dimnames
    # Make some random FLPars
    flq <- random_FLQuant_generator()
    flp2D <- FLPar(rnorm(dim(flq)[1] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,6)])
    flp3D1 <- FLPar(rnorm(dim(flq)[1] * dim(flq)[2] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,2,6)])
    flp3D2 <- FLPar(rnorm(dim(flq)[1] * dim(flq)[4] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,4,6)])
    flp4D <- FLPar(rnorm(dim(flq)[1] * dim(flq)[2] * dim(flq)[4] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,2,4,6)])
    flp5D1 <- FLPar(rnorm(dim(flq)[1] * dim(flq)[2] * dim(flq)[3] * dim(flq)[4] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,2,3,4,6)])
    flp5D2 <- FLPar(rnorm(dim(flq)[1] * dim(flq)[2] * dim(flq)[4] * dim(flq)[5] * dim(flq)[6]), dimnames=dimnames(flq)[c(1,2,4,5,6)])
    flp6D <- FLPar(rnorm(prod(dim(flq))), dimnames=dimnames(flq))
    flp7D <- FLPar(rnorm(dim(flq)[1] * dim(flq)[2] * dim(flq)[4] * dim(flq)[5] * 3 * dim(flq)[6]), dimnames=c(dimnames(flq)[1:5], other=list(c("a","b","c")), dimnames(flq)[6]))

    # Extra dim which will be ignored - no it's not
    # flp2D2 <- FLPar(rnorm(dim(flq)[1] * dim(flq)[6] * 5), dimnames=list(params = dimnames(flq)[[1]], other_name = c("a","b","c","d","e"), iter = dimnames(flq)[[6]]))
    # flq_out <- test_FLPar_to_FLQuant(flp2D2)

    # 2D
    flq_out <- test_FLPar_to_FLQuant(flp2D)
    expect_that(dim(flq_out), equals(unname(c(dim(flp2D)[1],1,1,1,1,dim(flp2D)[2]))))
    expect_that(c(flq_out), equals(c(flp2D)))
    # 3D
    flq_out <- test_FLPar_to_FLQuant(flp3D1)
    expect_that(dim(flq_out), equals(unname(c(dim(flp3D1)[1],dim(flp3D1)[2],1,1,1,dim(flp3D1)[3]))))
    expect_that(c(flq_out), equals(c(flp3D1)))
    flq_out <- test_FLPar_to_FLQuant(flp3D2)
    expect_that(dim(flq_out), equals(unname(c(dim(flp3D2)[1],1,1,dim(flp3D2)[2],1,dim(flp3D2)[3]))))
    expect_that(c(flq_out), equals(c(flp3D2)))
    # 4D
    flq_out <- test_FLPar_to_FLQuant(flp4D)
    expect_that(dim(flq_out), equals(unname(c(dim(flp4D)[1],dim(flp4D)[2],1,dim(flp4D)[3],1,dim(flp4D)[4]))))
    expect_that(c(flq_out), equals(c(flp4D)))
    # 5D
    flq_out <- test_FLPar_to_FLQuant(flp5D1)
    expect_that(dim(flq_out), equals(unname(c(dim(flp5D1)[1],dim(flp5D1)[2],dim(flp5D1)[3],dim(flp5D1)[4],1,dim(flp5D1)[5]))))
    expect_that(c(flq_out), equals(c(flp5D1)))
    flq_out <- test_FLPar_to_FLQuant(flp5D2)
    expect_that(dim(flq_out), equals(unname(c(dim(flp5D2)[1],dim(flp5D2)[2],1,dim(flp5D2)[3],dim(flp5D2)[4],dim(flp5D2)[5]))))
    expect_that(c(flq_out), equals(c(flp5D2)))
    # 6D
    flq_out <- test_FLPar_to_FLQuant(flp6D)
    expect_that(dim(flq_out), equals(unname(c(dim(flp6D)))))
    expect_that(c(flq_out), equals(c(flp6D)))
    # > 7D (fail)
    expect_that(test_FLPar_to_FLQuant(flp7D), throws_error())
})
