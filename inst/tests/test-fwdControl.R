context("Implementation of fwdControl")

test_that("fwdControl as and wrap",{
    fc_in <- dummy_fwdControl_generator(years = 1:10, niters = 10)
    fc_out <- test_as_wrap_fwdControl(fc_in)
    attr(fc_in@target,"FCB") <- NULL
    expect_that(fc_in@target, is_identical_to(fc_out@target)) # Tests FCB attr too
})

test_that("fwdControl copy constructor and assignement operator", {
    # No need to test if values change after copy as we cannot change the values of fwdControl
    fc_in <- dummy_fwdControl_generator(years = 1:10, niters = 10)
    # Copy constructor
    fcs <- test_fwdControl_copy_constructor(fc_in)
    # Assignment
    fc_out <- test_fwdControl_assignment_operator(fc_in)
    attr(fc_in@target,"FCB") <- NULL
    expect_that(fc_in, is_identical_to(fcs[["fc1"]]))
    expect_that(fc_in, is_identical_to(fcs[["fc2"]]))
    expect_that(fc_in, is_identical_to(fc_out))
})


test_that("fwdControl accessors", {
    fc <- dummy_fwdControl_generator()
    fc@target@element$minAge <- as.integer(round(runif(dim(fc@target@element)[1], min=1, max = 10)))
    fc@target@element$maxAge <- as.integer(fc@target@element$minAge * 2)
    # fill up min and max too - just for the accessor checks
    fc@target@iters[,"min",] <- rnorm(prod(dim(fc@target@iters)[c(1,3)]))
    fc@target@iters[,"max",] <- rnorm(prod(dim(fc@target@iters)[c(1,3)]))

    # get target
    target <- test_fwdControl_get_target(fc)
    expect_that(target, is_identical_to(fc@target@element))

    # Tests for the timestep column
    # No timestep column
    expect_that(test_fwdControl_get_ntimestep(fc), throws_error())
    # Add the timestep column 
    fc@target@element$timestep <- fc@target@element$year + 2
    expect_that(max(fc@target@element$timestep) - min(fc@target@element$timestep) + 1, equals(test_fwdControl_get_ntimestep(fc)))

    # get ntarget
    # No target column
    expect_that(test_fwdControl_get_ntarget(fc), throws_error())
    # Add the target column 
    fc@target@element$target <- rep(1:ceiling(nrow(fc@target@element)/2), each = 2)[1:nrow(fc@target@element)]
    expect_that(test_fwdControl_get_ntarget(fc), equals(max(fc@target@element$target) - min(fc@target@element$target) + 1))

    # get niter
    niter <- test_fwdControl_get_niter(fc)
    expect_that(niter, is_identical_to(dim(fc@target@iters)[3]))

    target_no <- fc@target@element$target[round(runif(1, min=1, max=length(fc@target@element$target)))]
    # nsim_target
    expect_that(sum(fc@target@element$target == target_no), equals(test_fwdControl_get_nsim_target(fc, target_no)))

    nsim_target <- sum(fc@target@element$target == target_no)
    sim_target_no <- round(runif(1, min=1, max=nsim_target))
    row_no <- which(fc@target@element$target==target_no)[sim_target_no]
    expect_that(row_no-1, equals(test_fwdControl_get_target_row(fc, target_no, sim_target_no)))
    #test_fwdControl_get_target_row(fc, target_no, sim_target_no)

    # get target value
    col_no <- round(runif(1,min=1,max=3))
    values <- test_fwdControl_get_target_value(fc, target_no, col_no)
    target_rows <- which(fc@target@element$target==target_no)
    expect_that(c(t(fc@target@iters[target_rows,col_no,])), equals(values))

    ## get year, season, fishery of target
    #target_no <- round(runif(1, min=1, max=nrow(fc@target@element)))
    #year <- test_fwdControl_get_target_year(fc, target_no)
    #expect_that(fc@target@element[target_no, "year"], is_identical_to(year))
    #season <- test_fwdControl_get_target_season(fc, target_no)
    #expect_that(fc@target@element[target_no, "season"], is_identical_to(season))
    #fc@target@element[target_no,"fishery"] <- as.integer(round(runif(1)))
    ## get rel_year and rel_season of target
    ## Test with NA first then some real values
    #target_no <- round(runif(1, min=1, max=nrow(fc@target@element)))
    #fc@target@element$relYear <- as.integer(NA)
    #fc@target@element$relSeason <- as.integer(NA)
    #rel_year <- test_fwdControl_get_target_rel_year(fc, target_no)
    #rel_season <- test_fwdControl_get_target_rel_season(fc, target_no)
    #expect_that(rel_year, is_identical_to(fc@target@element[target_no, "relYear"]))
    #expect_that(rel_season, is_identical_to(fc@target@element[target_no, "relSeason"]))
    #fc@target@element$relYear <- fc@target@element[target_no, "year"]
    #fc@target@element$relSeason <- fc@target@element[target_no, "season"]
    #rel_year <- test_fwdControl_get_target_rel_year(fc, target_no)
    #rel_season <- test_fwdControl_get_target_rel_season(fc, target_no)
    #expect_that(rel_year, is_identical_to(fc@target@element[target_no, "relYear"]))
    #expect_that(rel_season, is_identical_to(fc@target@element[target_no, "relSeason"]))
    ## force fishery column to be integer
    #fishery <- test_fwdControl_get_target_fishery(fc, target_no)
    #expect_that(fc@target@element[target_no, "fishery"], is_identical_to(fishery))
    ## get target type / quantity
    #type <- test_fwdControl_get_target_quantity(fc, target_no)
    #expect_that(type, is_identical_to(as.character(fc@target@element[target_no, "quantity"])))
    #expect_that(test_fwdControl_get_target_quantity(fc, nrow(fc@target@element)+1), throws_error())
    ## age range    
    #age_range <- test_fwdControl_get_age_range(fc, target_no)
    #expect_that(unname(unlist(fc@target@element[target_no,c("minAge", "maxAge")])), is_identical_to(age_range))

})

test_that("fwdControl get_FCB methods", {
    fwc <- dummy_fwdControl_generator()
    # Make a temporary FCB attribute - add to class later
    #FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    #colnames(FCB) <- c("F","C","B")
    #attr(fwc@target, "FCB") <- FCB

    # Get FC
    biol_no <- sample(unique(fwc@target@FCB[,"B"]),1)
    FC_out <- test_fwdControl_get_FC(fwc, biol_no)
    FC_in <- fwc@target@FCB[fwc@target@FCB[,"B"] == biol_no,c("F","C")]
    expect_that(unname(FC_in), equals(c(FC_out)))
    # What if biol not found in FCB?
    biol_no <- max(fwc@target@FCB[,"B"])+1
    FC_out <- test_fwdControl_get_FC(fwc, biol_no)
    # empty

    # Get B
    row_no <- sample(nrow(fwc@target@FCB),1)
    FC <- fwc@target@FCB[row_no,c("F","C")]
    B_out <- test_fwdControl_get_B(fwc, fwc@target@FCB[row_no,"F"], fwc@target@FCB[row_no,"C"])
    B_in <- fwc@target@FCB[(fwc@target@FCB[,"F"] == FC["F"]) & (fwc@target@FCB[,"C"] == FC["C"]),"B"]
    expect_that(unname(B_in), equals(unname(B_out)))
})


