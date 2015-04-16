context("Implementation of fwdControl")

test_that("fwdControl as and wrap",{
    fc_in <- random_fwdControl_generator(years = 1:10, niters = 10)
    fc_out <- test_as_wrap_fwdControl(fc_in)
    attr(fc_in@target,"FCB") <- NULL
    expect_that(fc_in@target, is_identical_to(fc_out@target)) # Tests FCB attr too
})

test_that("fwdControl copy constructor and assignment operator", {
    # No need to test if values change after copy as we cannot change the values of fwdControl
    fc_in <- random_fwdControl_generator(years = 1:10, niters = 10)
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
    fc <- random_fwdControl_generator()

    # get target - just the data.frame
    target <- test_fwdControl_get_target(fc)
    expect_that(target, is_identical_to(fc@target@element))

    # Tests for the timestep column
    # If no timestep column - fail
    fc2 <- fc
    fc2@target@element <- fc2@target@element[,colnames(fc2@target@element) != "timestep"]
    expect_that(test_fwdControl_get_ntimestep(fc2), throws_error())
    # With timestep column
    expect_that(max(fc@target@element$timestep) - min(fc@target@element$timestep) + 1, equals(test_fwdControl_get_ntimestep(fc)))

    # get ntarget
    # If no target column - fail
    fc2 <- fc
    fc2@target@element <- fc2@target@element[,colnames(fc2@target@element) != "target"]
    expect_that(test_fwdControl_get_ntarget(fc2), throws_error())
    # With target column
    expect_that(test_fwdControl_get_ntarget(fc), equals(max(fc@target@element$target) - min(fc@target@element$target) + 1))

    # get niter
    niter <- test_fwdControl_get_niter(fc)
    expect_that(niter, is_identical_to(dim(fc@target@iters)[3]))

    # Pull out random target for testing - would be good to pull out a target with sim targets
    target_no <- fc@target@element$target[round(runif(1, min=1, max=length(fc@target@element$target)))]

    # nsim_target
    expect_that(sum(fc@target@element$target == target_no), equals(test_fwdControl_get_nsim_target(fc, target_no)))
    expect_that(test_fwdControl_get_nsim_target(fc, max(fc@target@element$target)+1), throws_error()) # Out of bounds

    # get target row and rows
    nsim_target <- sum(fc@target@element$target == target_no)
    sim_target_no <- round(runif(1, min=1, max=nsim_target))
    row_no <- which(fc@target@element$target==target_no)[sim_target_no]
    expect_that(row_no, equals(test_fwdControl_get_target_row(fc, target_no, sim_target_no) + 1)) # +1 as start at 0
    row_nos <- which(fc@target@element$target==target_no)
    expect_that(row_nos, equals(test_fwdControl_get_target_rows(fc, target_no) + 1))

    # get target value
    col_no <- round(runif(1,min=1,max=3))
    # all sim targets
    values <- test_fwdControl_get_target_value(fc, target_no, col_no)
    target_rows <- which(fc@target@element$target==target_no)
    expect_that(c(t(fc@target@iters[target_rows,col_no,])), equals(values))
    # a single sim target
    values <- test_fwdControl_get_target_value2(fc, target_no, sim_target_no, col_no)
    expect_that(unname(fc@target@iters[target_rows[sim_target_no],col_no,]), equals(values))

    # int_col - attempt a column that isn't there
    expect_that(test_fwdControl_get_target_int_col(fc, target_no, "balls"), throws_error()) # column name not in control

    # year and season
    expect_that(fc@target@element$year[target_rows], equals(test_fwdControl_get_target_int_col(fc, target_no, "year")))
    expect_that(fc@target@element$season[target_rows], equals(test_fwdControl_get_target_int_col(fc, target_no, "season")))

    # relYear
    fc@target@element$relYear <- as.integer(NA)
    expect_that(fc@target@element$relYear[target_rows], equals(as.integer(test_fwdControl_get_target_int_col(fc, target_no, "relYear"))))
    fc@target@element$relYear <- 1990:(1990+length(fc@target@element$relYear)-1)
    expect_that(fc@target@element$relYear[target_rows], equals(test_fwdControl_get_target_int_col(fc, target_no, "relYear")))

    # relSeason
    fc@target@element$relSeason <- as.integer(NA)
    expect_that(fc@target@element$relSeason[target_rows], equals(as.integer(test_fwdControl_get_target_int_col(fc, target_no, "relSeason"))))
    fc@target@element$relSeason <- 1:(1+length(fc@target@element$relSeason)-1)
    expect_that(fc@target@element$relSeason[target_rows], equals(test_fwdControl_get_target_int_col(fc, target_no, "relSeason")))

    # get target type / quantity
    type <- test_fwdControl_get_target_quantity(fc, target_no, sim_target_no)
    expect_that(type, is_identical_to(as.character(fc@target@element[target_rows[sim_target_no], "quantity"])))
    expect_that(test_fwdControl_get_target_quantity(fc, max(fc@target@element$target)+1, sim_target_no), throws_error()) # target number too high

    # fishery, catch, biol - just pull out 1 value - Need to force to be int in case it's an NA
    #as.integer(test_fwdControl_get_target_int_col2(fc, target_no, sim_target_no, "fishery"))
    # Throws warning as we convert to NA
    expect_that(fc@target@element$fishery[row_no] , equals(as.integer(test_fwdControl_get_target_int_col2(fc, target_no, sim_target_no, "fishery")))) 
    expect_that(fc@target@element$catch[row_no] , equals(as.integer(test_fwdControl_get_target_int_col2(fc, target_no, sim_target_no, "catch")))) 
    expect_that(fc@target@element$biol[row_no] , equals(as.integer(test_fwdControl_get_target_int_col2(fc, target_no, sim_target_no, "biol")))) 
    # Do these work with NA?
    if (any(is.na(fc@target@element$fishery))){
        na_row <- which(is.na(fc@target@element$fishery))[1]
        na_target_no <- fc@target@element$target[na_row]
        na_sim_target_no <- which(which(fc@target@element$target == na_target_no) == na_row)
        expect_that(is.na(as.integer(test_fwdControl_get_target_int_col2(fc, na_target_no, na_sim_target_no, "fishery"))), is_true())
    }

    # age range    
    age_range <- test_fwdControl_get_age_range(fc, target_no, sim_target_no)
    expect_that(unname(unlist( fc@target@element[row_no,c("minAge", "maxAge")])), equals(age_range))

    # effort timestep
    timestep_out <- test_fwdControl_get_target_effort_timestep(fc, target_no, sim_target_no)
    timestep_in <- fc@target@element$timestep[row_no]
    qs <- fc@target@element$quantity[row_no]
    if (qs %in% c("biomass", "ssb")){
        timestep_in <- timestep_in - 1
    }
    expect_that(timestep_in, equals(timestep_out))

})

test_that("fwdControl get_FCB methods", {
    fwc <- random_fwdControl_generator()
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


