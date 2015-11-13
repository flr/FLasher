context("Implementation of fwdControl")

test_that("fwdControl as and wrap",{
    fc_in <- random_fwdControl_generator(years = 1:10, niters = 10)
    fc_out <- test_as_wrap_fwdControl(fc_in)
    test_fwdControl_equal(fc_in, fc_out)
})

test_that("fwdControl copy constructor and assignment operator", {
    # No need to test if values change after copy as we cannot change the values of fwdControl
    fc_in <- random_fwdControl_generator(years = 1:10, niters = 10)
    # Copy constructor
    fcs <- test_fwdControl_copy_constructor(fc_in)
    # Assignment
    fc_out <- test_fwdControl_assignment_operator(fc_in)
    test_fwdControl_equal(fc_in, fcs[["fc1"]])
    test_fwdControl_equal(fc_in, fcs[["fc2"]])
    test_fwdControl_equal(fc_in, fc_out)
})

test_that("fwdControl accessors", {
    fc <- random_fwdControl_generator()
    # get target - just the data.frame
    target <- test_fwdControl_get_target(fc)
    expect_identical(target, fc@target@element)
    # get ntarget
    # If no target column - fail
    fc2 <- fc
    fc2@target@element <- fc2@target@element[,colnames(fc2@target@element) != "target"]
    expect_error(test_fwdControl_get_ntarget(fc2))
    # With target column
    expect_equal(test_fwdControl_get_ntarget(fc), max(fc@target@element$target) - min(fc@target@element$target) + 1)
    # get niter
    niter <- test_fwdControl_get_niter(fc)
    expect_identical(niter, dim(fc@target@iters)[3])
    # Pull out random target for testing - would be good to pull out a target with sim targets
    target_no <- fc@target@element$target[round(runif(1, min=1, max=length(fc@target@element$target)))]
    # nsim_target
    expect_equal(sum(fc@target@element$target == target_no), test_fwdControl_get_nsim_target(fc, target_no))
    expect_error(test_fwdControl_get_nsim_target(fc, max(fc@target@element$target)+1)) # Out of bounds
    # get target row and rows
    nsim_target <- sum(fc@target@element$target == target_no)
    sim_target_no <- round(runif(1, min=1, max=nsim_target))
    row_no <- which(fc@target@element$target==target_no)[sim_target_no]
    expect_equal(row_no, test_fwdControl_get_target_row(fc, target_no, sim_target_no) + 1) # +1 as start at 0
    row_nos <- which(fc@target@element$target==target_no)
    expect_equal(row_nos, test_fwdControl_get_target_rows(fc, target_no) + 1)
    # get target value
    col_no <- round(runif(1,min=1,max=3))
    # all sim targets
    values <- test_fwdControl_get_target_value(fc, target_no, col_no)
    target_rows <- which(fc@target@element$target==target_no)
    expect_equal(c(t(fc@target@iters[target_rows,col_no,])), values)
    # a single sim target
    values <- test_fwdControl_get_target_value2(fc, target_no, sim_target_no, col_no)
    expect_equal(unname(fc@target@iters[target_rows[sim_target_no],col_no,]), values)
    # int_col - attempt a column that isn't there
    expect_error(test_fwdControl_get_target_int_col(fc, target_no, "balls")) # column name not in control
    # year and season
    expect_equal(fc@target@element$year[target_rows], test_fwdControl_get_target_int_col(fc, target_no, "year"))
    expect_equal(fc@target@element$season[target_rows], test_fwdControl_get_target_int_col(fc, target_no, "season"))
    # relYear
    fc@target@element$relYear <- as.integer(NA) # test NA works
    expect_equal(fc@target@element$relYear[target_rows], as.integer(test_fwdControl_get_target_int_col(fc, target_no, "relYear")))
    fc@target@element$relYear <- 1990:(1990+length(fc@target@element$relYear)-1)
    expect_equal(fc@target@element$relYear[target_rows], test_fwdControl_get_target_int_col(fc, target_no, "relYear"))
    # relSeason
    fc@target@element$relSeason <- as.integer(NA)
    expect_equal(fc@target@element$relSeason[target_rows], as.integer(test_fwdControl_get_target_int_col(fc, target_no, "relSeason")))
    fc@target@element$relSeason <- 1:(1+length(fc@target@element$relSeason)-1)
    expect_equal(fc@target@element$relSeason[target_rows], test_fwdControl_get_target_int_col(fc, target_no, "relSeason"))
    # fishery, catch, biol - just pull out 1 value - Need to force to be int in case it's an NA - throws warnings - bit annoying
    expect_equal(fc@target@element$fishery[row_no] , as.integer(test_fwdControl_get_target_int_col2(fc, target_no, sim_target_no, "fishery"))) 
    expect_equal(fc@target@element$catch[row_no] , as.integer(test_fwdControl_get_target_int_col2(fc, target_no, sim_target_no, "catch"))) 
    expect_equal(fc@target@element$biol[row_no] , as.integer(test_fwdControl_get_target_int_col2(fc, target_no, sim_target_no, "biol"))) 
    # Do these work with NA?
    if (any(is.na(fc@target@element$fishery))){
        na_row <- which(is.na(fc@target@element$fishery))[1]
        na_target_no <- fc@target@element$target[na_row]
        na_sim_target_no <- which(which(fc@target@element$target == na_target_no) == na_row)
        expect_true(is.na(as.integer(test_fwdControl_get_target_int_col2(fc, na_target_no, na_sim_target_no, "fishery"))))
    }
    # get_target_num_col
    expect_equal(fc@target@element$value[target_rows], test_fwdControl_get_target_num_col(fc, target_no, "value"))
    expect_equal(fc@target@element$value[row_no], as.numeric(test_fwdControl_get_target_num_col2(fc, target_no, sim_target_no, "value")))
    # get target type / quantity
    type <- test_fwdControl_get_target_quantity(fc, target_no, sim_target_no)
    expect_identical(type, as.character(fc@target@element[target_rows[sim_target_no], "quantity"]))
    expect_error(test_fwdControl_get_target_quantity(fc, max(fc@target@element$target)+1, sim_target_no)) # target number too high
    # age range    
    age_range <- test_fwdControl_get_age_range(fc, target_no, sim_target_no)
    expect_equal(unname(unlist( fc@target@element[row_no,c("minAge", "maxAge")])), age_range)
    # effort timestep - need to be careful with this - timing of effort for certain target types
    timestep_out <- test_fwdControl_get_target_effort_timestep(fc, target_no, sim_target_no)
    timestep_in <- fc@target@element$timestep[row_no]
    qs <- fc@target@element$quantity[row_no]
    if (qs %in% c("biomass", "ssb")){
        timestep_in <- timestep_in - 1
    }
    expect_equal(timestep_in, timestep_out)
})

test_that("fwdControl get_FCB methods", {
    fwc <- random_fwdControl_generator()
    # Get FC
    biol_no <- sample(unique(fwc@target@FCB[,"B"]),1)
    FC_out <- test_fwdControl_get_FC(fwc, biol_no)
    FC_in <- fwc@target@FCB[fwc@target@FCB[,"B"] == biol_no,c("F","C")]
    expect_equal(unname(FC_in), c(FC_out))
    # What if biol not found in FCB? - empty array - no rows
    biol_no <- max(fwc@target@FCB[,"B"])+1
    FC_out <- test_fwdControl_get_FC(fwc, biol_no)
    expect_equal(nrow(FC_out), 0)
    # Get B
    row_no <- sample(nrow(fwc@target@FCB),1)
    FC <- fwc@target@FCB[row_no,c("F","C")]
    B_out <- test_fwdControl_get_B(fwc, fwc@target@FCB[row_no,"F"], fwc@target@FCB[row_no,"C"])
    B_in <- fwc@target@FCB[(fwc@target@FCB[,"F"] == FC["F"]) & (fwc@target@FCB[,"C"] == FC["C"]),"B"]
    expect_equal(unname(B_in), unname(B_out))
})
