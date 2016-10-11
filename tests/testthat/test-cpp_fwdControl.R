# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Implementation of fwdControl")
source("expect_funs.R")

test_that("fwdControl as and wrap",{
    fc_in <- random_fwdControl_generator(years = 1:10, niters = 10)
    fc_out <- test_as_wrap_fwdControl(fc_in)
    expect_fwdControl_equal(fc_in, fc_out)
})

test_that("fwdControl copy constructor and assignment operator", {
    # No need to test if values change after copy as we cannot change the values of fwdControl
    fc_in <- random_fwdControl_generator(years = 1:10, niters = 10)
    # Copy constructor
    fcs <- test_fwdControl_copy_constructor(fc_in)
    # Assignment
    fc_out <- test_fwdControl_assignment_operator(fc_in)
    expect_fwdControl_equal(fc_in, fcs[["fc1"]])
    expect_fwdControl_equal(fc_in, fcs[["fc2"]])
    expect_fwdControl_equal(fc_in, fc_out)
})

test_that("fwdControl accessors", {
    fc <- random_fwdControl_generator()
    # But no order column in the fwdControl constructor - it is added by fwd().
    # So we add one here for testing.
    # Make it random so that get_target_row is properly tested
    fc@target$order <- sample(1:nrow(fc@target), nrow(fc@target))

    # Get target - just the data.frame
    target <- test_fwdControl_get_target(fc)
    expect_identical(target, fc@target)
    # get ntarget
    # If no order column - fail



    fc2 <- fc
    fc2@target <- fc2@target[,colnames(fc2@target) != "order"]
    expect_error(test_fwdControl_get_ntarget(fc2))
    # With order column
    expect_equal(test_fwdControl_get_ntarget(fc), max(fc@target$order) - min(fc@target$order) + 1)
    # get niter
    niter <- test_fwdControl_get_niter(fc)
    expect_identical(niter, dim(fc@iters)[3])
    # Pull out random target for testing - would be good to pull out a target with sim targets
    target_no <- fc@target$order[round(runif(1, min=1, max=length(fc@target$order)))]
    # nsim_target
    expect_equal(sum(fc@target$order == target_no), test_fwdControl_get_nsim_target(fc, target_no))
    expect_error(test_fwdControl_get_nsim_target(fc, max(fc@target$order)+1)) # Out of bounds
    # get target row and rows
    nsim_target <- sum(fc@target$order == target_no)
    sim_target_no <- round(runif(1, min=1, max=nsim_target))
    row_no <- which(fc@target$order==target_no)[sim_target_no]
    expect_equal(row_no, test_fwdControl_get_target_row(fc, target_no, sim_target_no) + 1) # +1 as start at 0
    row_nos <- which(fc@target$order==target_no)
    expect_equal(row_nos, test_fwdControl_get_target_rows(fc, target_no) + 1)
    # get target value
    col_no <- round(runif(1,min=1,max=3))
    # all sim targets
    values <- test_fwdControl_get_target_value(fc, target_no, col_no)
    target_rows <- which(fc@target$order==target_no)
    expect_equal(c(t(fc@iters[target_rows,col_no,])), values)
    # a single sim target
    values <- test_fwdControl_get_target_value2(fc, target_no, sim_target_no, col_no)
    expect_equal(unname(fc@iters[target_rows[sim_target_no],col_no,]), values)
    # int_col - attempt a column that isn't there
    expect_error(test_fwdControl_get_target_int_col(fc, target_no, "balls")) # column name not in control
    # year and season
    expect_equal(fc@target$year[target_rows], test_fwdControl_get_target_int_col(fc, target_no, "year"))
    expect_equal(fc@target$season[target_rows], test_fwdControl_get_target_int_col(fc, target_no, "season"))
    # relYear
    fc@target$relYear <- as.integer(NA) # test NA works
    expect_equal(fc@target$relYear[target_rows], as.integer(test_fwdControl_get_target_int_col(fc, target_no, "relYear")))
    fc@target$relYear <- 1990:(1990+length(fc@target$relYear)-1)
    expect_equal(fc@target$relYear[target_rows], test_fwdControl_get_target_int_col(fc, target_no, "relYear"))
    # relSeason
    fc@target$relSeason <- as.integer(NA)
    expect_equal(fc@target$relSeason[target_rows], as.integer(test_fwdControl_get_target_int_col(fc, target_no, "relSeason")))
    fc@target$relSeason <- 1:(1+length(fc@target$relSeason)-1)
    expect_equal(fc@target$relSeason[target_rows], test_fwdControl_get_target_int_col(fc, target_no, "relSeason"))
    # fishery, catch, biol - just pull out 1 value - Need to force to be int in case it's an NA - throws warnings - bit annoying
    expect_equal(fc@target$fishery[row_no] , as.integer(test_fwdControl_get_target_int_col2(fc, target_no, sim_target_no, "fishery"))) 
    expect_equal(fc@target$catch[row_no] , as.integer(test_fwdControl_get_target_int_col2(fc, target_no, sim_target_no, "catch"))) 
    expect_equal(fc@target$biol[row_no] , as.integer(test_fwdControl_get_target_int_col2(fc, target_no, sim_target_no, "biol"))) 
    # Do these work with NA?
    fc@target$fishery[1] <- as.integer(NA)
    if (any(is.na(fc@target$fishery))){
        na_row <- which(is.na(fc@target$fishery))[1]
        na_target_no <- fc@target$order[na_row]
        na_sim_target_no <- which(which(fc@target$order == na_target_no) == na_row)
        expect_true(is.na(as.integer(test_fwdControl_get_target_int_col2(fc, na_target_no, na_sim_target_no, "fishery"))))
    }
    # get target type / quantity
    type <- test_fwdControl_get_target_quantity(fc, target_no, sim_target_no)
    expect_identical(type, as.character(fc@target[target_rows[sim_target_no], "quant"]))
    expect_error(test_fwdControl_get_target_quantity(fc, max(fc@target$order)+1, sim_target_no)) # target number too high
    # age range    
    age_range <- test_fwdControl_get_age_range(fc, target_no, sim_target_no)
    expect_equal(unname(unlist( fc@target[row_no,c("minAge", "maxAge")])), age_range)
})

test_that("fwdControl get_FCB methods", {
    fwc <- random_fwdControl_generator()
    # Get FC
    biol_no <- sample(unique(fwc@FCB[,"B"]),1)
    FC_out <- test_fwdControl_get_FC(fwc, biol_no)
    FC_in <- fwc@FCB[fwc@FCB[,"B"] == biol_no,c("F","C"), drop=FALSE]
    expect_equal(unname(FC_in), FC_out)
    # What if biol not found in FCB? - empty array - no rows
    biol_no <- max(fwc@FCB[,"B"])+1
    FC_out <- test_fwdControl_get_FC(fwc, biol_no)
    expect_equal(nrow(FC_out), 0)
    # Get B
    row_no <- sample(nrow(fwc@FCB),1)
    FC <- fwc@FCB[row_no,c("F","C")]
    B_out <- test_fwdControl_get_B(fwc, fwc@FCB[row_no,"F"], fwc@FCB[row_no,"C"])
    B_in <- fwc@FCB[(fwc@FCB[,"F"] == FC["F"]) & (fwc@FCB[,"C"] == FC["C"]),"B"]
    expect_equal(unname(B_in), unname(B_out))
    # Get nrow
    nrow_out <- test_fwdControl_get_FCB_nrow(fwc)
    expect_equal(nrow_out, dim(fwc@FCB)[1])
    # Get row no
    fcb <- fwc@FCB
    # Pick a row
    row_in <- round(runif(1, min=1, max=nrow(fcb)))
    row_out <- test_fwdControl_get_FCB_row_no(fwc, fcb[row_in,"F"], fcb[row_in,"C"], fcb[row_in,"B"])
    expect_equal(row_in, row_out + 1)
    # first row
    row_in <- 1
    row_out <- test_fwdControl_get_FCB_row_no(fwc, fcb[row_in,"F"], fcb[row_in,"C"], fcb[row_in,"B"])
    expect_equal(row_in, row_out + 1)
    # last row
    row_in <- nrow(fcb)
    row_out <- test_fwdControl_get_FCB_row_no(fwc, fcb[row_in,"F"], fcb[row_in,"C"], fcb[row_in,"B"])
    expect_equal(row_in, row_out + 1)
    # Not found
    expect_error(test_fwdControl_get_FCB_row_no(fwc, 10, fcb[row_in,"C"], fcb[row_in,"B"]))
})
