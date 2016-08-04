# Copyright European Union, 2016
# Author: Finlay Scott (EC JRC) <finlay.scott@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# Functions for testing equality of FLR objects using testthat

expect_FLQuant_equal <- function(flq1, flq2){
    expect_identical(dim(flq1), dim(flq2))
    expect_equal(c(flq1), c(flq2))
}

expect_fwdControl_equal <- function(fwc1, fwc2){
    expect_identical(fwc1@target, fwc2@target)
    expect_identical(fwc1@iters, fwc2@iters)
}

expect_FLFishery_equal <- function(flf1, flf2){
    expect_identical(flf1@effort, flf2@effort)
    expect_identical(flf1@vcost, flf2@vcost)
    expect_identical(flf1@fcost, flf2@fcost)
    expect_identical(flf1@hperiod, flf2@hperiod)
    expect_identical(flf1@name, flf2@name)
    expect_identical(flf1@range, flf2@range)
    expect_identical(flf1@.Data, flf2@.Data)
    expect_identical(flf1@desc, flf2@desc)
    expect_identical(flf1@names, flf2@names)
}

expect_FLFisheries_equal <- function(flfs1, flfs2){
    expect_identical(flfs1@desc, flfs2@desc)
    expect_identical(flfs1@names, flfs2@names)
    for (i in 1:length(FLFisheries)){
        expect_FLFishery_equal(flfs1[[i]], flfs2[[i]])
    }
}

