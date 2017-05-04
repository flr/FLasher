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
    for (i in 1:length(flfs1)){
        expect_FLFishery_equal(flfs1[[i]], flfs2[[i]])
    }
}

expect_FLBiolcpp_equal <- function(flb1, flb2, ignore_srr = FALSE){
    expect_identical(flb1@n, flb2@n)
    expect_identical(flb1@m, flb2@m)
    expect_identical(flb1@wt, flb2@wt)
    expect_identical(flb1@mat, flb2@mat)
    expect_identical(flb1@fec, flb2@fec)
    expect_identical(flb1@spwn, flb2@spwn)
    expect_identical(flb1@name, flb2@name)
    expect_identical(flb1@desc, flb2@desc)
    expect_identical(flb1@range, flb2@range)
    if (!ignore_srr){
        expect_identical(flb1@srmodel, flb2@srmodel)
        expect_identical(flb1@srparams, flb2@srparams)
    }
}

expect_FLBiolcpps_equal <- function(flbs1, flbs2, ignore_srr = FALSE){
    for (i in 1:length(flbs1)){
        expect_FLBiolcpp_equal(flbs1[[i]], flbs2[[i]], ignore_srr=ignore_srr)
    }
}


