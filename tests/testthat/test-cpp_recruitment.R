
# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Test recruitment processes model in the operating model")
source("expect_funs.R")

# f_prop_spwn
# Proportion of fishing before spawning
# 186

test_that("operatingModel f_prop_spwn methods",{
    # Test with a seasonal OM with several units
    # 4 seasons, 3 units
    # One Biol fished by a one Fishery
    flq <- random_FLQuant_generator(min_dims=c(5,5,NA,NA,1,5), fixed_dims=c(NA,NA,3,4,1,NA))
    flbs <- random_fwdBiols_list_generator(min_biols = 1, max_biols = 1, fixed_dims = dim(flq))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=1, max_fisheries=1)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Set up spwn and fishery timings
    # Four examples
    # Spwn before fishing u1 s1
    # Spwn after fishing  u2 s2
    # Spwn during fishing u3 s3
    # No spawning
    year <- round(runif(1, min=1, max=dim(flq)[2]))
    flbs[[1]][["biol"]]@spwn[,year,1,1,] <- runif(dim(flq)[6], min= 0, max = c(flfs[[1]]@hperiod["start",year,1,1,])) # spwn before fishing
    flbs[[1]][["biol"]]@spwn[,year,2,2,] <- runif(dim(flq)[6], min= c(flfs[[1]]@hperiod["end",year,1,2,]), max = 1) # spwn after fishing
    flbs[[1]][["biol"]]@spwn[,year,3,3,] <- runif(dim(flq)[6], min= c(flfs[[1]]@hperiod["start",year,1,3,]), max = c(flfs[[1]]@hperiod["end",year,1,3,])) # In between
    flbs[[1]][["biol"]]@spwn[,year,1,3,] <- NA # No spawning
    indices_min <- c(1,year,1,1,1,1)
    indices_max <- c(1,year,3,4,1,dim(flq)[6])
    prop_out <- test_operatingModel_f_prop_spwn_FLQ_subset(flfs, flbs, fc, 1, 1, indices_min[-1], indices_max[-1])
    expect_equal( c(prop_out[,,1,1,]) ,rep(0.0, dim(flq)[6])) # spwn before F
    expect_equal(c(prop_out[,,2,2,]),rep(1.0, dim(flq)[6])) # spwn after F
    prop_in <- (flbs[[1]][["biol"]]@spwn[,year,3,3,] - flfs[[1]]@hperiod["start",year,1,3,]) / (flfs[[1]]@hperiod["end",year,1,3,] - flfs[[1]]@hperiod["start",year,1,3,])
    expect_FLQuant_equal(prop_in, prop_out[,,3,3,]) # spwn during F
    # No spwn
    expect_equal(c(prop_out[,,1,3,]), rep(as.numeric(NA),dim(flq)[6]))
    # Bigger test - less controlled - random
    # Get full FLQ
    indices_min <- rep(1,6)
    indices_max <- dim(flq)
    prop_in <- (spwn(flbs[[1]][["biol"]])[1,] %-% flfs[[1]]@hperiod[1,]) %/% (flfs[[1]]@hperiod[2,] - flfs[[1]]@hperiod[1,])
    # If hperiod1 > spwn, prop = 1
    # If hperiod2 < spwn, prop = 0
    nunit <- dim(flq)[3]
    for (i in 1:nunit){
        prop_in_temp <- prop_in[,,i,]
        prop_in_temp@.Data[flfs[[1]]@hperiod[1,,1] >= spwn(flbs[[1]][["biol"]])[1,,i] ] <- 0
        prop_in_temp@.Data[flfs[[1]]@hperiod[2,,1] <= spwn(flbs[[1]][["biol"]])[1,,i] ] <- 1
        prop_in[,,i,] <- prop_in_temp
    }
    prop_out <- test_operatingModel_f_prop_spwn_FLQ_subset(flfs, flbs, fc, 1, 1, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(prop_in, prop_out) 
    # Subset of f
    indices_max <- round(runif(6,1,dim(prop_in)))
    indices_min <- round(runif(6,1,indices_max))
    prop_out <- test_operatingModel_f_prop_spwn_FLQ_subset(flfs, flbs, fc, 1, 1, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(prop_in[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], prop_out)
    # NA in hperiod - no fishing
    flfs[[1]]@hperiod[1, indices_min[2], indices_min[3], indices_min[4], indices_min[5], indices_min[6]] <- NA
    prop_out <- test_operatingModel_f_prop_spwn_FLQ_subset(flfs, flbs, fc, 1, 1, indices_min[-1], indices_min[-1])
    expect_that(c(prop_out), is_identical_to(0.0))
})


# SRP - unit
# SRP - total

# Recruitment calculation
