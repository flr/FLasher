
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

# SRP - disaggrated - not summed over unit
test_that("operatingModel disaggregated srp methods",{
    # Assumes that f_prop_spwn and f methods are working correctly
    # Calculated as SSB: N*mat*wt*exp(-Fprespwn - m*spwn) summed over age dimension
    # Make a complicated OM with seasons and units
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    # Recruitment in seasons 1 and 3, so spawning in seasons 4 and 2
    om <- make_test_operatingModel(ple4, FCB, nseasons = 4, recruitment_seasons = c(1,3), recruitment_age = 1, niters = 20, sd = 0.1)
    # Add unfished biol
    om[["biols"]][[5]] <- om[["biols"]][[1]]
    flbs_in <- lapply(om$biols, function(x) return(x[["biol"]]))

    # Biol 1 - fished by 1 fishery only
    biol_no <- 1
    fishery_no <- 1
    catch_no <- 1
    dim <- dim(flbs_in[[biol_no]]@n)
    dim[1] <- 1
    # Get full range from input data
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    prop_in <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, biol_no, f_indices_min[-1], f_indices_max[-1])
    f_in <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, f_indices_min, f_indices_max)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-(f_in %*% prop_in) - (flbs_in[[biol_no]]@m %*% flbs_in[[biol_no]]@spwn[1,])))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(srp_in, srp_out)
    # Subset the full range
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)

    # Biol 2 - fished by two fisheries
    biol_no <- 2
    fishery_no <- 1
    catch_no <- 2
    dim <- dim(flbs_in[[biol_no]]@n)
    dim[1] <- 1
    # Full range
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    prop_in1 <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, biol_no, f_indices_min[-1], f_indices_max[-1])
    f_in1 <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, biol_no, f_indices_min, f_indices_max)
    prop_in2 <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, biol_no, f_indices_min[-1], f_indices_max[-1])
    f_in2 <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 1, biol_no, f_indices_min, f_indices_max)
    f_prop <- (f_in1 %*% prop_in1) + (f_in2 %*% prop_in2)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-f_prop - (flbs_in[[biol_no]]@m %*% flbs_in[[biol_no]]@spwn[1,])))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(srp_in, srp_out)
    # Subset the full range
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)

    # Biol 3
    biol_no <- 3
    fishery_no <- 2
    catch_no <- 2
    dim <- dim(flbs_in[[biol_no]]@n)
    dim[1] <- 1
    # Full range
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    prop_in <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, biol_no, f_indices_min[-1], f_indices_max[-1])
    f_in <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, f_indices_min, f_indices_max)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-(f_in %*% prop_in) - (flbs_in[[biol_no]]@m %*% flbs_in[[biol_no]]@spwn[1,])))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(srp_in, srp_out)
    # Subset the full range
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
    # Biol 4
    biol_no <- 4
    fishery_no <- 2
    catch_no <- 2
    dim <- dim(flbs_in[[biol_no]]@n)
    dim[1] <- 1
    # Full range
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    prop_in <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, biol_no, f_indices_min[-1], f_indices_max[-1])
    f_in <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, f_indices_min, f_indices_max)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-(f_in %*% prop_in) - (flbs_in[[biol_no]]@m %*% flbs_in[[biol_no]]@spwn[1,])))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(srp_in, srp_out)
    # Subset the full range
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
    # Biol 5 - no fishing
    biol_no <- 5
    dim <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    dim[1] <- 1
    # Full range
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-sweep(flbs_in[[biol_no]]@m, 2:6, flbs_in[[biol_no]]@spwn[1,], "*")))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(srp_in, srp_out)
    # Subset the full range
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
})


# Recruitment calculation
