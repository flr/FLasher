
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
    flq <- random_FLQuant_generator(min_dims=c(5,5,NA,NA,NA,5), fixed_dims=c(NA,NA,3,4,1,NA))
    flbs <- random_fwdBiols_list_generator(min_biols = 1, max_biols = 1, fixed_dims = dim(flq))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=1, max_fisheries=1)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Set up spwn and fishery timings
    # Four examples - all units are the same
    # Spwn before fishing s1
    # Spwn after fishing  s2
    # Spwn during fishing s3
    # No spawning
    year <- round(runif(1, min=1, max=dim(flq)[2]))
    flbs[[1]][["biol"]]@spwn[,year,,1,] <- runif(dim(flq)[6] * dim(flq)[3], min= 0, max = rep(c(flfs[[1]]@hperiod["start",year,,1,]),each=dim(flq)[3])) # spwn before fishing
    flbs[[1]][["biol"]]@spwn[,year,,2,] <- runif(dim(flq)[6] * dim(flq)[3], min= rep(c(flfs[[1]]@hperiod["end",year,,2,]), each=dim(flq)[3]), max = 1) # spwn after fishing
    flbs[[1]][["biol"]]@spwn[,year,,3,] <- runif(dim(flq)[6] * dim(flq)[3], min= rep(c(flfs[[1]]@hperiod["start",year,,3,]),each=dim(flq)[3]), max = rep(c(flfs[[1]]@hperiod["end",year,1,3,]),each=dim(flq)[3])) # In between
    flbs[[1]][["biol"]]@spwn[,year,,4,] <- NA # No spawning
    # Full indices
    indices_min <- c(1,year,1,1,1,1)
    indices_max <- c(1,year,dim(flq)[3:6])
    prop_out <- test_operatingModel_f_prop_spwn_FLQ_subset(flfs, flbs, fc, 1, 1, indices_min[-1], indices_max[-1])
    expect_equal(c(prop_out[,,,1,]), rep(0.0, dim(flq)[3] * dim(flq)[6])) # spwn before F
    expect_equal(c(prop_out[,,,2,]),rep(1.0, dim(flq)[3] * dim(flq)[6])) # spwn after F
    prop_in <- (flbs[[1]][["biol"]]@spwn[,year,,3,] %-% flfs[[1]]@hperiod["start",year,,3,]) %/% (flfs[[1]]@hperiod["end",year,,3,] - flfs[[1]]@hperiod["start",year,,3,])
    expect_FLQuant_equal(prop_in, prop_out[,,,3,]) # spwn during F
    # No spwn
    expect_equal(c(prop_out[,,,4,]), rep(as.numeric(NA),dim(flq)[3]*dim(flq)[6]))

    # Bigger test - less controlled - random
    # Get full FLQ
    indices_min <- rep(1,6)
    indices_max <- dim(flq)
    prop_in <- (spwn(flbs[[1]][["biol"]])[1,] %-% flfs[[1]]@hperiod[1,]) %/% (flfs[[1]]@hperiod[2,] - flfs[[1]]@hperiod[1,])
    # If hperiod1 > spwn, prop = 1
    # If hperiod2 < spwn, prop = 0
    nunit <- dim(flq)[3]
    # Go unit by unit as hperiod has no unit
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

# z_pre_spwn
# SRP 
test_that("operatingModel srp methods",{
    FCB <- array(c(1,1,2,1,2,1,1,2,2), dim=c(3,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    # Recruitment in seasons 1 and 3, 
    om <- make_test_operatingModel(ple4, FCB, nseasons = 4, recruitment_seasons = c(1,3), recruitment_age = 1, niters = 20, sd = 0.1)
    # Spwns in middle of timestep
    biols <- lapply(om[["biols"]], function(x) {
                        spwn(x[["biol"]])[,,,1,] <- 0.5
                        spwn(x[["biol"]])[,,,3,] <- 0.5
                        return(x)
    })
    om[["biols"]] <- biols
    flbs_in <- lapply(om$biols, function(x) return(x[["biol"]]))

    # Biol 1 - fished by 1 fishery only
    biol_no <- 1
    fishery_no <- 1
    catch_no <- 1
    dim <- dim(flbs_in[[biol_no]]@n)
    year <- round(runif(1, min=1, max=dim[2]))
    indices_min <- c(1,year,1,1,1,1)
    indices_max <- c(dim[1],year, dim[3], dim[4], dim[5], dim[6])
    prop_in <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, biol_no, indices_min[-1], indices_max[-1])
    f_in <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, indices_min, indices_max)
    f_pre_spwn_in <- (f_in %*% prop_in)
    m_pre_spwn_in <- (flbs_in[[biol_no]]@m %*% flbs_in[[biol_no]]@spwn[1,])[,year,]
    z_pre_spwn_in <- m_pre_spwn_in + f_pre_spwn_in
    exp_z_pre_spwn_in <- exp(-z_pre_spwn_in)
    # replace NA with 0.0
    exp_z_pre_spwn_in[is.na(exp_z_pre_spwn_in)] <- 0.0
    exp_z_pre_spwn_out <- test_operatingModel_get_exp_z_pre_spwn(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min, indices_max)
    expect_FLQuant_equal(exp_z_pre_spwn_in, exp_z_pre_spwn_out)

    # Biol 2 - fished by a couple
    dim <- dim(flbs_in[[2]]@n)
    year <- round(runif(1, min=1, max=dim[2]))
    indices_min <- c(1,year,1,1,1,1)
    indices_max <- c(dim[1],year, dim[3], dim[4], dim[5], dim[6])
    prop_in12 <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, indices_min[-1], indices_max[-1])
    f_in122 <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, 2, indices_min, indices_max)
    prop_in21 <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 2, indices_min[-1], indices_max[-1])
    f_in212 <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 1, 2, indices_min, indices_max)
    f_pre_spwn_in <- (f_in122 %*% prop_in12) + (f_in212 %*% prop_in21)
    m_pre_spwn_in <- (flbs_in[[2]]@m %*% flbs_in[[2]]@spwn[1,])[,year,]
    z_pre_spwn_in <- m_pre_spwn_in + f_pre_spwn_in
    exp_z_pre_spwn_in <- exp(-z_pre_spwn_in)
    # replace NA with 0.0
    exp_z_pre_spwn_in[is.na(exp_z_pre_spwn_in)] <- 0.0
    exp_z_pre_spwn_out <- test_operatingModel_get_exp_z_pre_spwn(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, indices_min, indices_max)
    expect_FLQuant_equal(exp_z_pre_spwn_in, exp_z_pre_spwn_out)
})


# SRP 
test_that("operatingModel srp methods",{
    # Assumes that f_prop_spwn and f methods are working correctly
    # Calculated as SSB: N*mat*wt*exp(-Fprespwn - m*spwn) summed over age dimension
    # Make a complicated OM with seasons and units
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    # Recruitment in seasons 1 and 3, 
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
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    prop_in <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, biol_no, f_indices_min[-1], f_indices_max[-1])
    f_in <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, f_indices_min, f_indices_max)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-(f_in %*% prop_in) - (flbs_in[[biol_no]]@m %*% flbs_in[[biol_no]]@spwn[1,])))
    srp_in[is.na(srp_in)] <- 0.0 # replace NA with 0 - NA is from NA in spwn - means no spawning so SRP is 0
    # Get full range from input data
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(srp_in, srp_out)
    # Subset the full range
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
    # Total full
    srp_out <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(apply(srp_in, c(1,2,4,5,6), sum, na.rm=TRUE), srp_out)
    # Total subset
    srp_out <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(apply(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], c(1,2,4,5,6), sum, na.rm=TRUE), srp_out)

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
    srp_in[is.na(srp_in)] <- 0.0 # replace NA with 0
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(srp_in, srp_out)
    # Subset the full range
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
    # Total full
    srp_out <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(apply(srp_in, c(1,2,4,5,6), sum, na.rm=TRUE), srp_out)
    # Total subset
    srp_out <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(apply(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], c(1,2,4,5,6), sum, na.rm=TRUE), srp_out)

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
    srp_in[is.na(srp_in)] <- 0.0 # replace NA with 0
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(srp_in, srp_out)
    # Subset the full range
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
    # Total full
    srp_out <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(apply(srp_in, c(1,2,4,5,6), sum, na.rm=TRUE), srp_out)
    # Total subset
    srp_out <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(apply(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], c(1,2,4,5,6), sum, na.rm=TRUE), srp_out)

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
    srp_in[is.na(srp_in)] <- 0.0 # replace NA with 0
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(srp_in, srp_out)
    # Subset the full range
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
    # Total full
    srp_out <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(apply(srp_in, c(1,2,4,5,6), sum, na.rm=TRUE), srp_out)
    # Total subset
    srp_out <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(apply(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], c(1,2,4,5,6), sum, na.rm=TRUE), srp_out)

    # Biol 5 - no fishing
    biol_no <- 5
    dim <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    dim[1] <- 1
    # Full range
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-sweep(flbs_in[[biol_no]]@m, 2:6, flbs_in[[biol_no]]@spwn[1,], "*")))
    srp_in[is.na(srp_in)] <- 0.0 # replace NA with 0
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(srp_in, srp_out)
    # Subset the full range
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
    # Total full
    srp_out <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, f_indices_min[-1], f_indices_max[-1])
    expect_FLQuant_equal(apply(srp_in, c(1,2,4,5,6), sum, na.rm=TRUE), srp_out)
    # Total subset
    srp_out <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    expect_FLQuant_equal(apply(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], c(1,2,4,5,6), sum, na.rm=TRUE), srp_out)
})


test_that("operatingModel calc_rec annual OM", {
    FCB <- array(c(1,1,1), dim=c(1,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    # Annual model
    niters <- 20
    for (recruitment_age in 0:3){
        om <- make_test_operatingModel(ple4, FCB, nseasons = 1, recruitment_age = recruitment_age, niters = niters, sd = 0.1)
        pars <- om[["biols"]][[1]][["srr_params"]]
        res <- om[["biols"]][[1]][["srr_residuals"]]
        om[["biols"]][[1]][["srr_residuals_mult"]] <- TRUE
        rec_year <- round(runif(1, min=5, max=10))
        # Get the SRP - rec age is 1, so get SRP year before, 
        srp <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, c(rec_year-recruitment_age,1,1,1,1), c(rec_year-recruitment_age,1,1,1,niters))
        rec_in <- pars[1,] * srp / (pars[2,] + srp) # bevholt
        rec_in <- rec_in * res[,rec_year]
        rec_out <- test_operatingModel_calc_rec(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 1, rec_year)
        expect_equal(c(rec_in), rec_out)
        # Additive
        om[["biols"]][[1]][["srr_residuals_mult"]] <- FALSE
        rec_in <- pars[1,] * srp / (pars[2,] + srp) # bevholt
        rec_in <- rec_in + res[,rec_year]
        rec_out <- test_operatingModel_calc_rec(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 1, rec_year)
        expect_equal(c(rec_in), rec_out)
    }
})

test_that("operatingModel calc_rec seasonal OM - 1 recruitment event", {
    FCB <- array(c(1,1,1), dim=c(1,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    niters <- 20
    nseasons <- 4
    for (recruitment_season in 1:nseasons){
        # Seasonal model
        for (recruitment_age in 0:3){ # recruitment age 0 is special case
            om <- make_test_operatingModel(ple4, FCB, nseasons = 4, recruitment_seasons = recruitment_season, recruitment_age = recruitment_age, niters = niters, sd = 0.1)
            pars <- om[["biols"]][[1]][["srr_params"]]
            res <- om[["biols"]][[1]][["srr_residuals"]]
            rec_year <- round(runif(1, min=5, max=10))
            for(season in 1:4){
                if (recruitment_age == 0){
                    srp_season <- season - 1
                    srp_year <- rec_year
                    if (srp_season < 1){
                        srp_season <- nseasons
                        srp_year <- rec_year - 1
                    }
                }
                if (recruitment_age > 0){
                    srp_season <- season
                    srp_year <- rec_year - recruitment_age 
                }
                srp <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, c(srp_year,1,srp_season,1,1), c(srp_year,1,srp_season,1,niters))
                rec_in <- pars[1,,,season] * srp / (pars[2,,,season] + srp) # bevholt
                rec_in <- rec_in * res[,rec_year,,season]
                rec_out <- test_operatingModel_calc_rec(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 1, ((rec_year-1) * 4)+season)
                expect_equal(c(rec_in), rec_out)
            }
        }
    }
})

test_that("operatingModel calc_rec seasonal OM - multiple recruitment event", {
    FCB <- array(c(1,1,1), dim=c(1,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    niters <- 20
    nseasons <- 4
    recruitment_seasons <- c(1,3)
    for (recruitment_age in 0:3){
        om <- make_test_operatingModel(ple4, FCB, nseasons = 4, recruitment_seasons = recruitment_seasons, recruitment_age = recruitment_age, niters = niters, sd = 0.1)
        pars <- om[["biols"]][[1]][["srr_params"]]
        res <- om[["biols"]][[1]][["srr_residuals"]]
        rec_year <- round(runif(1, min=5, max=10))
        for(season in 1:4){
            if (recruitment_age == 0){
                srp_season <- season - 1
                srp_year <- rec_year
                if (srp_season < 1){
                    srp_season <- nseasons
                    srp_year <- rec_year - 1
                }
            }
            if (recruitment_age > 0){
                srp_season <- season
                srp_year <- rec_year - recruitment_age 
            }
            srp <- test_operatingModel_total_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, c(srp_year,1,srp_season,1,1), c(srp_year,2,srp_season,1,niters))
            # Test by unit
            for (unit in 1:length(recruitment_seasons)){
                rec_in <- pars[1,,unit,season] * srp / (pars[2,,unit,season] + srp) # bevholt
                rec_in <- rec_in * res[,rec_year,unit,season]
                rec_out <- test_operatingModel_calc_rec(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, unit, ((rec_year-1) * nseasons)+season)
                expect_equal(c(rec_in), rec_out)
            }
        }
    }
})



