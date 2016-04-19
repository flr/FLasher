context("Test operating model")

test_that("operatingModel constructors",{
    # Empty constructor - just check it doesn't fail
    test_operatingModel_empty_constructor()
    # Main constructor test
    # Set up parameters for full test 
    flq <- random_FLQuant_generator()
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Test as and wrap
    out <- test_operatingModel_full_constructor(flfs, flbs, fc)
    expect_identical(out[["biols"]], flbs_in)
    test_FLFisheries_equal(out[["fisheries"]], flfs)
    test_fwdControl_equal(out[["ctrl"]], fc)
})

test_that("operatingModel constructor dimension checks",{
    flq <- random_FLQuant_generator(min_dims=c(2,2,2,2,2,2))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    good_dim <- dim(flq)
    bad_year_dim <- good_dim
    bad_year_dim[2] <- good_dim[2] + 1
    bad_season_dim <- good_dim
    bad_season_dim[4] <- good_dim[4] + 1
    bad_iter_dim <- good_dim
    bad_iter_dim[6] <- good_dim[6] + 1
    bad_age_dim <- good_dim
    bad_age_dim[1] <- good_dim[1] + 1
    bad_unit_dim <- good_dim
    bad_unit_dim[3] <- good_dim[3] + 1
    # Check Biols year and season match catch - catches already forced to have same through validity check of FLFisheries
    bad_year_flb <- random_FLBiolcpp_generator(fixed_dims = bad_year_dim)
    bad_season_flb <- random_FLBiolcpp_generator(fixed_dims = bad_season_dim)
    bad_biol <- round(runif(1,min=1,max=length(flbs)))
    bad_flbs <- flbs
    bad_flbs[[bad_biol]][["biol"]] <- bad_year_flb
    bad_flbs[[bad_biol]][["srr_residuals"]] <- random_FLQuant_generator(fixed_dims=bad_year_dim)
    expect_error(test_operatingModel_full_constructor(flfs, bad_flbs, fc))
    bad_flbs[[bad_biol]][["biol"]] <- bad_season_flb
    bad_flbs[[bad_biol]][["srr_residuals"]] <- random_FLQuant_generator(fixed_dims=bad_season_dim)
    expect_error(test_operatingModel_full_constructor(flfs, bad_flbs, fc))
    # Check iters in effort, landings, discards and n must be the same
    bad_flfs <- random_FLFisheries_generator(fixed_dims = bad_iter_dim, min_fisheries=2, max_fisheries=2)
    expect_error(test_operatingModel_full_constructor(bad_flfs, flbs, fc))
    # Check age and unit range for catches and biols is the same - catch 1 catches biol 1
    bad_age_flb <- random_FLBiolcpp_generator(fixed_dims = bad_age_dim)
    bad_flbs <- flbs
    bad_flbs[[1]][["biol"]] <- bad_age_flb
    bad_flbs[[1]][["srr_residuals"]] <- random_FLQuant_generator(fixed_dims=bad_age_dim)
    expect_error(test_operatingModel_full_constructor(flfs, bad_flbs, fc))
    bad_unit_flb <- random_FLBiolcpp_generator(fixed_dims = bad_unit_dim)
    bad_flbs[[bad_biol]][["biol"]] <- bad_unit_flb
    bad_flbs[[bad_biol]][["srr_residuals"]] <- random_FLQuant_generator(fixed_dims=bad_unit_dim)
    expect_error(test_operatingModel_full_constructor(flfs, bad_flbs, fc))
    # Check iters in the control object is 1 or n
    # iter = 1 - should be OK
    good_fc <- random_fwdControl_generator(years = 1, niters = 1)
    out <- test_operatingModel_full_constructor(flfs, flbs, good_fc)
    bad_fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6]+1)
    expect_error(test_operatingModel_full_constructor(flfs, flbs, bad_fc))
})

test_that("operatingModel housekeeping",{
    flq <- random_FLQuant_generator()
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    out <- test_operatingModel_get_niter(flfs, flbs, fc)
    expect_equal(out, dim(flq)[6])
})

# Test F method with random Biols and Fisheries
# No check if FC catches B
test_that("operatingModel get_f method for FCB with random OM objects - just partial F - one catch on one biol",{
    flq <- random_FLQuant_generator(min_dims = c(2,2,2,2,2,2))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    # fwdControl and FCB needed for constructor but not actually used to test F
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    attr(fc, "FCB") <- array(0, dim = c(10,3))
    # Any FLC FLB combination
    fishery_no <- round(runif(1,min=1, max=length(flfs)))
    catch_no <- round(runif(1,min=1, max=length(flfs[[fishery_no]])))
    biol_no <- round(runif(1,min=1, max=length(flbs)))
    cq_flq <- as(catch.q(flfs[[fishery_no]][[catch_no]]), "FLQuant")
    biomass <- quantSums(n(flbs[[biol_no]][["biol"]]) * wt(flbs[[biol_no]][["biol"]]))
    qin <- sweep(sweep(biomass, 6, -cq_flq[2,], "^"), 6, cq_flq[1], "*")
    fin <- sweep(catch.sel(flfs[[fishery_no]][[catch_no]]), 2:6, qin %*% flfs[[fishery_no]]@effort, "*")
    # Full FLQ
    fout <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)
    expect_equal(dim(fout), dim(fin))
    expect_equal(c(fout), c(fin))
    # Subset FLQ
    dim_max <- dim(flq)
    dim_min <- round(runif(6, min=1, max = dim_max))
    fout <- test_operatingModel_get_f_FCB_subset(flfs, flbs, fc, fishery_no, catch_no, biol_no, dim_min, dim_max)
    fin_sub <- fin[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2],dim_min[3]:dim_max[3],dim_min[4]:dim_max[4],dim_min[5]:dim_max[5],dim_min[6]:dim_max[6]]
    expect_equal(dim(fout), dim(fin_sub))
    expect_equal(c(fout), c(fin_sub))
    # Fbar subset 
    fbar_out <- test_operatingModel_fbar_subset1(flfs, flbs, fc, fishery_no, catch_no, biol_no, dim_min, dim_max)
    fbar_in <- apply(fout, 2:6, mean)
    test_FLQuant_equal(fbar_in, fbar_out)
    # With years in the Catch Q pars too
    flp <- FLPar(abs(rnorm(2 * dim(flq)[6] * dim(flq)[2])), dimnames = list(params = c("alpha","beta"), year=1:dim(flq)[2], iter = 1:dim(flq)[6]))
    catch.q(flfs[[fishery_no]][[catch_no]]) <- flp # desc removes! Why?
    flfs@desc <- "mwng"
    cq_flq <- as(catch.q(flfs[[fishery_no]][[catch_no]]), "FLQuant")
    biomass <- quantSums(n(flbs[[biol_no]][["biol"]]) * wt(flbs[[biol_no]][["biol"]]))
    qin <- sweep(sweep(biomass, c(2,6), -cq_flq[2,], "^"), c(2,6), cq_flq[1], "*")
    fin <- sweep(catch.sel(flfs[[fishery_no]][[catch_no]]), 2:6, qin %*% flfs[[fishery_no]]@effort, "*")
    # Full FLQ
    fout <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)
    expect_equal(dim(fout), dim(fin))
    expect_equal(c(fout), c(fin))
    # Subset FLQ
    dim_max <- dim(flq)
    dim_min <- round(runif(6, min=1, max = dim_max))
    fout <- test_operatingModel_get_f_FCB_subset(flfs, flbs, fc, fishery_no, catch_no, biol_no, dim_min, dim_max)
    fin_sub <- fin[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2],dim_min[3]:dim_max[3],dim_min[4]:dim_max[4],dim_min[5]:dim_max[5],dim_min[6]:dim_max[6]]
    expect_equal(dim(fout), dim(fin_sub))
    expect_equal(c(fout), c(fin_sub))
    # With units in the Catch Q pars too
    flp <- FLPar(abs(rnorm(2 * dim(flq)[6] * dim(flq)[3])), dimnames = list(params = c("alpha","beta"), unit=1:dim(flq)[3], iter = 1:dim(flq)[6]))
    catch.q(flfs[[fishery_no]][[catch_no]]) <- flp # desc removes! Why?
    flfs@desc <- "mwng"
    cq_flq <- as(catch.q(flfs[[fishery_no]][[catch_no]]), "FLQuant")
    biomass <- quantSums(n(flbs[[biol_no]][["biol"]]) * wt(flbs[[biol_no]][["biol"]]))
    qin <- sweep(sweep(biomass, c(3,6), -cq_flq[2,], "^"), c(3,6), cq_flq[1], "*")
    fin <- sweep(catch.sel(flfs[[fishery_no]][[catch_no]]), 2:6, qin %*% flfs[[fishery_no]]@effort, "*")
    # Full FLQ
    fout <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)
    expect_equal(dim(fout), dim(fin))
    expect_equal(c(fout), c(fin))
    # Subset FLQ
    dim_max <- dim(flq)
    dim_min <- round(runif(6, min=1, max = dim_max))
    fout <- test_operatingModel_get_f_FCB_subset(flfs, flbs, fc, fishery_no, catch_no, biol_no, dim_min, dim_max)
    fin_sub <- fin[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2],dim_min[3]:dim_max[3],dim_min[4]:dim_max[4],dim_min[5]:dim_max[5],dim_min[6]:dim_max[6]]
    expect_equal(dim(fout), dim(fin_sub))
    expect_equal(c(fout), c(fin_sub))
})

test_that("get_f for biols with example operatingModel1 - total Fs from multiple FLFishery objects",{
    # Get the total Fs on Biols
    # Uses the FCB attribute
    om <- make_test_operatingModel1(20)
    flfs <- om[["fisheries"]]
    flbs_in <- lapply(om[["biols"]], function(x) return(x[["biol"]]))
    nbiols <- length(om[["biols"]])
    for (i in 1:nbiols){
        dim <- dim(n(om[["biols"]][[i]][["biol"]]))
        indices_max <- round(runif(6,1,dim))
        indices_min <- round(runif(6,1,indices_max))
        fout <- test_operatingModel_get_f_B_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], i, indices_min,  indices_max)
        fout_all <- test_operatingModel_get_f_B(om[["fisheries"]], om[["biols"]], om[["fwc"]], i)
        FC <- om[["fwc"]]@FCB[om[["fwc"]]@FCB[,"B"] == i,,drop=FALSE]
        if (nrow(FC) > 0){
            # Loop over each FC
            biomass <- quantSums(n(flbs_in[[i]]) * wt(flbs_in[[i]]))
            fin <- FLQuant(0, dim=dim)
            for (j in 1:nrow(FC)){
                cq_flq <- as(catch.q(flfs[[FC[j,"F"]]][[FC[j,"C"]]]), "FLQuant")
                qin <- (c(cq_flq[1,])) * biomass ^ (-c(cq_flq[2,]))
                fin <- fin + sweep(catch.sel(flfs[[FC[j,"F"]]][[FC[j,"C"]]]), 2:6, qin %*% flfs[[FC[j,"F"]]]@effort, "*")
            }
            test_FLQuant_equal(fin, fout_all)
            test_FLQuant_equal(fin[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2],indices_min[3]:indices_max[3],indices_min[4]:indices_max[4],indices_min[5]:indices_max[5],indices_min[6]:indices_max[6]], fout)
            # Check fbar
            fbar_out <- test_operatingModel_fbar_subset2(om[["fisheries"]], om[["biols"]], om[["fwc"]], i, indices_min, indices_max)
            test_FLQuant_equal(apply(fout, 2:6, mean), fbar_out)
        }
    }
})

test_that("operatingModel get_unit_z and unit_f- one catch on one biol",{
    flq <- random_FLQuant_generator(min_dims = c(2,2,5,2,2,2))
    flbs <- random_fwdBiols_list_generator(min_biols = 1, max_biols = 1, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=1, max_fisheries=1)
    # fwdControl and FCB needed for constructor but not actually used to test F
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    FCB <- array(1, dim = c(1,3))
    attr(fc, "FCB") <- FCB
    dim_max <- dim(flq)
    dim_min <- round(runif(6, min=1, max = dim_max))
    # Assume F is correct
    fin <- test_operatingModel_get_f_B_subset(flfs, flbs, fc, 1, dim_min, dim_max)
    min <- flbs_in[[1]]@m[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2],dim_min[3]:dim_max[3],dim_min[4]:dim_max[4],dim_min[5]:dim_max[5],dim_min[6]:dim_max[6]]
    zin <- min+fin
    nin <- flbs_in[[1]]@n[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2],dim_min[3]:dim_max[3],dim_min[4]:dim_max[4],dim_min[5]:dim_max[5],dim_min[6]:dim_max[6]]
    survivors <- nin*exp(-zin)
    unit_zin <- -log(unitSums(survivors) / unitSums(nin))
    unit_zout <- test_operatingModel_unit_z_subset(flfs, flbs, fc, 1, dim_min, dim_max)
    test_FLQuant_equal(unit_zin, unit_zout)


    unit_fout <- test_operatingModel_unit_f_subset(flfs, flbs, fc, 1, dim_min, dim_max)

})

test_that("operatingModel f_prop_spwn methods",{
    # Random OM with units
    flq <- random_FLQuant_generator()
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Random fishery and biol
    biol_no <- round(runif(1, min=1, max=length(flbs)))
    fishery_no <- round(runif(1, min=1, max=length(flfs)))
    # Get full FLQ
    indices_min <- rep(1,6)
    indices_max <- dim(n(flbs_in[[biol_no]]))
    prop_in <- (spwn(flbs_in[[biol_no]])[1,] %-% flfs[[fishery_no]]@hperiod[1,]) %/% (flfs[[fishery_no]]@hperiod[2,] - flfs[[fishery_no]]@hperiod[1,])
    # If hperiod1 > spwn, prop = 1
    # If hperiod2 < spwn, prop = 0
    nunit <- dim(flq)[3]
    for (i in 1:nunit){
        prop_in_temp <- prop_in[,,i,]
        prop_in_temp@.Data[flfs[[fishery_no]]@hperiod[1,,1] >= spwn(flbs_in[[biol_no]])[1,,i] ] <- 0
        prop_in_temp@.Data[flfs[[fishery_no]]@hperiod[2,,1] <= spwn(flbs_in[[biol_no]])[1,,i] ] <- 1
        prop_in[,,i,] <- prop_in_temp
    }
    prop_out <- test_operatingModel_f_prop_spwn_FLQ_subset(flfs, flbs, fc, fishery_no, biol_no, indices_min[-1], indices_max[-1])
    test_FLQuant_equal(prop_in, prop_out) 
    # Subset of f
    indices_max <- round(runif(6,1,dim(prop_in)))
    indices_min <- round(runif(6,1,indices_max))
    prop_out <- test_operatingModel_f_prop_spwn_FLQ_subset(flfs, flbs, fc, fishery_no, biol_no, indices_min[-1], indices_max[-1])
    test_FLQuant_equal(prop_in[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], prop_out)
})

test_that("operatingModel srp methods with units",{
    # Random simple OM with units - 1 biol fished by 1 catch
    flq <- random_FLQuant_generator(min_dims=c(2,2,2,2,2,2))
    flbs <- random_fwdBiols_list_generator(min_biols = 1, max_biols = 1, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=1, max_fisheries=1)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    FCB <- array(NA, dim=c(1,3))
    FCB[1,] <- c(1,1,1)
    attr(fc, "FCB") <- FCB
    # Biol 1
    biol_no <- 1
    fishery_no <- 1
    catch_no <- 1
    dim <- dim(flbs_in[[1]]@n)
    # Get full range
    indices_min <- rep(1,6)
    indices_max <- dim
    prop_in <- test_operatingModel_f_prop_spwn_FLQ_subset(flfs, flbs, fc, fishery_no, biol_no, indices_min[-1], indices_max[-1])
    f_in <- test_operatingModel_get_f_FCB_subset(flfs, flbs, fc, fishery_no, catch_no, biol_no, indices_min, indices_max)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-(f_in %*% prop_in) - (flbs_in[[biol_no]]@m %*% flbs_in[[biol_no]]@spwn[1,])))
    srp_out <- test_operatingModel_SRP_FLQ_subset(flfs, flbs, fc, biol_no, indices_min[-1], indices_max[-1])
    test_FLQuant_equal(srp_in, srp_out)
    # Subset
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    srp_out <- test_operatingModel_SRP_FLQ_subset(flfs, flbs, fc, biol_no, indices_min[-1], indices_max[-1])
    test_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
})

test_that("operatingModel srp methods with annual OM",{
    # Assumes that f_prop_spwn and f is working correctly
    # Calculated as SSB: N*mat*wt*exp(-Fprespwn - m*spwn) summed over age dimension
    om <- make_test_operatingModel1(niters = 100)
    flbs_in <- lapply(om$biols, function(x) return(x[["biol"]]))
    # Biol 1
    biol_no <- 1
    fishery_no <- 1
    catch_no <- 1
    dim <- dim(flbs_in[[biol_no]]@n)
    dim[1] <- 1
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    # Get full range
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    prop_in <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, biol_no, f_indices_min[-1], f_indices_max[-1])
    f_in <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, f_indices_min, f_indices_max)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-(f_in %*% prop_in) - (flbs_in[[biol_no]]@m %*% flbs_in[[biol_no]]@spwn[1,])))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    test_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
    # Biol 2
    biol_no <- 2
    fishery_no <- 1
    catch_no <- 2
    dim <- dim(flbs_in[[biol_no]]@n)
    dim[1] <- 1
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    # Full range
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    prop_in1 <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, biol_no, f_indices_min[-1], f_indices_max[-1])
    f_in1 <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, 2, biol_no, f_indices_min, f_indices_max)
    prop_in2 <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, biol_no, f_indices_min[-1], f_indices_max[-1])
    f_in2 <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, 1, biol_no, f_indices_min, f_indices_max)
    f_prop <- (f_in1 %*% prop_in1) + (f_in2 %*% prop_in2)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-f_prop - (flbs_in[[biol_no]]@m %*% flbs_in[[biol_no]]@spwn[1,])))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    test_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
    # Biol 3
    biol_no <- 3
    fishery_no <- 2
    catch_no <- 2
    dim <- dim(flbs_in[[biol_no]]@n)
    dim[1] <- 1
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    # Full range
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    prop_in <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, biol_no, f_indices_min[-1], f_indices_max[-1])
    f_in <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, f_indices_min, f_indices_max)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-(f_in %*% prop_in) - (flbs_in[[biol_no]]@m %*% flbs_in[[biol_no]]@spwn[1,])))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    test_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
    # Biol 4
    biol_no <- 4
    fishery_no <- 2
    catch_no <- 2
    dim <- dim(flbs_in[[biol_no]]@n)
    dim[1] <- 1
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    # Full range
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    prop_in <- test_operatingModel_f_prop_spwn_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, biol_no, f_indices_min[-1], f_indices_max[-1])
    f_in <- test_operatingModel_get_f_FCB_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], fishery_no, catch_no, biol_no, f_indices_min, f_indices_max)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-(f_in %*% prop_in) - (flbs_in[[biol_no]]@m %*% flbs_in[[biol_no]]@spwn[1,])))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    test_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
    # Biol 5 - no fishing
    biol_no <- 5
    dim <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    dim[1] <- 1
    indices_max <- round(runif(6,1,dim))
    indices_min <- round(runif(6,1,indices_max))
    # Full range
    f_indices_min <- rep(1,6)
    f_indices_max <- dim(om[["biols"]][[biol_no]][["biol"]]@n)
    srp_in <- quantSums(flbs_in[[biol_no]]@n * flbs_in[[biol_no]]@mat * flbs_in[[biol_no]]@wt * exp(-sweep(flbs_in[[biol_no]]@m, 2:6, flbs_in[[biol_no]]@spwn[1,], "*")))
    srp_out <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, indices_min[-1], indices_max[-1])
    test_FLQuant_equal(srp_in[,indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]], srp_out)
})

test_that("operatingModel calc_rec simple OM", {
    # Testing with a simple annual model
    # Params have no structure (recycle)
    om <- make_test_operatingModel1(niters = 100)
    # With Bevholt
    biol_no <- 1 
    flq <- n(om[["biols"]][[biol_no]][["biol"]])
    rec_year <- round(runif(1,min=2,max=dim(flq)[2]))
    rec_season <- 1
    # Here SRP timing is 1 year behind the rec timestep but the same season
    srp_indices_min <- c(rec_year-1,1,1,1,1)
    srp_indices_max <- c(rec_year-1,1,1,1,dim(flq)[6])
    # Assume SRP calc works
    srp_in <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, srp_indices_min, srp_indices_max)
    # It's a Bevholt model
    a <- c(om[["biols"]][[biol_no]][["srr_params"]]['a',])
    b <- c(om[["biols"]][[biol_no]][["srr_params"]]['b',])
    rec_in_det <- (a * srp_in) / (b + srp_in)
    # Apply the residuals - multiplicative
    om[["biols"]][[biol_no]][["srr_residuals"]] <- exp(om[["biols"]][[biol_no]][["srr_residuals"]])
    om[["biols"]][[biol_no]][["srr_residuals_mult"]] <- TRUE
    rec_in_mult <- rec_in_det * om[["biols"]][[biol_no]][["srr_residuals"]][,rec_year,]
    rec_out_mult <- test_operatingModel_calc_rec(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, 1, rec_year)
    expect_equal(c(rec_in_mult), rec_out_mult)
    # Additive residuals
    om[["biols"]][[biol_no]][["srr_residuals"]] <- log(om[["biols"]][[biol_no]][["srr_residuals"]])
    om[["biols"]][[biol_no]][["srr_residuals_mult"]] <- FALSE
    rec_in_add <- rec_in_det + om[["biols"]][[biol_no]][["srr_residuals"]][,rec_year,1,rec_season,1,]
    rec_out_add <- test_operatingModel_calc_rec(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, 1, rec_year)
    expect_equal(c(rec_in_add), rec_out_add)
    # With Ricker
    biol_no <- 2 
    flq <- n(om[["biols"]][[biol_no]][["biol"]])
    rec_year <- round(runif(1,min=2,max=dim(flq)[2]))
    # Here SRP timing is 1 year behind the rec timestep but the same season
    srp_indices_min <- c(rec_year-1,1,1,1,1)
    srp_indices_max <- c(rec_year-1,1,1,1,dim(flq)[6])
    srp_in <- test_operatingModel_SRP_FLQ_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, srp_indices_min, srp_indices_max)
    # It's a Ricker model
    a <- c(om[["biols"]][[biol_no]][["srr_params"]]['a',])
    b <- c(om[["biols"]][[biol_no]][["srr_params"]]['b',])
    rec_in_det <- a * srp_in * exp(-b * srp_in)
    # Apply the residuals - multiplicative
    om[["biols"]][[biol_no]][["srr_residuals"]] <- exp(om[["biols"]][[biol_no]][["srr_residuals"]])
    om[["biols"]][[biol_no]][["srr_residuals_mult"]] <- TRUE
    rec_in_mult <- rec_in_det * om[["biols"]][[biol_no]][["srr_residuals"]][,rec_year,]
    rec_out_mult <- test_operatingModel_calc_rec(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, 1, rec_year)
    expect_equal(c(rec_in_mult), rec_out_mult)
    # Additive residuals
    om[["biols"]][[biol_no]][["srr_residuals"]] <- log(om[["biols"]][[biol_no]][["srr_residuals"]])
    om[["biols"]][[biol_no]][["srr_residuals_mult"]] <- FALSE
    rec_in_add <- rec_in_det + om[["biols"]][[biol_no]][["srr_residuals"]][,rec_year,1,rec_season,1,]
    rec_out_add <- test_operatingModel_calc_rec(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, 1, rec_year)
    expect_equal(c(rec_in_add), rec_out_add)
})

test_that("operatingModel calc_rec complex OM", {
    # Biol with 4 seasons and 4 units, fished by 1 fishery and 1 catch
    # First age = 0, SRP lag = 1 timestep
    min_age_name <- 0
    flq <- random_FLQuant_generator(fixed_dims = c(5,6,4,4,1,10))
    flbs <- random_fwdBiols_list_generator(min_biols = 1, max_biols = 1, fixed_dims = dim(flq), min_age_name = min_age_name)
    # Set SR params - same every year but broken down by unit and season - necessary
    # Units 1 and 2 recruit in season 1
    # Units 3 and 4 recruit in season 3
    srr_params <- FLQuant(NA, dim=c(2,1,4,4,1,10))
    srr_params[,,c(1,2),1,] <- abs(rnorm(40))
    srr_params[,,c(3,4),3,] <- abs(rnorm(40))
    flbs[[1]][["srr_params"]] <- srr_params
    flbs[[1]][["srr_model_name"]] <- "bevholt"
    flbs[[1]][["srr_residuals_mult"]] <- TRUE
    # Pull out just FLBiol for testing
    flb_in <- flbs[[1]][["biol"]]
    # Make fishery - single catch
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=1, max_fisheries=1, min_catches=1, max_catches=1, min_age_name = min_age_name)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Fix FCB
    FCB <- array(NA, dim=c(1,3))
    FCB[1,] <- c(1,1,1)
    attr(fc, "FCB") <- FCB
    # Rec in season 1
    rec_year <- round(runif(1,min=2,max=dim(flq)[2]))
    rec_season <- 1
    rec_timestep <- (rec_year-1)*dim(flq)[4] + rec_season
    # Rec for all units in that timestep
    srp_indices_min <- c(rec_year-1,1,4,1,1)
    srp_indices_max <- c(rec_year-1,dim(flq)[3],4,1,dim(flq)[6])
    srp_in <- test_operatingModel_SRP_FLQ_subset(flfs, flbs, fc, 1, srp_indices_min, srp_indices_max)
    # All units at same time
    a <- (srr_params[1,1,,rec_season,])
    b <- (srr_params[2,1,,rec_season,])
    rec_in_det <- a * srp_in / (b + srp_in)
    rec_in_mult <- rec_in_det * flbs[[1]][["srr_residuals"]][,rec_year,,rec_season,,]
    for (unit_count in 1:2){ # Only 1 and 2 recruit
        rec_out_mult <- test_operatingModel_calc_rec(flfs, flbs, fc, 1, unit_count, rec_timestep)
        expect_equal(rec_out_mult, c(rec_in_mult[,,unit_count,]))
    }
    for (unit_count in 3:4){ # No recruitment for units 3 and 4 NAs
        expect_true(all(is.na(test_operatingModel_calc_rec(flfs, flbs, fc, 1, unit_count, rec_timestep))))
    }
    # Rec in seasons 2 and 4
    for (rec_season in c(2,4)){
        rec_year <- round(runif(1,min=2,max=dim(flq)[2]))
        rec_timestep <- (rec_year-1)*dim(flq)[4] + rec_season
        # Rec for all units in that timestep
        srp_indices_min <- c(rec_year,1,rec_season-1,1,1)
        srp_indices_max <- c(rec_year,dim(flq)[3],rec_season-1,1,dim(flq)[6])
        srp_in <- test_operatingModel_SRP_FLQ_subset(flfs, flbs, fc, 1, srp_indices_min, srp_indices_max)
        # All units at same time
        a <- (srr_params[1,1,,rec_season,])
        b <- (srr_params[2,1,,rec_season,])
        rec_in_det <- a * srp_in / (b + srp_in)
        rec_in_mult <- rec_in_det * flbs[[1]][["srr_residuals"]][,rec_year,,rec_season,,]
        for (unit_count in 1:4){ # No recruitment 
            expect_true(all(is.na(test_operatingModel_calc_rec(flfs, flbs, fc, 1, unit_count, rec_timestep))))
        }
    }
    # Rec in season 3
    rec_year <- round(runif(1,min=2,max=dim(flq)[2]))
    rec_season <- 3
    rec_timestep <- (rec_year-1)*dim(flq)[4] + rec_season
    # Rec for all units in that timestep
    srp_indices_min <- c(rec_year,1,rec_season-1,1,1)
    srp_indices_max <- c(rec_year,dim(flq)[3],rec_season-1,1,dim(flq)[6])
    srp_in <- test_operatingModel_SRP_FLQ_subset(flfs, flbs, fc, 1, srp_indices_min, srp_indices_max)
    # All units at same time
    a <- (srr_params[1,1,,rec_season,])
    b <- (srr_params[2,1,,rec_season,])
    rec_in_det <- a * srp_in / (b + srp_in)
    rec_in_mult <- rec_in_det * flbs[[1]][["srr_residuals"]][,rec_year,,rec_season,,]
    for (unit_count in 3:4){ # Only 1 and 2 recruit
        rec_out_mult <- test_operatingModel_calc_rec(flfs, flbs, fc, 1, unit_count, rec_timestep)
        expect_equal(rec_out_mult, c(rec_in_mult[,,unit_count,]))
    }
    for (unit_count in 1:2){ # No recruitment for units 3 and 4 NAs
        expect_true(all(is.na(test_operatingModel_calc_rec(flfs, flbs, fc, 1, unit_count, rec_timestep))))
    }
})

test_that("operatingModel project_biol", {
    # Assume get_f method works (it does...)
    # Single biol fished by one catch
    # Seasonal Biol with 4 units (M / F and 2 spawning morphs)
    # Min age = 1
    flq <- random_FLQuant_generator(fixed_dims = c(5,6,4,4,1,10))
    flbs <- random_fwdBiols_list_generator(min_biols = 1, max_biols = 1, fixed_dims = dim(flq))
    # Set SR params - same every year but broken down by unit and season - necessary
    # Units 1 and 2 recruit in season 1
    # Units 3 and 4 recruit in season 3
    srr_params <- FLQuant(NA, dim=c(2,1,4,4,1,10))
    srr_params[,,c(1,2),1,] <- rnorm(40)
    srr_params[,,c(3,4),3,] <- rnorm(40) 
    flbs[[1]][["srr_params"]] <- srr_params
    flbs[[1]][["srr_model_name"]] <- "bevholt"
    flbs[[1]][["srr_residuals_mult"]] <- TRUE
    # Pull out just FLBiol for testing
    flb_in <- flbs[[1]][["biol"]]
    # Make fishery - single catch
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=1, max_fisheries=1, min_catches=1, max_catches=1)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Fix FCB
    FCB <- array(NA, dim=c(1,3))
    FCB[1,] <- c(1,1,1)
    attr(fc, "FCB") <- FCB
    # Project in each season in a random year
    year <- round(runif(1,min=2,dim(flq)[2]))
    # Season 1
    season <- 1
    timestep <- test_year_season_to_timestep(flq, year, season)
    om_out <- test_operatingModel_project_biols(flfs, flbs, fc, timestep)
    fout <- test_operatingModel_get_f_B(flfs, flbs, fc, 1)[,year-1,,4,]
    old_n <- n(flb_in)[,year-1,,4,,]
    survivors <- old_n * exp(-(fout + m(flb_in)[,year-1,,4,]))
    new_n_out <- n(om_out[["biols"]][[1]])[,year,,1,]
    # Check each unit
    # Unit 1 and 2 should have birthday!
    for (unit in c(1,2)){
        test_FLQuant_equal(new_n_out[2:4,,unit,], survivors[1:3,,unit])
        test_FLQuant_equal(new_n_out[5,,unit], survivors[4,,unit]+survivors[5,,unit])
        # Recruitment for units 1 and 2
        a <- srr_params[1,1,,season]
        b <- srr_params[2,1,,season]
        srp_in <- test_operatingModel_SRP_FLQ_subset(flfs, flbs, fc, 1, c(year-1,1,4,1,1), c(year-1,4,4,1,10))
        rec_in <- (a * srp_in / (srp_in + b)) * flbs[[1]][["srr_residuals"]][,year,,1,]
        test_FLQuant_equal(rec_in[,,c(1,2)], new_n_out[1,,c(1,2)])
    }
    for (unit in c(3,4)){
        test_FLQuant_equal(new_n_out[,,unit], survivors[,,unit])
    }
    # Season 2
    season <- 2
    timestep <- test_year_season_to_timestep(flq, year, season)
    om_out <- test_operatingModel_project_biols(flfs, flbs, fc, timestep)
    fout <- test_operatingModel_get_f_B(flfs, flbs, fc, 1)[,year,,season-1,]
    old_n <- n(flb_in)[,year,,season-1,,]
    survivors <- old_n * exp(-(fout + m(flb_in)[,year,,season-1,]))
    new_n_out <- n(om_out[["biols"]][[1]])[,year,,season,]
    # Check each unit at the same time - no recruitment
    test_FLQuant_equal(new_n_out, survivors)
    # Season 3
    season <- 3
    timestep <- test_year_season_to_timestep(flq, year, season)
    om_out <- test_operatingModel_project_biols(flfs, flbs, fc, timestep)
    fout <- test_operatingModel_get_f_B(flfs, flbs, fc, 1)[,year,,season-1,]
    old_n <- n(flb_in)[,year,,season-1,,]
    survivors <- old_n * exp(-(fout + m(flb_in)[,year,,season-1,]))
    new_n_out <- n(om_out[["biols"]][[1]])[,year,,season,]
    # Check each unit
    for (unit in c(1,2)){
        test_FLQuant_equal(new_n_out[,,unit], survivors[,,unit])
    }
    # Unit 3 and 4 should have birthday!
    for (unit in c(3,4)){
        test_FLQuant_equal(new_n_out[2:4,,unit,], survivors[1:3,,unit])
        test_FLQuant_equal(new_n_out[5,,unit], survivors[4,,unit]+survivors[5,,unit])
        # Recruitment for units 3 and 4
        a <- srr_params[1,1,,season]
        b <- srr_params[2,1,,season]
        srp_in <- test_operatingModel_SRP_FLQ_subset(flfs, flbs, fc, 1, c(year,1,3,1,1), c(year,4,3,1,10))
        rec_in <- (a * srp_in / (srp_in + b)) * flbs[[1]][["srr_residuals"]][,year,,3,]
        test_FLQuant_equal(rec_in[,,c(3,4)], new_n_out[1,,c(3,4)])
    }
    # Season 4
    season <- 4
    timestep <- test_year_season_to_timestep(flq, year, season)
    om_out <- test_operatingModel_project_biols(flfs, flbs, fc, timestep)
    fout <- test_operatingModel_get_f_B(flfs, flbs, fc, 1)[,year,,season-1,]
    old_n <- n(flb_in)[,year,,season-1,,]
    survivors <- old_n * exp(-(fout + m(flb_in)[,year,,season-1,]))
    new_n_out <- n(om_out[["biols"]][[1]])[,year,,season,]
    # Check each unit at the same time - no recruitment
    test_FLQuant_equal(new_n_out, survivors)
})

test_that("operatingModel project_fisheries", {
    # Seasonal FLQ with Units
    # Assume get_f method works (it does...)
    # FLB with 2 units - second biol only
    flq_unit <- random_FLQuant_generator(fixed_dims = c(5,6,2,4,1,10))
    flbs_unit <- random_fwdBiols_list_generator(min_biols = 1, max_biols = 1, fixed_dims = dim(flq_unit))
    # Other FLB with 1 unit
    flq <- random_FLQuant_generator(fixed_dims = c(5,6,1,4,1,10))
    flbs <- random_fwdBiols_list_generator(min_biols = 3, max_biols = 3, fixed_dims = dim(flq))
    # Insert FLB 2 into correct place
    flbs[["the 4th"]] <- flbs[[3]]
    flbs[[3]] <- flbs[[2]]
    flbs[[2]] <- flbs_unit[[1]]
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    # Make the fisheries - but we want FLCs 12 and 21 to be based on the units
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
    flfs[[1]][[2]] <- flc_unit <- random_FLCatch_generator(fixed_dims = dim(flq_unit))
    flfs[[2]][[1]] <- flc_unit <- random_FLCatch_generator(fixed_dims = dim(flq_unit))
    flfs@desc <- "Choose life"
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Fix FCB
    FCB <- array(NA, dim=c(5,3))
    FCB[1,] <- c(1,1,1)
    FCB[2,] <- c(1,2,2)
    FCB[3,] <- c(2,1,2)
    FCB[4,] <- c(2,2,3)
    FCB[5,] <- c(2,2,4)
    attr(fc, "FCB") <- FCB
    # Random timestep
    season <- round(runif(1,min=1,max=4))
    year <- round(runif(1,min=1,dim(flq)[2]))
    timestep <- test_year_season_to_timestep(flq, year, season)
    # Go!
    om_out <- test_operatingModel_project_fisheries(flfs, flbs, fc, timestep)
    # FC 11 - no units
    fishery_no <- 1
    catch_no <- 1
    biol_no <- 1
    lout <- landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dout <- discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    cout <- catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    # Check that timestep is correct
    fin <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,,season,1,]
    zin <- fin + m(flbs_in[[biol_no]])[,year,,season,1,]
    cin <- (fin / zin) * (1 - exp(-zin)) * n(flbs_in[[biol_no]])[,year,,season,1]
    dr <- (discards.n(flfs[[fishery_no]][[catch_no]]) / (discards.n(flfs[[fishery_no]][[catch_no]]) + landings.n(flfs[[fishery_no]][[catch_no]])))[,year,,season,]
    # Catch, Landings, Discards numbers
    test_FLQuant_equal(cin, cout)
    test_FLQuant_equal(cin*(1-dr), lout)
    test_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq))) %in% get_FLQuant_elements(flq, c(1,year,1,season,1,1), c(dim(flq)[1], year,1,season, 1, dim(flq)[6])))
    expect_equal(c(landings.n(flfs[[fishery_no]][[catch_no]]))[elem], c(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    expect_equal(c(discards.n(flfs[[fishery_no]][[catch_no]]))[elem], c(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    # FC 12 - units
    fishery_no <- 1
    catch_no <- 2
    biol_no <- 2
    lout <- landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dout <- discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    cout <- catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    fin <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,,season,]
    total_fin <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,,season,]
    zin <- total_fin + m(flbs_in[[biol_no]])[,year,,season,]
    cin <- (fin / zin) * (1 - exp(-zin)) * n(flbs_in[[biol_no]])[,year,,season,]
    dr <- (discards.n(flfs[[fishery_no]][[catch_no]]) / (discards.n(flfs[[fishery_no]][[catch_no]]) + landings.n(flfs[[fishery_no]][[catch_no]])))[,year,,season,]
    # Catch, Landings, Discards numbers
    test_FLQuant_equal(cin, cout)
    test_FLQuant_equal(cin*(1-dr), lout)
    test_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq_unit))) %in% get_FLQuant_elements(flq_unit, c(1,year,1,season,1,1), c(dim(flq_unit)[1], year, dim(flq_unit)[3], season, 1, dim(flq_unit)[6])))
    expect_equal(c(landings.n(flfs[[fishery_no]][[catch_no]]))[elem], c(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    expect_equal(c(discards.n(flfs[[fishery_no]][[catch_no]]))[elem], c(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    # FC 21
    fishery_no <- 2
    catch_no <- 1
    biol_no <- 2
    lout <- landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dout <- discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    cout <- catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    fin <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,,season,]
    total_fin <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,,season,]
    zin <- total_fin + m(flbs_in[[biol_no]])[,year,,season,]
    cin <- (fin / zin) * (1 - exp(-zin)) * n(flbs_in[[biol_no]])[,year,,season,]
    dr <- (discards.n(flfs[[fishery_no]][[catch_no]]) / (discards.n(flfs[[fishery_no]][[catch_no]]) + landings.n(flfs[[fishery_no]][[catch_no]])))[,year,,season,]
    # Catch, Landings, Discards numbers
    test_FLQuant_equal(cin, cout)
    test_FLQuant_equal(cin*(1-dr), lout)
    test_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq_unit))) %in% get_FLQuant_elements(flq_unit, c(1,year,1,season,1,1), c(dim(flq_unit)[1], year, dim(flq_unit)[3], season, 1, dim(flq_unit)[6])))
    expect_equal(c(landings.n(flfs[[fishery_no]][[catch_no]]))[elem], c(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    expect_equal(c(discards.n(flfs[[fishery_no]][[catch_no]]))[elem], c(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    # FC 22
    fishery_no <- 2
    catch_no <- 2
    lout <- landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dout <- discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    cout <- catch.n(om_out[["fisheries"]][[fishery_no]][[catch_no]])[,year,,season,]
    dr <- (discards.n(flfs[[fishery_no]][[catch_no]]) / (discards.n(flfs[[fishery_no]][[catch_no]]) + landings.n(flfs[[fishery_no]][[catch_no]])))[,year,,season,]
    biol_no <- 3
    fin3 <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,,season,]
    total_fin3 <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,,season,]
    zin3 <- total_fin3 + m(flbs_in[[biol_no]])[,year,,season,]
    cin3 <- (fin3 / zin3) * (1 - exp(-zin3)) * n(flbs_in[[biol_no]])[,year,,season,]
    biol_no <- 4
    fin4 <- test_operatingModel_get_f_FCB(flfs, flbs, fc, fishery_no, catch_no, biol_no)[,year,,season,]
    total_fin4 <- test_operatingModel_get_f_B(flfs, flbs, fc, biol_no)[,year,,season,]
    zin4 <- total_fin4 + m(flbs_in[[biol_no]])[,year,,season,]
    cin4 <- (fin4 / zin4) * (1 - exp(-zin4)) * n(flbs_in[[biol_no]])[,year,,season,]
    cin <- cin3 + cin4
    # Catch, Landings, Discards numbers
    test_FLQuant_equal(cin, cout)
    test_FLQuant_equal(cin*(1-dr), lout)
    test_FLQuant_equal(cin*dr, dout)
    # Check all other timesteps are unchanged
    elem <- !((1:prod(dim(flq))) %in% get_FLQuant_elements(flq, c(1,year,1,season,1,1), c(dim(flq)[1], year, 1, season, 1, dim(flq)[6])))
    expect_equal(c(landings.n(flfs[[fishery_no]][[catch_no]]))[elem], c(landings.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
    expect_equal(c(discards.n(flfs[[fishery_no]][[catch_no]]))[elem], c(discards.n(om_out[["fisheries"]][[fishery_no]][[catch_no]]))[elem])
})

test_that("operatingModel landings, catch and discards methods",{
    # These methods evaluate the current state of the OM
    # i.e. just pull values out, no calculation
    om <- make_test_operatingModel1(10)
    dim_max <- dim(n(om[["biols"]][[1]][["biol"]]))
    dim_min <- round(runif(6, min=1, max=dim_max))
    year <- round(runif(1,min=dim_min[2],max=dim_max[2]))
    season <- round(runif(1,min=dim_min[4],max=dim_max[4]))
    timestep <- (year-1) * dim(n(om[["biols"]][[1]][["biol"]]))[4] + season
    # 1 biol -> 1 catch
    biol_no <- 1
    landings1_out <- test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    landings1_in <- landings(om[["fisheries"]][[1]][[1]])
    test_FLQuant_equal(landings1_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], landings1_out)
    discards1_out <- test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    discards1_in <- discards(om[["fisheries"]][[1]][[1]])
    test_FLQuant_equal(discards1_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], discards1_out)
    catch1_out <- test_operatingModel_catches_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    catch1_in <- catch(om[["fisheries"]][[1]][[1]])
    test_FLQuant_equal(catch1_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], catch1_out)

    landings1_n_out <- test_operatingModel_landings_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    landings1_n_in <- landings.n(om[["fisheries"]][[1]][[1]])
    test_FLQuant_equal(landings1_n_in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], landings1_n_out)
    discards1_n_out <- test_operatingModel_discards_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    discards1_n_in <- discards.n(om[["fisheries"]][[1]][[1]])
    test_FLQuant_equal(discards1_n_in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], discards1_n_out)
    catch1_n_out <- test_operatingModel_catch_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    catch1_n_in <- catch.n(om[["fisheries"]][[1]][[1]])
    test_FLQuant_equal(catch1_n_in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], catch1_n_out)

    # 1 biol -> 2 catch
    biol_no <- 2
    landings2_out <- test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    landings12_in <- landings(om[["fisheries"]][[1]][[2]])
    landings21_in <- landings(om[["fisheries"]][[2]][[1]])
    test_FLQuant_equal((landings12_in+landings21_in)[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], landings2_out)
    discards2_out <- test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    discards12_in <- discards(om[["fisheries"]][[1]][[2]])
    discards21_in <- discards(om[["fisheries"]][[2]][[1]])
    test_FLQuant_equal((discards12_in+discards21_in)[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], discards2_out)
    catch2_out <- test_operatingModel_catches_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    catch12_in <- catch(om[["fisheries"]][[1]][[2]])
    catch21_in <- catch(om[["fisheries"]][[2]][[1]])
    test_FLQuant_equal((catch12_in+catch21_in)[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], catch2_out)

    landings2_n_out <- test_operatingModel_landings_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    landings12_n_in <- landings.n(om[["fisheries"]][[1]][[2]])
    landings21_n_in <- landings.n(om[["fisheries"]][[2]][[1]])
    test_FLQuant_equal((landings12_n_in+landings21_n_in)[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], landings2_n_out)

    discards2_n_out <- test_operatingModel_discards_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    discards12_n_in <- discards.n(om[["fisheries"]][[1]][[2]])
    discards21_n_in <- discards.n(om[["fisheries"]][[2]][[1]])
    test_FLQuant_equal((discards12_n_in+discards21_n_in)[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], discards2_n_out)
    catch2_n_out <- test_operatingModel_catch_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    catch12_n_in <- catch.n(om[["fisheries"]][[1]][[2]])
    catch21_n_in <- catch.n(om[["fisheries"]][[2]][[1]])
    test_FLQuant_equal((catch12_n_in+catch21_n_in)[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], catch2_n_out)

    # 2 biol -> 1 catch
    # Not yet implemented for a single catch so fails
    biol_no <- 3
    expect_error(test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))
    expect_error(test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))
    expect_error(test_operatingModel_catch_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))
    expect_error(test_operatingModel_landings_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max))
    expect_error(test_operatingModel_discards_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max))
    expect_error(test_operatingModel_catch_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max))
    biol_no <- 4
    expect_error(test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))
    expect_error(test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))
    expect_error(test_operatingModel_catch_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1]))
    expect_error(test_operatingModel_landings_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max))
    expect_error(test_operatingModel_discards_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max))
    expect_error(test_operatingModel_catch_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max))
})

# eval_om work with units - important as the target calculations use unit_sum()
test_that("operatingModel eval_om units", {
    # Two fisheries, two biols
    # FC11 -> B1
    # FC12 and FC21 -> B2
    nunits <- 8
    niters <- 10 
    flq <- random_FLQuant_generator(fixed_dims = c(5,20,nunits,4,1,niters))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 2, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
    fwc <- random_fwdControl_generator(niters=1)
    FCB <- array(NA, dim=c(3,3))
    FCB[1,] <- c(1,1,1)
    FCB[2,] <- c(1,2,2)
    FCB[3,] <- c(2,1,2)
    attr(fwc, "FCB") <- FCB
    # Random indices
    dim_max <- dim(n(flbs[[1]][["biol"]]))
    dim_min <- round(runif(6, min=1, max=dim_max - c(1,1,1,1,0,1))) # 1 season so 0
    #  Catch FC11
    cout11 <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", 1, 1, as.integer(NA), dim_min[-1], dim_max[-1])
    cin11 <- unitSums(catch(flfs[[1]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
    test_FLQuant_equal(cout11,cin11)
    #  Catch FC12
    cout12 <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", 1, 2, as.integer(NA), dim_min[-1], dim_max[-1])
    cin12 <- unitSums(catch(flfs[[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
    test_FLQuant_equal(cout12,cin12)
    #  Catch FC21
    cout21 <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", 2, 1, as.integer(NA), dim_min[-1], dim_max[-1])
    cin21 <- unitSums(catch(flfs[[2]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
    test_FLQuant_equal(cout21,cin21)
    #  Catch B1
    coutb1 <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", as.integer(NA),as.integer(NA),1, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(coutb1,cin11)
    #  Catch B2
    coutb2 <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", as.integer(NA),as.integer(NA),2, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(coutb2,cin12+cin21)
    #  Landings FC11
    lout11 <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", 1, 1, as.integer(NA), dim_min[-1], dim_max[-1])
    lin11 <- unitSums(landings(flfs[[1]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
    test_FLQuant_equal(lout11,lin11)
    #  Landings FC12
    lout12 <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", 1, 2, as.integer(NA), dim_min[-1], dim_max[-1])
    lin12 <- unitSums(landings(flfs[[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
    test_FLQuant_equal(lout12,lin12)
    #  Landings FC21
    lout21 <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", 2, 1, as.integer(NA), dim_min[-1], dim_max[-1])
    lin21 <- unitSums(landings(flfs[[2]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
    test_FLQuant_equal(lout21,lin21)
    #  Landings B1
    loutb1 <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", as.integer(NA),as.integer(NA),1, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(loutb1,lin11)
    #  Landings B2
    loutb2 <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", as.integer(NA),as.integer(NA),2, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(loutb2,lin12+lin21)
    #  Discards FC11
    dout11 <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", 1, 1, as.integer(NA), dim_min[-1], dim_max[-1])
    din11 <- unitSums(discards(flfs[[1]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
    test_FLQuant_equal(dout11,din11)
    #  Discards FC12
    dout12 <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", 1, 2, as.integer(NA), dim_min[-1], dim_max[-1])
    din12 <- unitSums(discards(flfs[[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
    test_FLQuant_equal(dout12,din12)
    #  Discards FC21
    dout21 <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", 2, 1, as.integer(NA), dim_min[-1], dim_max[-1])
    din21 <- unitSums(discards(flfs[[2]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
    test_FLQuant_equal(dout21,din21)
    #  Discards B1
    doutb1 <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", as.integer(NA),as.integer(NA),1, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(doutb1,din11)
    #  Discards B2
    doutb2 <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", as.integer(NA),as.integer(NA),2, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(doutb2,din12+din21)
})

test_that("operatingModel eval_om simple", {
    # Based on ple4 simple
    # Add more as target types added
    niters <- 10 
    om <- make_test_operatingModel1(niters)
    dim_max <- dim(n(om[["biols"]][[1]][["biol"]]))
    dim_min <- round(runif(6, min=1, max=dim_max))
    # Catch
    # 1, 1, 1 Cannot ask for biol and a catch
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", 1, 1, 1, dim_min[-1], dim_max[-1]))
    # 1, 2, NA
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", 1, 2, as.integer(NA), dim_min[-1], dim_max[-1])
    cin <- catch(om[["fisheries"]][[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]] 
    test_FLQuant_equal(cout, cin)
    # NA, NA, 2
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", as.integer(NA), as.integer(NA), 2, dim_min[-1], dim_max[-1])
    cin <- test_operatingModel_catches_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(cout, cin)
    # NA, NA, 3 error
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", as.integer(NA), as.integer(NA), 3, dim_min[-1], dim_max[-1]))
    # If catch but no fishery - error
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", as.integer(NA), 1, as.integer(NA), dim_min[-1], dim_max[-1]))
    # Fishery but no catch - error
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", 1, as.integer(NA), as.integer(NA), dim_min[-1], dim_max[-1]))
    # Landings
    # 1, 1, 1 Cannot ask for biol and a catch
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", 1, 1, 1, dim_min[-1], dim_max[-1]))
    # 1, 2, NA
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", 1, 2, as.integer(NA), dim_min[-1], dim_max[-1])
    cin <- landings(om[["fisheries"]][[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]] 
    test_FLQuant_equal(cout, cin)
    # NA, NA, 2
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", as.integer(NA), as.integer(NA), 2, dim_min[-1], dim_max[-1])
    cin <- test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(cout, cin)
    # NA, NA, 3 error
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", as.integer(NA), as.integer(NA), 3, dim_min[-1], dim_max[-1]))
    # Discards
    # 1, 1, 1 Cannot ask for biol and a catch
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", 1, 1, 1, dim_min[-1], dim_max[-1]))
    # 1, 2, NA
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", 1, 2, as.integer(NA), dim_min[-1], dim_max[-1])
    cin <- discards(om[["fisheries"]][[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]] 
    test_FLQuant_equal(cout, cin)
    # NA, NA, 2
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", as.integer(NA), as.integer(NA), 2, dim_min[-1], dim_max[-1])
    cin <- test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, dim_min[-1], dim_max[-1])
    test_FLQuant_equal(cout, cin)
    # NA, NA, 3 error
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", as.integer(NA), as.integer(NA), 3, dim_min[-1], dim_max[-1]))
    # Effort
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "effort", 1, as.integer(NA), as.integer(NA), dim_min[-1], dim_max[-1])
    test_FLQuant_equal(om[["fisheries"]][[1]]@effort[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], cout)
    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "effort", 2, as.integer(NA), as.integer(NA), dim_min[-1], dim_max[-1])
    test_FLQuant_equal(om[["fisheries"]][[2]]@effort[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], cout)
    # Effort but no fishery no specified
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "effort", as.integer(NA), as.integer(NA), 1, dim_min[-1], dim_max[-1]))
    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "effort", as.integer(NA), 1, as.integer(NA), dim_min[-1], dim_max[-1]))
})

# Current values in operating model (asked for by values in control)
test_that("get_target_value_hat", {
    # Two fisheries, two biols, with units
    # FC11 -> B1
    # FC12 and FC21 -> B2
    niters <- 10 
    flq <- random_FLQuant_generator(fixed_dims = c(5,20,2,4,1,niters))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 2, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
    # Fix FCB
    FCB <- array(NA, dim=c(3,3))
    FCB[1,] <- c(1,1,1)
    FCB[2,] <- c(1,2,2)
    FCB[3,] <- c(2,1,2)
    # Make an fwdControl to test
    # Two sim catch targets
    years <- rep(round(runif(4, min=1,max=dim(flq)[2])),each=2)
    seasons <- rep(round(runif(4, min=1,max=dim(flq)[4])),each=2)
    timesteps <- (years-1) * dim(flq)[4] + seasons;
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], timestep = timesteps[1:2],
                        quant = c("catch","catch"), order = 1, 
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2),
                        relFishery = NA, relCatch = NA, relBiol = NA,
                        relYear = NA, relSeason = NA)
    trgt2 <- data.frame(year = years[3:4], season = seasons[3:4], timestep = timesteps[3:4],
                        quant = c("landings","discards"), order = 2, 
                        fishery = c(NA,1), catch = c(NA,2), biol = c(1,NA),
                        relFishery = NA, relCatch = NA, relBiol = NA,
                        relYear = NA, relSeason = NA)
    rel_years <- rep(round(runif(4, min=1,max=dim(flq)[2])),each=2)
    rel_seasons <- rep(round(runif(4, min=1,max=dim(flq)[4])),each=2)
    rel_trgt1 <- data.frame(year = years[5:6], season = seasons[5:6], timestep = timesteps[5:6],
                        quant = c("catch","catch"), order = 3, 
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2),
                        relFishery = c(1,NA), relCatch = c(1,NA), relBiol = c(NA,2),
                        relYear = rel_years[5:6], relSeason = rel_seasons[5:6])
    # Discards relative to different catch and fishery
    rel_trgt2 <- data.frame(year = years[7:8], season = seasons[7:8], timestep = timesteps[7:8],
                        quant = c("landings","discards"), order = 4, 
                        fishery = c(NA,1), catch = c(NA,2), biol = c(1,NA),
                        relFishery = c(NA,2), relCatch = c(NA,1), relBiol = c(1,NA),
                        relYear = rel_years[7:8], relSeason = rel_seasons[7:8])
    fwc <- fwdControl(rbind(trgt1, trgt2, rel_trgt1, rel_trgt2))
    attr(fwc, "FCB") <- FCB
    # Target 1 - 1 sim target at a time
    val_hat1 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 1, 1)
    val_in1 <- c(unitSums(catch(flfs[[1]][[1]])[,years[1],,seasons[1]]))
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 1, 2)
    val_in2 <- c(unitSums((catch(flfs[[1]][[2]]) + catch(flfs[[2]][[1]]))[,years[2],,seasons[2]]))
    expect_equal(val_hat2, val_in2)
    # Both sim targets
    val_hat <- test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_in1,val_in2))
    # Target 2 - 1 sim target at a time
    val_hat1 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 2, 1)
    val_in1 <- c(unitSums(landings(flfs[[1]][[1]])[,years[3],,seasons[3]]))
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 2, 2)
    val_in2 <- c(unitSums(discards(flfs[[1]][[2]])[,years[4],,seasons[4]]))
    expect_equal(val_hat2, val_in2)
    # Both sim targets
    val_hat <- test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 2)
    expect_equal(val_hat, c(val_in1,val_in2))
    # Target 3 - Relative - 1 sim target at a time
    val_hat1 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 3, 1)
    val_in1 <- c(unitSums(catch(flfs[[1]][[1]])[,years[5],,seasons[5]]) / unitSums(catch(flfs[[1]][[1]])[,rel_years[5],,rel_seasons[5]]))
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 3, 2)
    val_in2 <- c(unitSums((catch(flfs[[1]][[2]]) + catch(flfs[[2]][[1]]))[,years[6],,seasons[6]]) / unitSums((catch(flfs[[1]][[2]]) + catch(flfs[[2]][[1]]))[,rel_years[6],,rel_seasons[6]]))
    expect_equal(val_hat2, val_in2)
    # Both sim targets
    val_hat <- test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 3)
    expect_equal(val_hat, c(val_in1,val_in2))
    # Target 4 - Relative - 1 sim target at a time
    val_hat1 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 4, 1)
    val_in1 <- c(unitSums(landings(flfs[[1]][[1]])[,years[7],,seasons[7]]) / unitSums(landings(flfs[[1]][[1]])[,rel_years[7],,rel_seasons[7]]))
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 4, 2)
    val_in2 <- c(unitSums(discards(flfs[[1]][[2]])[,years[8],,seasons[8]]) / unitSums(discards(flfs[[2]][[1]])[,rel_years[8],,rel_seasons[8]]))
    expect_equal(val_hat2, val_in2)
    # Both sim targets
    val_hat <- test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 4)
    expect_equal(val_hat, c(val_in1,val_in2))
    # Relative fails due to not being set properly
    rel_trgt3 <- data.frame(year = 1:8, season = 1, timestep = 1:8,
                        quant = "catch", order = 1:8,
                        fishery = 1, catch = 1, biol = NA,
                        relFishery = c(NA,1,1,1,1,NA,NA,NA), relCatch = c(1,NA,1,1,1,NA,NA,NA), relBiol = c(NA,NA,NA,NA,NA,1,1,NA),
                        relYear = c(1,1,NA,NA,1,1,NA,1), relSeason = c(1,1,NA,1,NA,NA,1,1))
    fwc <- fwdControl(rel_trgt3)
    attr(fwc, "FCB") <- FCB
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 1))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 2)) 
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 3))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 4))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 5))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 6))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 7))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 8))
})

# Values in control object 
test_that("get_target_value - straight value", {
    niters <- 10 
    flq <- random_FLQuant_generator(fixed_dims = c(5,20,3,4,1,niters))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 2, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
    # Fix FCB
    FCB <- array(NA, dim=c(3,3))
    FCB[1,] <- c(1,1,1)
    FCB[2,] <- c(1,2,2)
    FCB[3,] <- c(2,1,2)
    # Make an fwdControl to test
    # Two sim catch targets
    years <- sort(rep(round(runif(4, min=1,max=dim(flq)[2])),each=2))
    seasons <- sort(rep(round(runif(4, min=1,max=dim(flq)[4])),each=2))
    timesteps <- sort((years-1) * dim(flq)[4] + seasons)
    value <- abs(rnorm(4))
    # 1 iter in control, many in OM - should blow up control iters internally
    # Simple control object
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], timestep = timesteps[1:2],
                        quant = c("catch","catch"), order = 1, value = value[1:2],
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2))
    trgt2 <- data.frame(year = years[3:4], season = seasons[3:4], timestep = timesteps[3:4],
                        quant = c("landings","discards"), order = 2, value = value[3:4],
                        fishery = c(NA,1), catch = c(NA,2), biol = c(1,NA))
    fwc <- fwdControl(rbind(trgt1,trgt2))
    attr(fwc, "FCB") <- FCB
    # No iters in control - but iters in OM
    # Hack - why are values in some weird order
    fwc@iters[,"value",] <- value
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    val_in1 <- rep(fwc@iters[1,"value",],niters)
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    val_in2 <- rep(fwc@iters[2,"value",],niters)
    expect_equal(val_hat2, val_in2)
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_in1, val_in2))
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 2, 1)
    val_in1 <- rep(fwc@iters[3,"value",],niters)
    expect_equal(val_hat1, val_in1)
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 2, 2)
    val_in2 <- rep(fwc@iters[4,"value",],niters)
    expect_equal(val_hat2, val_in2)
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 2)
    expect_equal(val_hat, c(val_in1, val_in2))

    # Same number of iters in iters and OM
    fwc <- fwdControl(rbind(trgt1, trgt2), niters)
    fwc@iters[,"value",] <- abs(rnorm(4*niters))
    attr(fwc, "FCB") <- FCB
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    val_in1 <- fwc@iters[1,"value",]
    expect_equal(val_hat1, unname(val_in1))
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    val_in2 <- fwc@iters[2,"value",]
    expect_equal(val_hat2, unname(val_in2))
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, unname(c(val_in1, val_in2)))
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 2, 1)
    val_in1 <- fwc@iters[3,"value",]
    expect_equal(val_hat1, unname(val_in1))
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 2, 2)
    val_in2 <- fwc@iters[4,"value",]
    expect_equal(val_hat2, unname(val_in2))
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 2)
    expect_equal(val_hat, unname(c(val_in1, val_in2)))
    # Too few iters in control - should fail
    fwc <- fwdControl(rbind(trgt1, trgt2), iters=niters-1)
    fwc@iters[,"value",] <- abs(rnorm(4*(niters-1)))
    attr(fwc, "FCB") <- FCB
    expect_error(test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1))

    # Many iters in control object, 1 in OM
    # Just throws error
    niters <- 1 
    flq <- random_FLQuant_generator(fixed_dims = c(5,20,1,4,1,niters))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 2, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
    # Make an fwdControl to test
    # Two sim catch targets
    years <- rep(round(runif(4, min=1,max=dim(flq)[2])),each=2)
    seasons <- rep(round(runif(4, min=1,max=dim(flq)[4])),each=2)
    timesteps <- (years-1) * dim(flq)[4] + seasons;
    niters <- 100
    fwc <- fwdControl(rbind(trgt1, trgt2), niters)
    fwc@iters[,"value",] <- abs(rnorm(4*niters))
    attr(fwc, "FCB") <- FCB
    expect_error(test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1))
})

test_that("get_target_value - min / max values", {
    # Min / Max tests
    niters <- 10 
    flq <- random_FLQuant_generator(fixed_dims = c(5,20,6,4,1,niters))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 2, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
    # Fix FCB
    FCB <- array(NA, dim=c(3,3))
    FCB[1,] <- c(1,1,1)
    FCB[2,] <- c(1,2,2)
    FCB[3,] <- c(2,1,2)
    # Make an fwdControl to test
    # Two sim catch targets
    years <- rep(round(runif(4, min=1,max=dim(flq)[2])),each=2)
    seasons <- rep(round(runif(4, min=1,max=dim(flq)[4])),each=2)
    timesteps <- (years-1) * dim(flq)[4] + seasons;
    value <- abs(rnorm(4))
    # Simple control object - just catch
    # Which iters will be less (max) / greater (min) than actual catch
    big_iters <- sort(sample(1:niters, 5)) # Bigger than actual catch - will be limited by min
    small_iters <- (1:niters)[!((1:niters) %in% big_iters)] # Smaller than actual catch - will be limited by max
    catch1 <- c(unitSums(catch(flfs[[1]][[1]])[,years[1],,seasons[[1]]])) # The actual catch
    ctrl_catch1 <- catch1
    ctrl_catch1[small_iters] <- ctrl_catch1[small_iters] * 0.9
    ctrl_catch1[big_iters] <- ctrl_catch1[big_iters] * 1.1
    catch2 <- c(unitSums((catch(flfs[[1]][[2]]) + catch(flfs[[2]][[1]]))[,years[1],,seasons[[1]]])) # Actual catch on biol 2
    ctrl_catch2 <- catch2
    ctrl_catch2[small_iters] <- ctrl_catch2[small_iters] * 0.9
    ctrl_catch2[big_iters] <- ctrl_catch2[big_iters] * 1.1

    # Max only - small iters will be limited
    # Same iters in OM and control
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], timestep = timesteps[1:2],
                        quant = c("catch","catch"), order = 1,
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2))
    # fwdControl constructor fix
    fwc <- fwdControl(trgt1, niters)
    fwc@iters[1,"max",] <- ctrl_catch1
    fwc@iters[2,"max",] <- ctrl_catch2
    attr(fwc, "FCB") <- FCB
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    # only the small iters should equal values in control
    expect_equal(val_hat1[small_iters], unname(fwc@iters[1,"max",small_iters]))
    # Others same as in OM
    expect_equal(val_hat1[big_iters], catch1[big_iters])
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    expect_equal(val_hat2[small_iters], unname(fwc@iters[2,"max",small_iters]))
    expect_equal(val_hat2[big_iters], catch2[big_iters])
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_hat1, val_hat2))

    # Max only - iters in OM, only 1 in control
    # Set max target so half of them are maxed out
    fwc <- fwdControl(trgt1, 1)
    max1 <- median(catch1)
    max2 <- median(catch2)
    fwc@iters[1,"max",] <- max1
    fwc@iters[2,"max",] <- max2
    attr(fwc, "FCB") <- FCB
    maxed_out1 <- which(catch1 > max1)
    not_maxed_out1 <- (1:niters)[!((1:niters) %in% maxed_out1)]
    maxed_out2 <- which(catch2 > max2)
    not_maxed_out2 <- (1:niters)[!((1:niters) %in% maxed_out2)]
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    expect_identical(val_hat1[maxed_out1], rep(max1, length(maxed_out1)))
    expect_equal(val_hat1[not_maxed_out1], catch1[not_maxed_out1])
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    expect_identical(val_hat2[maxed_out2], rep(max2, length(maxed_out2)))
    expect_equal(val_hat2[not_maxed_out2], catch2[not_maxed_out2])

    # Min only - iters in OM = iters in control
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], timestep = timesteps[1:2],
                        quant = c("catch","catch"), order = 1,
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2))
    fwc <- fwdControl(trgt1, niters)
    fwc@iters[1,"min",] <- ctrl_catch1
    fwc@iters[2,"min",] <- ctrl_catch2
    attr(fwc, "FCB") <- FCB
    # Big iters will be limited
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    expect_equal(val_hat1[big_iters], unname(fwc@iters[1,"min",big_iters]))
    # Others same as in OM
    expect_equal(val_hat1[small_iters], catch1[small_iters])
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    expect_equal(val_hat2[big_iters], unname(fwc@iters[2,"min",big_iters]))
    expect_equal(val_hat2[small_iters], catch2[small_iters])
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_hat1, val_hat2))

    # Min only - iters in OM, only 1 in control
    fwc <- fwdControl(trgt1, 1)
    min1 <- median(catch1)
    min2 <- median(catch2)
    fwc@iters[1,"min",] <- min1
    fwc@iters[2,"min",] <- min2
    attr(fwc, "FCB") <- FCB
    mined_out1 <- which(catch1 < min1)
    not_mined_out1 <- (1:niters)[!((1:niters) %in% mined_out1)]
    mined_out2 <- which(catch2 < min2)
    not_mined_out2 <- (1:niters)[!((1:niters) %in% mined_out2)]
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    expect_identical(val_hat1[mined_out1], rep(min1, length(mined_out1)))
    expect_equal(val_hat1[not_mined_out1], catch1[not_mined_out1])
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    expect_identical(val_hat2[mined_out2], rep(min2, length(mined_out2)))
    expect_equal(val_hat2[not_mined_out2], catch2[not_mined_out2])

    # Min and Max - together
    # 3 max limits, 3 mins, 4 within
    # Equal iters in control and OM
    min_limit_iters <- sort(sample(1:niters, 3))
    max_limit_iters <- sort(sample((1:niters)[!((1:niters) %in% min_limit_iters)], 3))
    not_limit_iters <- (1:niters)[!((1:niters) %in% c(min_limit_iters, max_limit_iters))]
    not_min_limit_iters <- (1:niters)[!((1:niters) %in% min_limit_iters)]
    not_max_limit_iters <- (1:niters)[!((1:niters) %in% max_limit_iters)]
    catch1 <- c(unitSums(catch(flfs[[1]][[1]])[,years[1],,seasons[[1]]]))
    max_catch1 <- catch1
    max_catch1[max_limit_iters] <- max_catch1[max_limit_iters] * 0.9
    max_catch1[not_max_limit_iters] <- max_catch1[not_max_limit_iters] * 1.1
    min_catch1 <- catch1
    min_catch1[not_min_limit_iters] <- min_catch1[not_min_limit_iters] * 0.9
    min_catch1[min_limit_iters] <- min_catch1[min_limit_iters] * 1.1
    catch2 <- c(unitSums(catch(flfs[[1]][[2]])[,years[1],,seasons[[1]]] + catch(flfs[[2]][[1]])[,years[1],,seasons[[1]]]))
    max_catch2 <- catch2
    max_catch2[max_limit_iters] <- max_catch2[max_limit_iters] * 0.9
    max_catch2[not_max_limit_iters] <- max_catch2[not_max_limit_iters] * 1.1
    min_catch2 <- catch2
    min_catch2[not_min_limit_iters] <- min_catch2[not_min_limit_iters] * 0.9
    min_catch2[min_limit_iters] <- min_catch2[min_limit_iters] * 1.1
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], timestep = timesteps[1:2],
                        quant = c("catch","catch"), order = 1, 
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2))
    fwc <- fwdControl(trgt1, niters)
    fwc@iters[1,"max",] <- max_catch1
    fwc@iters[1,"min",] <- min_catch1
    fwc@iters[2,"max",] <- max_catch2
    fwc@iters[2,"min",] <- min_catch2
    attr(fwc, "FCB") <- FCB
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    expect_identical(val_hat1[max_limit_iters], unname(fwc@iters[1,"max", max_limit_iters]))
    expect_identical(val_hat1[min_limit_iters], unname(fwc@iters[1,"min", min_limit_iters]))
    expect_equal(val_hat1[not_limit_iters], catch1[not_limit_iters])
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    expect_identical(val_hat2[max_limit_iters], unname(fwc@iters[2,"max", max_limit_iters]))
    expect_identical(val_hat2[min_limit_iters], unname(fwc@iters[2,"min", min_limit_iters]))
    expect_equal(val_hat2[not_limit_iters], catch2[not_limit_iters])
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_hat1, val_hat2))

    # Min and Max together with 1 iter in control
    # Set min and max limit so a few of them are maxed / mined out
    min1 <- quantile(catch1, 0.25)
    max1 <- quantile(catch1, 0.75)
    min_limit_iters1 <- which(catch1 < min1)
    max_limit_iters1 <- which(catch1 > max1)
    not_limit_iters1 <- (1:niters)[!((1:niters) %in% c(min_limit_iters1, max_limit_iters1))]
    min2 <- quantile(catch2, 0.25)
    max2 <- quantile(catch2, 0.75)
    min_limit_iters2 <- which(catch2 < min2)
    max_limit_iters2 <- which(catch2 > max2)
    not_limit_iters2 <- (1:niters)[!((1:niters) %in% c(min_limit_iters2, max_limit_iters2))]
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], timestep = timesteps[1:2],
                        quant = c("catch","catch"), order= 1, 
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2))

    fwc <- fwdControl(trgt1, 1)
    fwc@iters[1,"max",] <- max1
    fwc@iters[1,"min",] <- min1
    fwc@iters[2,"max",] <- max2
    fwc@iters[2,"min",] <- min2
    attr(fwc, "FCB") <- FCB
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    expect_equal(val_hat1[not_limit_iters1], catch1[not_limit_iters1])
    expect_equal(val_hat1[max_limit_iters1], unname(rep(max1, length(max_limit_iters1))))
    expect_equal(val_hat1[min_limit_iters1], unname(rep(min1, length(min_limit_iters1))))
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    expect_equal(val_hat2[not_limit_iters2], catch2[not_limit_iters2])
    expect_equal(val_hat2[max_limit_iters2], unname(rep(max2, length(max_limit_iters2))))
    expect_equal(val_hat2[min_limit_iters2], unname(rep(min2, length(min_limit_iters2))))
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_hat1, val_hat2))
})

#test_that("operatingModel get_target_age_range", {
#    min_age_name <- round(runif(1,min=0,max=10))
#    flq <- random_FLQuant_generator(min_dims=c(5,5,1,1,1,1), min_age_name = min_age_name)
#    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 2, fixed_dims = dim(flq), min_age_name=min_age_name)
#    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2, min_age_name=min_age_name)
#    ages <- as.numeric(dimnames(flq)$age)
#    min_age <- round(runif(1,min=ages[1],max=max(ages)))
#    max_age <- round(runif(1,min=min_age, max=max(ages)))
#    # With biol no
#    trgt1 <- data.frame(year = 1, season = 1, timestep = 1,
#                        quant = "f", target = 1, value=1,
#                        fishery = NA, catch = NA, biol = 1,
#                        relFishery = NA, relCatch = NA, relBiol = NA,
#                        relYear = NA, relSeason = NA,
#                        minAge = min_age, maxAge = max_age)
#    fwc <- fwdControl(trgt1)
#    FCB <- array(1, dim=c(3,3))
#    attr(fwc, "FCB") <- FCB
#    ind_out <- test_operatingModel_get_target_age_range_indices(flfs, flbs, fwc, 1, 1) 
#    ind_in <- c(which(ages %in% min_age), which(ages %in% max_age)) - 1
#    expect_identical(ind_out, ind_in)
#    # With catch no
#    trgt1 <- data.frame(year = 1, season = 1, timestep = 1,
#                        quant = "f", target = 1, value=1,
#                        fishery = 1, catch = 1, biol = NA,
#                        relFishery = NA, relCatch = NA, relBiol = NA,
#                        relYear = NA, relSeason = NA,
#                        minAge = min_age, maxAge = max_age)
#    fwc <- fwdControl(trgt1)
#    FCB <- array(1, dim=c(3,3))
#    attr(fwc@target, "FCB") <- FCB
#    ind_out <- test_operatingModel_get_target_age_range_indices(flfs, flbs, fwc, 1, 1) 
#    ind_in <- c(which(ages %in% min_age), which(ages %in% max_age)) - 1
#    expect_identical(ind_out, ind_in)
#    # With neither
#    trgt1 <- data.frame(year = 1, season = 1, timestep = 1,
#                        quant = "f", target = 1, value=1,
#                        fishery = NA, catch = NA, biol = NA,
#                        relFishery = NA, relCatch = NA, relBiol = NA,
#                        relYear = NA, relSeason = NA,
#                        minAge = min_age, maxAge = max_age)
#    fwc <- fwdControl(trgt1)
#    FCB <- array(1, dim=c(3,3))
#    attr(fwc@target, "FCB") <- FCB
#    expect_error(test_operatingModel_get_target_age_range_indices(flfs, flbs, fwc, 1, 1))
#})
