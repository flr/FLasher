# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Test operating model")
source("expect_funs.R")

test_that("operatingModel constructors",{
    # Empty constructor - just check it doesn't fail
    test_operatingModel_empty_constructor()
    # Main constructor test
    # Set up parameters for full test 
    flq <- random_FLQuant_generator()
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    # Pull out just FLBiols for comparison
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    # Test as and wrap
    out <- test_operatingModel_full_constructor(flfs, flbs, fc)
    expect_FLBiolcpps_equal(out[["biols"]], flbs_in)
    expect_FLFisheries_equal(out[["fisheries"]], flfs)
    expect_fwdControl_equal(out[["ctrl"]], fc)
})

test_that("operatingModel constructor dimension checks",{
    flq <- random_FLQuant_generator(min_dims=c(2,2,2,2,2,2))
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
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
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

# Test partial F method with random Biols and Fisheries
# No check if FC catches B
test_that("operatingModel get_f method for FCB with random OM objects - just partial F - one catch on one biol",{
    flq <- random_FLQuant_generator(min_dims = c(2,2,2,2,2,2))
    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 5, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    # fwdControl and FCB needed for constructor but not actually used to test F
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    fc@FCB <- array(0, dim = c(10,3))
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
    # Uses the FCB slot
    data(ple4)
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    om <- make_test_operatingModel(ple4, FCB, nseasons = 1, recruitment_seasons = 1, recruitment_age = 1, niters = 20, sd = 0.1)
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
            expect_FLQuant_equal(fin, fout_all)
            expect_FLQuant_equal(fin[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2],indices_min[3]:indices_max[3],indices_min[4]:indices_max[4],indices_min[5]:indices_max[5],indices_min[6]:indices_max[6]], fout)
        }
    }
})

test_that("operatingModel unit_f and unit_z methods", {
    data(ple4)
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    nseasons <- 4
    niters <- 10
    rec_seasons <- c(1,3)
    om <- make_test_operatingModel(ple4, FCB, recruitment_seasons = rec_seasons, nseasons = nseasons, niters=niters)
    flfs <- om[["fisheries"]]
    flbs <- om[["biols"]]
    fwc <- om[["fwc"]]
    dim <- dim(n(om[["biols"]][[1]][["biol"]]))
    year <- round(runif(1, min=1, max=dim[2]))
    season <- round(runif(1, min=1, max=dim[4]))
    min_age <- round(runif(1,min=1,max=dim[1]/2))
    max_age <- round(runif(1,min=min_age+1,max=dim[1]))
    indices_min <- c(min_age, year, 1, season, 1, 1)
    indices_max <- c(max_age, year, dim[3], season, 1, dim[6])

    # ** unit_z (B) - possible for all Biols as only needs F on each B
    # Not actually used anymore!
    for (biol in 1:4){
        unit_survivors <- unitSums(test_operatingModel_survivors(flfs, flbs, fwc, biol, indices_min, indices_max))
        unit_nstart <- unitSums(n(flbs[[biol]][["biol"]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]])
        zin <- log(unit_nstart / unit_survivors)
        zout <- test_operatingModel_nunit_z_subset(flfs, flbs, fwc, biol, indices_min, indices_max)
        expect_FLQuant_equal(zin, zout)
    }

    ## nunit_f (B)
    # Biol 1
    biol <- 1
    fout <- test_operatingModel_nunit_f_B_subset(flfs, flbs, fwc, biol, indices_min, indices_max)
    cin <- unitSums(catch.n(flfs[[1]][[1]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]])
    survivors <- unitSums(test_operatingModel_survivors(flfs, flbs, fwc, biol, indices_min, indices_max))
    nstart <- unitSums(n(flbs[[biol]][["biol"]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]])
    fin <- (cin * log(nstart / survivors)) / (nstart - survivors)
    expect_FLQuant_equal(fin, fout)
    # Biol 2
    biol <- 2
    fout <- test_operatingModel_nunit_f_B_subset(flfs, flbs, fwc, biol, indices_min, indices_max)
    cin <- unitSums(catch.n(flfs[[1]][[2]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]] + catch.n(flfs[[2]][[1]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]])
    survivors <- unitSums(test_operatingModel_survivors(flfs, flbs, fwc, biol, indices_min, indices_max))
    nstart <- unitSums(n(flbs[[biol]][["biol"]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]])
    fin <- (cin * log(nstart / survivors)) / (nstart - survivors)
    expect_FLQuant_equal(fin, fout)
    # Biol 3 + 4
    # But for biols 3 and 4, we do not have Cin - only total catch of the Catch - not calculated catch
    expect_error(test_operatingModel_nunit_f_B_subset(flfs, flbs, fwc, 3, indices_min, indices_max))
    expect_error(test_operatingModel_nunit_f_B_subset(flfs, flbs, fwc, 4, indices_min, indices_max))
                         
    ## unit_f (FCB) - this only works correctly if all of the catch in the FLCatch comes from a single Biol
    # Not yet implemented so only errors expected
    # F1C1B1
    fout <- test_operatingModel_nunit_f_FCB_subset(flfs, flbs, fwc, 1,1,1, indices_min, indices_max) 
    cin <- unitSums(catch.n(flfs[[1]][[1]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]])
    survivors <- unitSums(test_operatingModel_survivors(flfs, flbs, fwc, 1, indices_min, indices_max))
    nstart <- unitSums(n(flbs[[1]][["biol"]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]])
    fin <- (cin * log(nstart / survivors)) / (nstart - survivors)
    expect_FLQuant_equal(fin, fout)
    # F1C2B2
    fout <- test_operatingModel_nunit_f_FCB_subset(flfs, flbs, fwc, 1,2,2, indices_min, indices_max) 
    cin <- unitSums(catch.n(flfs[[1]][[2]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]])
    survivors <- unitSums(test_operatingModel_survivors(flfs, flbs, fwc, 2, indices_min, indices_max))
    nstart <- unitSums(n(flbs[[2]][["biol"]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]])
    fin <- (cin * log(nstart / survivors)) / (nstart - survivors)
    expect_FLQuant_equal(fin, fout)
    # F2C1B2
    fout <- test_operatingModel_nunit_f_FCB_subset(flfs, flbs, fwc, 2,1,2, indices_min, indices_max) 
    cin <- unitSums(catch.n(flfs[[2]][[1]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]])
    survivors <- unitSums(test_operatingModel_survivors(flfs, flbs, fwc, 2, indices_min, indices_max))
    nstart <- unitSums(n(flbs[[2]][["biol"]])[indices_min[1]:indices_max[1], indices_min[2]:indices_max[2], indices_min[3]:indices_max[3], indices_min[4]:indices_max[4], indices_min[5]:indices_max[5], indices_min[6]:indices_max[6]])
    fin <- (cin * log(nstart / survivors)) / (nstart - survivors)
    expect_FLQuant_equal(fin, fout)
    # F2C2B3 and F2C2B4
    expect_error(test_operatingModel_nunit_f_FCB_subset(flfs, flbs, fwc, 2,2,3, indices_min, indices_max))
    expect_error(test_operatingModel_nunit_f_FCB_subset(flfs, flbs, fwc, 2,2,4, indices_min, indices_max))
})

test_that("operatingModel fbar methods",{
    # Gets complicated as depends on units
    data(ple4)
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    nseasons <- 4
    niters <- 10
    rec_seasons <- c(1,3)
    om <- make_test_operatingModel(ple4, FCB, recruitment_seasons = rec_seasons, nseasons = nseasons, niters=niters)
    flfs <- om[["fisheries"]]
    flbs <- om[["biols"]]
    fwc <- om[["fwc"]]
    dim <- dim(n(om[["biols"]][[1]][["biol"]]))
    year <- round(runif(1, min=1, max=dim[2]))
    season <- round(runif(1, min=1, max=dim[4]))
    unit <- round(runif(1, min=1, max=dim[3]))
    min_age <- round(runif(1,min=1,max=dim[1]/2))
    max_age <- round(runif(1,min=min_age+1,max=dim[1]))
    indices_min <- c(min_age, year, 1, season, 1, 1)
    indices_max <- c(max_age, year, dim[3], season, 1, dim[6])
    indices_min_u1 <- c(min_age, year, unit, season, 1, 1)
    indices_max_u1 <- c(max_age, year, unit, season, 1, dim[6])

    # Single units
    # Just biols
    for (biol in 1:4){
        fbout <- test_operatingModel_fbar_B(flfs, flbs, fwc, biol, indices_min_u1, indices_max_u1)
        fbin <- apply(test_operatingModel_get_f_B_subset(flfs, flbs, fwc, biol, indices_min_u1, indices_max_u1), 2:6, mean)
        expect_FLQuant_equal(fbout, fbin)
    }
    # FCB
    # F1C1B1
    fbout <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 1, 1, 1, indices_min_u1, indices_max_u1)
    fbin <- apply(test_operatingModel_get_f_FCB_subset(flfs, flbs, fwc, 1, 1, 1, indices_min_u1, indices_max_u1), 2:6, mean)
    expect_FLQuant_equal(fbout, fbin)
    # F1C2B2
    fbout <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 1, 2, 2, indices_min_u1, indices_max_u1)
    fbin <- apply(test_operatingModel_get_f_FCB_subset(flfs, flbs, fwc, 1, 2, 2, indices_min_u1, indices_max_u1), 2:6, mean)
    expect_FLQuant_equal(fbout, fbin)
    # F2C1B2
    fbout <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 2, 1, 2, indices_min_u1, indices_max_u1)
    fbin <- apply(test_operatingModel_get_f_FCB_subset(flfs, flbs, fwc, 2, 1, 2, indices_min_u1, indices_max_u1), 2:6, mean)
    expect_FLQuant_equal(fbout, fbin)
    # F2C2B3
    fbout <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 2, 2, 3, indices_min_u1, indices_max_u1)
    fbin <- apply(test_operatingModel_get_f_FCB_subset(flfs, flbs, fwc, 2, 2, 3, indices_min_u1, indices_max_u1), 2:6, mean)
    expect_FLQuant_equal(fbout, fbin)
    # F2C2B4
    fbout <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 2, 2, 4, indices_min_u1, indices_max_u1)
    fbin <- apply(test_operatingModel_get_f_FCB_subset(flfs, flbs, fwc, 2, 2, 4, indices_min_u1, indices_max_u1), 2:6, mean)
    expect_FLQuant_equal(fbout, fbin)

    # Multiple units
    # B1
    fbout <- test_operatingModel_fbar_B(flfs, flbs, fwc, 1, indices_min, indices_max)
    fbin <- apply(test_operatingModel_nunit_f_B_subset(flfs, flbs, fwc, 1, indices_min, indices_max), 2:6, mean)
    expect_FLQuant_equal(fbout, fbin)
    # B2
    fbout <- test_operatingModel_fbar_B(flfs, flbs, fwc, 2, indices_min, indices_max)
    fbin <- apply(test_operatingModel_nunit_f_B_subset(flfs, flbs, fwc, 2, indices_min, indices_max), 2:6, mean)
    expect_FLQuant_equal(fbout, fbin)
    # B3 and B4
    expect_error(test_operatingModel_fbar_B(flfs, flbs, fwc, 3, indices_min, indices_max))
    expect_error(test_operatingModel_fbar_B(flfs, flbs, fwc, 4, indices_min, indices_max))
    # FCB
    # F1C1B1
    fbout <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 1, 1, 1, indices_min, indices_max)
    fbin <- apply(test_operatingModel_nunit_f_FCB_subset(flfs, flbs, fwc, 1, 1, 1, indices_min, indices_max), 2:6, mean)
    expect_FLQuant_equal(fbout, fbin)
    # F1C2B2
    fbout <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 1, 2, 2, indices_min, indices_max)
    fbin <- apply(test_operatingModel_nunit_f_FCB_subset(flfs, flbs, fwc, 1, 2, 2, indices_min, indices_max), 2:6, mean)
    expect_FLQuant_equal(fbout, fbin)
    # F2C1B2
    fbout <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 2, 1, 2, indices_min, indices_max)
    fbin <- apply(test_operatingModel_nunit_f_FCB_subset(flfs, flbs, fwc, 2, 1, 2, indices_min, indices_max), 2:6, mean)
    expect_FLQuant_equal(fbout, fbin)
    # F2C2B3 and F2C2B4
    expect_error(test_operatingModel_fbar_FCB(flfs, flbs, fwc, 2, 2, 3, indices_min, indices_max))
    expect_error(test_operatingModel_fbar_FCB(flfs, flbs, fwc, 2, 2, 4, indices_min, indices_max))
})

test_that("operatingModel ssb and biomass target methods",{
    FCB <- array(c(1,1,1), dim=c(1,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    # Seasonal model
    min_age <- 1
    niters <- 20
    om <- make_test_operatingModel(ple4, FCB, nseasons = 4, recruitment_seasons = c(1,3,4), recruitment_age = min_age, niters = niters, sd = 0.1)
    flfs <- om[["fisheries"]]
    biols <- om[["biols"]]
    biols[[1]][["biol"]]@spwn[,,,1] <- 0.5
    biols[[1]][["biol"]]@spwn[,,,3] <- 0.5
    fwc <- om[["fwc"]]
    dms <- dim(n(biols[[1]][["biol"]]))
    year <- round(runif(1,min=1, max= dms[2]-1))

    ## SSB and Biomass at start of timestep
    # Makes no sense
    #season <- 1
    #indices_min <- c(year,1,season,1,1)
    #indices_max <- c(year,dms[3],season,dms[5],dms[6])
    #indices_min6 <- c(1, indices_min)
    #indices_max6 <- c(dms[1], indices_max)
    #ssb <- quantSums((n(biols[[1]][["biol"]]) * wt(biols[[1]][["biol"]]) * mat(biols[[1]][["biol"]]))[,year,,season,])
    #ssb_out <-  test_operatingModel_ssb_start(flfs, biols, fwc, 1, indices_min, indices_max)
    #expect_FLQuant_equal(ssb, ssb_out)
    #biomass <- quantSums((wt(biols[[1]][["biol"]]) * n(biols[[1]][["biol"]]))[,year,,season,])
    #biomass_out <-  test_operatingModel_biomass_start(flfs, biols, fwc, 1, indices_min, indices_max)
    #expect_FLQuant_equal(biomass, biomass_out)
    ## Indices too long
    #bad_indices_min <- c(1,indices_min)
    #bad_indices_max <- c(1,indices_max)
    #expect_error(test_operatingModel_ssb_start(flfs, biols, fwc, 1, bad_indices_min, indices_max))
    #expect_error(test_operatingModel_ssb_start(flfs, biols, fwc, 1, indices_min, bad_indices_max))
    #expect_error(test_operatingModel_biomass_start(flfs, biols, fwc, 1, bad_indices_min, indices_max))
    #expect_error(test_operatingModel_biomass_start(flfs, biols, fwc, 1, indices_min, bad_indices_max))

    # SSB and Biomass at end of timestep
    season <- 1
    indices_min <- c(year,1,season,1,1)
    indices_max <- c(year,dms[3],season,dms[5],dms[6])
    indices_min6 <- c(1, indices_min)
    indices_max6 <- c(dms[1], indices_max)
    survivors <- test_operatingModel_survivors(flfs, biols, fwc, 1, indices_min6, indices_max6)
    ssb <- quantSums(survivors * (wt(biols[[1]][["biol"]]) * mat(biols[[1]][["biol"]]))[,year,,season,])
    ssb_out <-  test_operatingModel_ssb_end(flfs, biols, fwc, 1, indices_min, indices_max)
    expect_FLQuant_equal(ssb, ssb_out)
    biomass <- quantSums(survivors * wt(biols[[1]][["biol"]])[,year,,season,])
    biomass_out <-  test_operatingModel_biomass_end(flfs, biols, fwc, 1, indices_min, indices_max)
    expect_FLQuant_equal(biomass, biomass_out)
    # Indices too long
    bad_indices_min <- c(1,indices_min)
    bad_indices_max <- c(1,indices_max)
    expect_error(test_operatingModel_ssb_end(flfs, biols, fwc, 1, bad_indices_min, indices_max))
    expect_error(test_operatingModel_ssb_end(flfs, biols, fwc, 1, indices_min, bad_indices_max))
    expect_error(test_operatingModel_biomass_end(flfs, biols, fwc, 1, bad_indices_min, indices_max))
    expect_error(test_operatingModel_biomass_end(flfs, biols, fwc, 1, indices_min, bad_indices_max))

    # SSB and Biomass at time of spawning
    season <- 1
    indices_min <- c(year,1,season,1,1)
    indices_max <- c(year,dms[3],season,dms[5],dms[6])
    indices_min6 <- c(1, indices_min)
    indices_max6 <- c(dms[1], indices_max)
    exp_z_pre_spwn <- test_operatingModel_get_exp_z_pre_spwn(flfs, biols, fwc, 1, indices_min6, indices_max6)
    ssb_in <- quantSums(exp_z_pre_spwn * (n(biols[[1]][["biol"]]) * wt(biols[[1]][["biol"]]) * mat(biols[[1]][["biol"]]))[,year,,season,])
    ssb_out <-  test_operatingModel_ssb_spawn(flfs, biols, fwc, 1, indices_min, indices_max)
    expect_FLQuant_equal(ssb_in, ssb_out)
    biomass_in <- quantSums(exp_z_pre_spwn * (n(biols[[1]][["biol"]]) * wt(biols[[1]][["biol"]]))[,year,,season,])
    biomass_out <-  test_operatingModel_biomass_spawn(flfs, biols, fwc, 1, indices_min, indices_max)
    expect_FLQuant_equal(biomass_in, biomass_out)
    season <- 2 # no spawning - should throw error
    indices_min <- c(year,1,season,1,1)
    indices_max <- c(year,dms[3],season,dms[5],dms[6])
    indices_min6 <- c(1, indices_min)
    indices_max6 <- c(dms[1], indices_max)
    expect_error(test_operatingModel_ssb_spawn(flfs, biols, fwc, 1, indices_min, indices_max))
    expect_error(test_operatingModel_biomass_spawn(flfs, biols, fwc, 1, indices_min, indices_max))

    # SSB and Biomass FLash style
    # * Season 1
    season <- 1
    indices_min <- c(year,1,season,1,1)
    indices_max <- c(year,dms[3],season,dms[5],dms[6])
    indices_min6 <- c(1, indices_min)
    indices_max6 <- c(dms[1], indices_max)
    # If F before spawning, get SSB at time of spawning in current timestep
    flfs[[1]]@hperiod[1,] <- 0.3
    ssb_out <-  test_operatingModel_ssb_flash(flfs, biols, fwc, 1, indices_min, indices_max)
    ssb_in <-  test_operatingModel_ssb_spawn(flfs, biols, fwc, 1, indices_min, indices_max)
    expect_FLQuant_equal(ssb_in, ssb_out)
    biomass_out <-  test_operatingModel_biomass_flash(flfs, biols, fwc, 1, indices_min, indices_max)
    biomass_in <-  test_operatingModel_biomass_spawn(flfs, biols, fwc, 1, indices_min, indices_max)
    expect_FLQuant_equal(biomass_in, biomass_out)
    # If F after spawning, get SSB at time of spawning in following timestep
    # But no spawning in following timestep - so what happens...
    # Not solvable in a projection - throws an error
    flfs[[1]]@hperiod[1,] <- 0.9
    expect_error(test_operatingModel_ssb_flash(flfs, biols, fwc, 1, indices_min, indices_max))
    expect_error(test_operatingModel_biomass_flash(flfs, biols, fwc, 1, indices_min, indices_max))

    # * Season 2 - no spawning
    # No matter what hperiod is, as no spawning in this season, you have to get SSB at time of spawning in next timestep
    # And if it does not spawn in next time period - error
    season <- 2
    indices_min <- c(year,1,season,1,1)
    indices_max <- c(year,dms[3],season,dms[5],dms[6])
    indices_min6 <- c(1, indices_min)
    indices_max6 <- c(dms[1], indices_max)
    ssb_out <-  test_operatingModel_ssb_flash(flfs, biols, fwc, 1, indices_min, indices_max)
    biomass_out <-  test_operatingModel_biomass_flash(flfs, biols, fwc, 1, indices_min, indices_max)
    # Get SSB at time of spawning in following timestep (season 3)
    next_year <- year
    next_season <- season + 1
    next_indices_min <- c(next_year,1,next_season,1,1)
    next_indices_max <- c(next_year,dms[3],next_season,dms[5],dms[6])
    next_indices_min6 <- c(1, next_indices_min)
    next_indices_max6 <- c(dms[1], next_indices_max)
    timestep <- (year-1)*dms[4]+season
    next_timestep <- timestep+1
    # need to get N in next timestep
    om2 <- test_operatingModel_project_biols(flfs, biols, fwc, next_timestep)
    nextn <- om2[["biols"]][[1]]@n[,next_year,,next_season,]
    exp_z_pre_spwn <- test_operatingModel_get_exp_z_pre_spwn(flfs, biols, fwc, 1, next_indices_min6, next_indices_max6)
    ssb_in <- quantSums(exp_z_pre_spwn * nextn * (wt(biols[[1]][["biol"]]) * mat(biols[[1]][["biol"]]))[,next_year,,next_season,])
    expect_FLQuant_equal(ssb_in, ssb_out)
    biomass_in <- quantSums(exp_z_pre_spwn * nextn * wt(biols[[1]][["biol"]])[,next_year,,next_season,])
    expect_FLQuant_equal(biomass_in, biomass_out)

    # * Season 3 spawns - also spawns in season 4
    season <- 3
    indices_min <- c(year,1,season,1,1)
    indices_max <- c(year,dms[3],season,dms[5],dms[6])
    indices_min6 <- c(1, indices_min)
    indices_max6 <- c(dms[1], indices_max)
    # If F before spawning, get SSB at time of spawning in season 3
    flfs[[1]]@hperiod[1,] <- 0.3
    ssb_out <-  test_operatingModel_ssb_flash(flfs, biols, fwc, 1, indices_min, indices_max)
    ssb_in <-  test_operatingModel_ssb_spawn(flfs, biols, fwc, 1, indices_min, indices_max)
    expect_FLQuant_equal(ssb_in, ssb_out)
    biomass_out <-  test_operatingModel_biomass_flash(flfs, biols, fwc, 1, indices_min, indices_max)
    biomass_in <-  test_operatingModel_biomass_spawn(flfs, biols, fwc, 1, indices_min, indices_max)
    expect_FLQuant_equal(biomass_in, biomass_out)
    # If F after spawning, get SSB at time of spawning in season 4
    flfs[[1]]@hperiod[1,] <- 0.9
    ssb_out <-  test_operatingModel_ssb_flash(flfs, biols, fwc, 1, indices_min, indices_max)
    biomass_out <-  test_operatingModel_biomass_flash(flfs, biols, fwc, 1, indices_min, indices_max)
    next_year <- year
    next_season <- season + 1
    next_indices_min <- c(next_year,1,next_season,1,1)
    next_indices_max <- c(next_year,dms[3],next_season,dms[5],dms[6])
    next_indices_min6 <- c(1, next_indices_min)
    next_indices_max6 <- c(dms[1], next_indices_max)
    timestep <- (year-1)*dms[4]+season
    next_timestep <- timestep+1
    om2 <- test_operatingModel_project_biols(flfs, biols, fwc, next_timestep)
    nextn <- om2[["biols"]][[1]]@n[,next_year,,next_season,]
    exp_z_pre_spwn <- test_operatingModel_get_exp_z_pre_spwn(flfs, biols, fwc, 1, next_indices_min6, next_indices_max6)
    ssb_in <- quantSums(exp_z_pre_spwn * nextn * (wt(biols[[1]][["biol"]]) * mat(biols[[1]][["biol"]]))[,next_year,,next_season,])
    expect_FLQuant_equal(ssb_in, ssb_out)
    biomass_in <- quantSums(exp_z_pre_spwn * nextn * wt(biols[[1]][["biol"]])[,next_year,,next_season,])
    expect_FLQuant_equal(biomass_in, biomass_out)
})

test_that("operatingModel spawn_before_fishing and fishing_before_spawn",{
    # These methods evaluate the current state of the OM
    # i.e. just pull values out, no calculation
    data(ple4)
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    niters <- 10
    om <- make_test_operatingModel(ple4, FCB, recruitment_seasons = c(1,3), nseasons = 4, niters=niters)
    flfs <- om[["fisheries"]]
    biols <- om[["biols"]]
    fwc <- om[["fwc"]]
    dim <- dim(n(biols[[1]][["biol"]]))
    # Simple case: 1F -> 1B
    biol_no <- 1
    # * Season 1
    season <- 1
    year <- round(runif(1, min=1,max=dim[2]))
    indices_min <- c(year,1,season,1,1)
    indices_max<- c(year,dim[3],season,dim[5],dim[6])
    # spwn = 0, hstart = 0
    spwn(biols[[biol_no]][["biol"]])[,year,,season] <- 0.0
    hperiod(flfs[[1]])[1,year,,season] <- 0.0
    expect_true(test_operatingModel_spawn_before_fishing(flfs, biols, fwc, biol_no, indices_min, indices_max))
    expect_false(test_operatingModel_fishing_before_spawn(flfs, biols, fwc, biol_no, indices_min, indices_max))
    # spwn = 0, hstart = 0.3
    spwn(biols[[biol_no]][["biol"]])[,year,,season] <- 0.0
    hperiod(flfs[[1]])[1,year,,season] <- 0.3
    expect_true(test_operatingModel_spawn_before_fishing(flfs, biols, fwc, biol_no, indices_min, indices_max))
    expect_false(test_operatingModel_fishing_before_spawn(flfs, biols, fwc, biol_no, indices_min, indices_max))
    # spwn = 0.3, hstart = 0.3
    spwn(biols[[biol_no]][["biol"]])[,year,,season] <- 0.3
    hperiod(flfs[[1]])[1,year,,season] <- 0.3
    expect_true(test_operatingModel_spawn_before_fishing(flfs, biols, fwc, biol_no, indices_min, indices_max))
    expect_false(test_operatingModel_fishing_before_spawn(flfs, biols, fwc, biol_no, indices_min, indices_max))
    # spwn = 0.5, hstart = 0.3
    spwn(biols[[biol_no]][["biol"]])[,year,,season] <- 0.5
    hperiod(flfs[[1]])[1,year,,season] <- 0.3
    expect_false(test_operatingModel_spawn_before_fishing(flfs, biols, fwc, biol_no, indices_min, indices_max))
    expect_true(test_operatingModel_fishing_before_spawn(flfs, biols, fwc, biol_no, indices_min, indices_max))
    # spwn = NA, hstart = 0.3
    spwn(biols[[biol_no]][["biol"]])[,year,,season] <- NA
    hperiod(flfs[[1]])[1,year,,season] <- 0.3
    expect_false(test_operatingModel_spawn_before_fishing(flfs, biols, fwc, biol_no, indices_min, indices_max))
    expect_false(test_operatingModel_fishing_before_spawn(flfs, biols, fwc, biol_no, indices_min, indices_max))
    # spwn = 0.3, hstart = NA 
    spwn(biols[[biol_no]][["biol"]])[,year,,season] <- 0.3
    hperiod(flfs[[1]])[1,year,,season] <- NA
    expect_false(test_operatingModel_spawn_before_fishing(flfs, biols, fwc, biol_no, indices_min, indices_max))
    expect_false(test_operatingModel_fishing_before_spawn(flfs, biols, fwc, biol_no, indices_min, indices_max))
    # spwn = NA, hstart = NA 
    spwn(biols[[biol_no]][["biol"]])[,year,,season] <- NA
    hperiod(flfs[[1]])[1,year,,season] <- NA
    expect_false(test_operatingModel_spawn_before_fishing(flfs, biols, fwc, biol_no, indices_min, indices_max))
    expect_false(test_operatingModel_fishing_before_spawn(flfs, biols, fwc, biol_no, indices_min, indices_max))

    # More complicated case: 2F -> 1B
    biol_no <- 2
    season <- 3
    year <- round(runif(1, min=1,max=dim[2]))
    indices_min <- c(year,1,season,1,1)
    indices_max<- c(year,dim[3],season,dim[5],dim[6])
    # spwn = 0, hstart = 0, hstart = 0
    spwn(biols[[biol_no]][["biol"]])[,year,,season] <- 0.0
    hperiod(flfs[[1]])[1,year,,season] <- 0.0
    hperiod(flfs[[2]])[1,year,,season] <- 0.0
    expect_true(test_operatingModel_spawn_before_fishing(flfs, biols, fwc, biol_no, indices_min, indices_max))
    expect_false(test_operatingModel_fishing_before_spawn(flfs, biols, fwc, biol_no, indices_min, indices_max))
    # spwn = 0.3, hstart = 0, hstart = 0
    spwn(biols[[biol_no]][["biol"]])[,year,,season] <- 0.3
    hperiod(flfs[[1]])[1,year,,season] <- 0.0
    hperiod(flfs[[2]])[1,year,,season] <- 0.0
    expect_false(test_operatingModel_spawn_before_fishing(flfs, biols, fwc, biol_no, indices_min, indices_max))
    expect_true(test_operatingModel_fishing_before_spawn(flfs, biols, fwc, biol_no, indices_min, indices_max))
    # spwn = 0.3, hstart = 0.5, hstart = 0
    spwn(biols[[biol_no]][["biol"]])[,year,,season] <- 0.3
    hperiod(flfs[[1]])[1,year,,season] <- 0.5
    hperiod(flfs[[2]])[1,year,,season] <- 0.0
    expect_true(test_operatingModel_spawn_before_fishing(flfs, biols, fwc, biol_no, indices_min, indices_max))
    expect_true(test_operatingModel_fishing_before_spawn(flfs, biols, fwc, biol_no, indices_min, indices_max))
})


test_that("operatingModel landings, catch and discards methods",{
    # These methods evaluate the current state of the OM
    # i.e. just pull values out, no calculation
    data(ple4)
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    om <- make_test_operatingModel(ple4, FCB, nseasons = 4, niters=10)
    dim_max <- dim(n(om[["biols"]][[1]][["biol"]]))
    dim_min <- round(runif(6, min=1, max=dim_max))
    year <- round(runif(1,min=dim_min[2],max=dim_max[2]))
    season <- round(runif(1,min=dim_min[4],max=dim_max[4]))
    timestep <- (year-1) * dim(n(om[["biols"]][[1]][["biol"]]))[4] + season
    # 1 biol -> 1 catch
    biol_no <- 1
    landings1_out <- test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    landings1_in <- landings(om[["fisheries"]][[1]][[1]])
    expect_FLQuant_equal(landings1_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], landings1_out)
    discards1_out <- test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    discards1_in <- discards(om[["fisheries"]][[1]][[1]])
    expect_FLQuant_equal(discards1_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], discards1_out)
    catch1_out <- test_operatingModel_catches_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    catch1_in <- catch(om[["fisheries"]][[1]][[1]])
    expect_FLQuant_equal(catch1_in[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], catch1_out)

    landings1_n_out <- test_operatingModel_landings_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    landings1_n_in <- landings.n(om[["fisheries"]][[1]][[1]])
    expect_FLQuant_equal(landings1_n_in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], landings1_n_out)
    discards1_n_out <- test_operatingModel_discards_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    discards1_n_in <- discards.n(om[["fisheries"]][[1]][[1]])
    expect_FLQuant_equal(discards1_n_in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], discards1_n_out)
    catch1_n_out <- test_operatingModel_catch_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    catch1_n_in <- catch.n(om[["fisheries"]][[1]][[1]])
    expect_FLQuant_equal(catch1_n_in[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], catch1_n_out)

    # 1 biol -> 2 catch
    biol_no <- 2
    landings2_out <- test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    landings12_in <- landings(om[["fisheries"]][[1]][[2]])
    landings21_in <- landings(om[["fisheries"]][[2]][[1]])
    expect_FLQuant_equal((landings12_in+landings21_in)[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], landings2_out)
    discards2_out <- test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    discards12_in <- discards(om[["fisheries"]][[1]][[2]])
    discards21_in <- discards(om[["fisheries"]][[2]][[1]])
    expect_FLQuant_equal((discards12_in+discards21_in)[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], discards2_out)
    catch2_out <- test_operatingModel_catches_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min[-1], dim_max[-1])
    catch12_in <- catch(om[["fisheries"]][[1]][[2]])
    catch21_in <- catch(om[["fisheries"]][[2]][[1]])
    expect_FLQuant_equal((catch12_in+catch21_in)[, dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], catch2_out)

    landings2_n_out <- test_operatingModel_landings_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    landings12_n_in <- landings.n(om[["fisheries"]][[1]][[2]])
    landings21_n_in <- landings.n(om[["fisheries"]][[2]][[1]])
    expect_FLQuant_equal((landings12_n_in+landings21_n_in)[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], landings2_n_out)

    discards2_n_out <- test_operatingModel_discards_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    discards12_n_in <- discards.n(om[["fisheries"]][[1]][[2]])
    discards21_n_in <- discards.n(om[["fisheries"]][[2]][[1]])
    expect_FLQuant_equal((discards12_n_in+discards21_n_in)[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], discards2_n_out)
    catch2_n_out <- test_operatingModel_catch_n_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], biol_no, dim_min, dim_max)
    catch12_n_in <- catch.n(om[["fisheries"]][[1]][[2]])
    catch21_n_in <- catch.n(om[["fisheries"]][[2]][[1]])
    expect_FLQuant_equal((catch12_n_in+catch21_n_in)[dim_min[1]:dim_max[1], dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], catch2_n_out)

    # BUG FAILS: 2 biol -> 1 catch
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

# Just making sure that the correct target is called with the right indices
# Also checking that FCB combinations are OK
test_that("operatingModel eval_om", {
    # All singing and dancing OM
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    min_age <- 1
    niters <- 20
    nseasons <- 4
    no_rec_seasons <- 2
    rec_seasons <- sort(sample(1:nseasons, no_rec_seasons))
    om <- make_test_operatingModel(ple4, FCB, nseasons = nseasons, recruitment_seasons = rec_seasons, recruitment_age = min_age, niters = niters, sd = 0.1)
    flfs <- om[["fisheries"]]
    flbs <- om[["biols"]]
    fwc <- om[["fwc"]]
    dms <- dim(n(om[["biols"]][[1]][["biol"]]))
    year <- round(runif(1, min=2,max=dms[2]-1))
    season <- round(runif(1, min=1,max=dms[4]))
    indices_min5 <- c(year,1,season,1,1)
    indices_max5 <- c(year,dms[3],season,1,dms[6])

    # Initial FCB checks
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "revenue", as.integer(NA), as.integer(NA), as.integer(NA), indices_min5, indices_max5))
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "revenue", as.integer(NA), 1, as.integer(NA), indices_min5, indices_max5))
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "revenue", 1, as.integer(NA), 1, indices_min5, indices_max5))

    # ** Revenue (F, F+C) **
    fishery <- round(runif(1,min=1,max=2))
    catch <- round(runif(1,min=1,max=2))
    # F
    rout <- test_operatingModel_eval_om(flfs, flbs, fwc, "revenue", fishery, as.integer(NA), as.integer(NA), indices_min5, indices_max5)
    rin <- unitSums(test_FLFishery_revenue_subset(flfs[[fishery]], indices_min5, indices_max5))
    expect_FLQuant_equal(rout, rin)
    # F + C
    rout <- test_operatingModel_eval_om(flfs, flbs, fwc, "revenue", fishery, catch, as.integer(NA), indices_min5, indices_max5)
    rin <- unitSums(test_FLCatch_revenue_subset(flfs[[fishery]][[catch]], indices_min5, indices_max5))
    expect_FLQuant_equal(rout, rin)
    # error - just C
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "revenue", as.integer(NA), catch, as.integer(NA), indices_min5, indices_max5))
    # error - FCB
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "revenue", fishery, catch, 1, indices_min5, indices_max5))

    # ** Effort (F)
    # No unit structure
    eindices_min5 <- c(year,1,season,1,1)
    eindices_max5 <- c(year,1,season,1,dms[6])
    fishery <- round(runif(1,min=1,max=2))
    eout <- test_operatingModel_eval_om(flfs, flbs, fwc, "effort", fishery, as.integer(NA), as.integer(NA), eindices_min5, eindices_max5)
    ein <- flfs[[fishery]]@effort[1,eindices_min5[1]:eindices_max5[1], eindices_min5[2]:eindices_max5[2], eindices_min5[3]:eindices_max5[3], eindices_min5[4]:eindices_max5[4], eindices_min5[5]:eindices_max5[5]]
    expect_FLQuant_equal(eout, ein)
    # Error F + C
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "effort", fishery, 1, as.integer(NA), eindices_min5, eindices_max5))
    # Error F + C + B
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "effort", fishery, 1, 1, eindices_min5, eindices_max5))

    # ** Catch (F+C, B)
    fishery <- round(runif(1,min=1,max=2))
    catch <- round(runif(1,min=1,max=2))
    biol <- round(runif(1,min=1,max=2))
    # F + C
    cout <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", fishery, catch, as.integer(NA), indices_min5, indices_max5)
    cin <- unitSums(catch(flfs[[fishery]][[catch]])[1,indices_min5[1]:indices_max5[1], indices_min5[2]:indices_max5[2], indices_min5[3]:indices_max5[3], indices_min5[4]:indices_max5[4], indices_min5[5]:indices_max5[5]])
    expect_FLQuant_equal(cout, cin)
    # B
    cout <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    cin <- unitSums(test_operatingModel_catches_subset(flfs, flbs, fwc, biol, indices_min5, indices_max5))
    expect_FLQuant_equal(cout, cin)
    # error - FCB - not yet implemented
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "catch", fishery, catch, biol, indices_min5, indices_max5))
    # error - just F
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "catch", fishery, as.integer(NA), as.integer(NA), indices_min5, indices_max5))

    # ** Landings (F+C, B)
    fishery <- round(runif(1,min=1,max=2))
    catch <- round(runif(1,min=1,max=2))
    biol <- round(runif(1,min=1,max=2))
    # F + C
    lout <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", fishery, catch, as.integer(NA), indices_min5, indices_max5)
    lin <- unitSums(landings(flfs[[fishery]][[catch]])[1,indices_min5[1]:indices_max5[1], indices_min5[2]:indices_max5[2], indices_min5[3]:indices_max5[3], indices_min5[4]:indices_max5[4], indices_min5[5]:indices_max5[5]])
    expect_FLQuant_equal(lout, lin)
    # B
    lout <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    lin <- unitSums(test_operatingModel_landings_subset(flfs, flbs, fwc, biol, indices_min5, indices_max5))
    expect_FLQuant_equal(lout, lin)
    # error - FCB - not yet implemented
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "landings", fishery, catch, biol, indices_min5, indices_max5))
    # error - just F
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "landings", fishery, as.integer(NA), as.integer(NA), indices_min5, indices_max5))

    # ** Discards (F+C, B)
    fishery <- round(runif(1,min=1,max=2))
    catch <- round(runif(1,min=1,max=2))
    biol <- round(runif(1,min=1,max=2))
    # F + C
    dout <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", fishery, catch, as.integer(NA), indices_min5, indices_max5)
    din <- unitSums(discards(flfs[[fishery]][[catch]])[1,indices_min5[1]:indices_max5[1], indices_min5[2]:indices_max5[2], indices_min5[3]:indices_max5[3], indices_min5[4]:indices_max5[4], indices_min5[5]:indices_max5[5]])
    expect_FLQuant_equal(dout, din)
    # B
    dout <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    din <- unitSums(test_operatingModel_discards_subset(flfs, flbs, fwc, biol, indices_min5, indices_max5))
    expect_FLQuant_equal(dout, din)
    # error - FCB - not yet implemented
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "discards", fishery, catch, biol, indices_min5, indices_max5))
    # error - just F
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "discards", fishery, as.integer(NA), as.integer(NA), indices_min5, indices_max5))

    # ** Fbar (FCB, B) - where possible depending on number of Catches fishing the biol and the number of units
    # Check the errors with FCB
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 1, as.integer(NA), as.integer(NA), indices_min_u1, indices_max_u1))
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 1, 1, as.integer(NA), indices_min_u1, indices_max_u1))
    # New indices
    min_age <- round(runif(1,min=1,max=dms[1]/2))
    max_age <- round(runif(1,min=min_age+1,max=dms[1]))
    # * Single Unit
    indices_min_u1 <- c(min_age, year,1,season,1,1)
    indices_max_u1 <- c(max_age, year,1,season,1,dms[6])
    # B1, B2, B3, B4
    for (biol in 1:4){
        fbar_in <- test_operatingModel_fbar_B(flfs, flbs, fwc, biol, indices_min_u1, indices_max_u1)
        fbar_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", as.integer(NA), as.integer(NA), biol, indices_min_u1, indices_max_u1)
        expect_FLQuant_equal(fbar_in, fbar_out)
    }
    # F1C1B1
    fbar_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 1, 1, 1, indices_min_u1, indices_max_u1)
    fbar_in <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 1, 1, 1, indices_min_u1, indices_max_u1)
    expect_FLQuant_equal(fbar_in, fbar_out)
    # F1C2B2
    fbar_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 1, 2, 2, indices_min_u1, indices_max_u1)
    fbar_in <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 1, 2, 2, indices_min_u1, indices_max_u1)
    expect_FLQuant_equal(fbar_in, fbar_out)
    # F2C1B2
    fbar_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 2, 1, 2, indices_min_u1, indices_max_u1)
    fbar_in <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 2, 1, 2, indices_min_u1, indices_max_u1)
    expect_FLQuant_equal(fbar_in, fbar_out)
    # F2C2B3
    fbar_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 2, 2, 3, indices_min_u1, indices_max_u1)
    fbar_in <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 2, 2, 3, indices_min_u1, indices_max_u1)
    expect_FLQuant_equal(fbar_in, fbar_out)
    # F2C2B4
    fbar_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 2, 2, 4, indices_min_u1, indices_max_u1)
    fbar_in <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 2, 2, 4, indices_min_u1, indices_max_u1)
    expect_FLQuant_equal(fbar_in, fbar_out)
    # * Multi unit
    indices_min <- c(min_age, year,1,season,1,1)
    indices_max <- c(max_age, year,dms[3],season,1,dms[6])
    # B1
    fbar_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", as.integer(NA), as.integer(NA), 1, indices_min, indices_max)
    fbar_in <- test_operatingModel_fbar_B(flfs, flbs, fwc, 1, indices_min, indices_max)
    expect_FLQuant_equal(fbar_in, fbar_out)
    # B2
    fbar_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", as.integer(NA), as.integer(NA), 2, indices_min, indices_max)
    fbar_in <- test_operatingModel_fbar_B(flfs, flbs, fwc, 2, indices_min, indices_max)
    expect_FLQuant_equal(fbar_in, fbar_out)
    # B3 and B4
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", as.integer(NA), as.integer(NA), 3, indices_min, indices_max))
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", as.integer(NA), as.integer(NA), 4, indices_min, indices_max))
    # F1C1B1
    fbar_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 1, 1, 1, indices_min, indices_max)
    fbar_in <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 1, 1, 1, indices_min, indices_max)
    expect_FLQuant_equal(fbar_in, fbar_out)
    # F1C2B2
    fbar_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 1, 2, 2, indices_min, indices_max)
    fbar_in <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 1, 2, 2, indices_min, indices_max)
    expect_FLQuant_equal(fbar_in, fbar_out)
    # F2C1B2
    fbar_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 2, 1, 2, indices_min, indices_max)
    fbar_in <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 2, 1, 2, indices_min, indices_max)
    expect_FLQuant_equal(fbar_in, fbar_out)
    # F2C1B2 and F2C2B3
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 2, 2, 3, indices_min, indices_max))
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "fbar", 2, 2, 4, indices_min, indices_max))

    # ** SRP (B)
    biol <- round(runif(1,min=1,max=4))
    # If spwn in timestep is NA OR hperiod > spwn - stop
    # Spwn is NA
    season <- (1:nseasons)[!((1:nseasons) %in% rec_seasons)][1]
    indices_min5 <- c(year,1,season,1,1)
    indices_max5 <- c(year,dms[3],season,1,dms[6])
    expect_warning(test_operatingModel_eval_om(flfs, flbs, fwc, "srp", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5))
    # Spwn before F
    season <- rec_seasons[1]
    indices_min5 <- c(year,1,season,1,1)
    indices_max5 <- c(year,dms[3],season,1,dms[6])
    flbs[[biol]][["biol"]]@spwn[] <- 0.0
    expect_warning(test_operatingModel_eval_om(flfs, flbs, fwc, "srp", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5))
    # Spwn after F - OK
    flbs[[biol]][["biol"]]@spwn[] <- 0.5
    sout <- test_operatingModel_eval_om(flfs, flbs, fwc, "srp", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    sin <- test_operatingModel_total_SRP_FLQ_subset(flfs, flbs, fwc, biol, indices_min5, indices_max5)
    expect_FLQuant_equal(sout, sin)
    # Error FCB
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "srp", 1, 1, 1, indices_min5, indices_max5))
    # Error FC
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "srp", 1, 1, as.integer(NA), indices_min5, indices_max5))

    # ** SSB_spawn and biomass_spawn (B)
    biol <- round(runif(1,min=1,max=4))
    # If spwn in timestep is NA OR hperiod > spwn - stop
    # Spwn is NA
    season <- (1:nseasons)[!((1:nseasons) %in% rec_seasons)][1]
    indices_min5 <- c(year,1,season,1,1)
    indices_max5 <- c(year,dms[3],season,1,dms[6])
    expect_warning(expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_spawn", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)))
    expect_warning(expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_spawn", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)))
    # Spwn before F - throw an error
    season <- rec_seasons[1]
    indices_min5 <- c(year,1,season,1,1)
    indices_max5 <- c(year,dms[3],season,1,dms[6])
    flbs[[biol]][["biol"]]@spwn[] <- 0.0
    expect_warning(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_spawn", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5))
    expect_warning(test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_spawn", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5))
    # Spwn after F - OK
    flbs[[biol]][["biol"]]@spwn[] <- 0.5
    sout <- test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_spawn", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    bout <- test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_spawn", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    sin <- unitSums(test_operatingModel_ssb_spawn(flfs, flbs, fwc, biol, indices_min5, indices_max5))
    bin <- unitSums(test_operatingModel_biomass_spawn(flfs, flbs, fwc, biol, indices_min5, indices_max5))
    expect_FLQuant_equal(sout, sin)
    expect_FLQuant_equal(bout, bin)
    # Error FCB
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_spawn", 1, 1, 1, indices_min5, indices_max5))
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_spawn", 1, 1, 1, indices_min5, indices_max5))
    # Error FC
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_spawn", 1, 1, as.integer(NA), indices_min5, indices_max5))
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_spawn", 1, 1, as.integer(NA), indices_min5, indices_max5))

    # ** SSB_end and Biomass_end (B) - at end of timestep
    biol <- round(runif(1,min=1,max=4))
    year <- round(runif(1, min=2,max=dms[2]-1))
    season <- round(runif(1, min=1,max=dms[4]))
    indices_min5 <- c(year,1,season,1,1)
    indices_max5 <- c(year,dms[3],season,1,dms[6])
    sout <- test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_end", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    bout <- test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_end", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    sin <- unitSums(test_operatingModel_ssb_end(flfs, flbs, fwc, biol, indices_min5, indices_max5))
    bin <- unitSums(test_operatingModel_biomass_end(flfs, flbs, fwc, biol, indices_min5, indices_max5))
    expect_FLQuant_equal(sout, sin)
    expect_FLQuant_equal(bout, bin)
    # Error FCB
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_end", 1, 1, 1, indices_min, indices_max))
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_end", 1, 1, 1, indices_min, indices_max))
    # Error FC
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_end", 1, 1, as.integer(NA), indices_min, indices_max))
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_end", 1, 1, as.integer(NA), indices_min, indices_max))
    
    ## ** SSB_start and Biomass_start (B) - at start of timestep
    # Doesn't make sense
    #biol <- round(runif(1,min=1,max=4))
    #year <- round(runif(1, min=2,max=dms[2]-1))
    #season <- round(runif(1, min=1,max=dms[4]))
    #indices_min5 <- c(year,1,season,1,1)
    #indices_max5 <- c(year,dms[3],season,1,dms[6])
    #sout <- test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_start", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    #bout <- test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_start", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    #sin <- unitSums(test_operatingModel_ssb_start(flfs, flbs, fwc, biol, indices_min5, indices_max5))
    #bin <- unitSums(test_operatingModel_biomass_start(flfs, flbs, fwc, biol, indices_min5, indices_max5))
    #expect_FLQuant_equal(sout, sin)
    #expect_FLQuant_equal(bout, bin)
    ## Error FCB
    #expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_start", 1, 1, 1, indices_min, indices_max))
    #expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_start", 1, 1, 1, indices_min, indices_max))
    ## Error FC
    #expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_start", 1, 1, as.integer(NA), indices_min, indices_max))
    #expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_start", 1, 1, as.integer(NA), indices_min, indices_max))

    # ** SSB_flash and biomass_flash (B)
    # Assume that ssb_flash methods are OK
    biol <- round(runif(1,min=1,max=4))
    # If spwn in timestep is NA OR hperiod > spwn - stop
    # Spwn is NA - get SSB in following timestep
    # This is really dodgy because if spawning in next timestep happens after fishing in next timestep, then SSB at spawning is a function of fishing effort in current AND next timestep
    season <- 1
    flbs[[biol]][["biol"]]@spwn[,,,season] <- NA
    flbs[[biol]][["biol"]]@spwn[,,,season+1] <- 0.0
    indices_min5 <- c(year,1,season,1,1)
    indices_max5 <- c(year,dms[3],season,1,dms[6])
    sout <- test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_flash", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    bout <- test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_flash", as.integer(NA), as.integer(NA), biol, indices_min5, indices_max5)
    sin <- unitSums(test_operatingModel_ssb_flash(flfs, flbs, fwc, biol, indices_min5, indices_max5))
    bin <- unitSums(test_operatingModel_biomass_flash(flfs, flbs, fwc, biol, indices_min5, indices_max5))
    expect_FLQuant_equal(sout, sin)
    expect_FLQuant_equal(bout, bin)
    # Error FCB
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_flash", 1, 1, 1, indices_min5, indices_max5))
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_flash", 1, 1, 1, indices_min5, indices_max5))
    # Error FC
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb_flash", 1, 1, as.integer(NA), indices_min5, indices_max5))
    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "biomass_flash", 1, 1, as.integer(NA), indices_min5, indices_max5))
})

# Current values in operating model (asked for by values in control)
test_that("get_target_value_hat", {
    # Two fisheries, two biols, with units
    # FC11 -> B1
    # FC12 and FC21 -> B2
    niters <- 10 
    data(ple4)
    FCB <- array(c(1,1,2,1,2,1,1,2,2), dim=c(3,3))
    colnames(FCB) <- c("F","C","B")
    om <- make_test_operatingModel(ple4, FCB, nseasons = 4, recruitment_seasons = c(1,3), recruitment_age = 1, niters = niters, sd = 0.1)
    flfs <- om[["fisheries"]]
    flbs <- om[["biols"]]
    dims <- dim(n(flbs[[1]][["biol"]]))
    # A selection of years and seasons for targets
    years <- sort(rep(round(runif(4, min=1,max=dims[2])),each=2))
    seasons <- rep(round(runif(4, min=1,max=dims[4])),each=2)
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], 
                        quant = c("catch","catch"),
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2),
                        relFishery = NA, relCatch = NA, relBiol = NA,
                        relYear = NA, relSeason = NA)
    trgt2 <- data.frame(year = years[3:4], season = seasons[3:4],
                        quant = c("landings","discards"),
                        fishery = c(NA,1), catch = c(NA,2), biol = c(1,NA),
                        relFishery = NA, relCatch = NA, relBiol = NA,
                        relYear = NA, relSeason = NA)
    rel_years <- rep(round(runif(4, min=1,max=dims[2])),each=2)
    rel_seasons <- rep(round(runif(4, min=1,max=dims[4])),each=2)
    rel_trgt1 <- data.frame(year = years[5:6], season = seasons[5:6],
                        quant = c("catch","catch"),
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2),
                        relFishery = c(1,NA), relCatch = c(1,NA), relBiol = c(NA,2),
                        relYear = rel_years[5:6], relSeason = rel_seasons[5:6])
    # Discards relative to different catch and fishery
    rel_trgt2 <- data.frame(year = years[7:8], season = seasons[7:8],
                        quant = c("landings","discards"),
                        fishery = c(NA,1), catch = c(NA,2), biol = c(1,NA),
                        relFishery = c(NA,2), relCatch = c(NA,1), relBiol = c(1,NA),
                        relYear = rel_years[7:8], relSeason = rel_seasons[7:8])
    trg_df <- rbind(trgt1, trgt2, rel_trgt1, rel_trgt2)
    fwc <- fwdControl(trg_df)
    # Need to add order column back in (done automatically in fwd)
    fwc@target$order <- c(1,1,2,2,3,3,4,4)
    fwc@FCB <- FCB
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

    # Test that relative targets fail if not set properly
    rel_trgt3 <- data.frame(year = 1:8, season = 1, 
                        quant = "catch", 
                        fishery = 1, catch = 1, biol = NA,
                        relFishery = c(NA,1,1,1,1,NA,NA,NA), relCatch = c(1,NA,1,1,1,NA,NA,NA), relBiol = c(NA,NA,NA,NA,NA,1,1,NA),
                        relYear = c(1,1,NA,NA,1,1,NA,1), relSeason = c(1,1,NA,1,NA,NA,1,1))
    fwc <- fwdControl(rel_trgt3)
    fwc@target$order <- 1:8
    fwc@FCB <- FCB
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 1))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 2)) 
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 3))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 4))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 5))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 6))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 7))
    expect_error(test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 8))

    # Fbar target with ages
    dimns <- dimnames(n(flbs[[1]][["biol"]]))
    min_age <- round(runif(1,min=min(as.numeric(dimns$age)), max=max(as.numeric(dimns$age))))
    max_age <- round(runif(1,min=min_age, max=max(as.numeric(dimns$age))))
    years <- rep(round(runif(1, min=1,max=dims[2])),each=2)
    seasons <- rep(round(runif(1, min=1,max=dims[4])),each=2)
    f_trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], 
                        quant = c("f","f"), 
                        fishery = c(NA,1), catch = c(NA,2), biol = c(1,1),
                        minAge = min_age, maxAge = max_age)
    fwc <- fwdControl(rbind(f_trgt1))
    # Fix order (done in fwd)
    fwc@target$order <- 1
    fwc@FCB <- FCB
    # 1 sim target at a time
    dim_min <- c(min_age, years[1], 1, seasons[1], 1, 1)
    dim_max <- c(max_age, years[1], dims[3], seasons[1], dims[5], dims[6])
    # Sim 1 - F on Biol 1
    val_hat1 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 1, 1)
    val_in1 <- c(test_operatingModel_fbar_B(flfs, flbs, fwc, 1, dim_min, dim_max))
    expect_equal(val_hat1, val_in1)
    # Sim 2
    val_hat2 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 1, 2)
    val_in2 <- c(test_operatingModel_fbar_FCB(flfs, flbs, fwc, 1,2,1, dim_min, dim_max))
    expect_equal(val_hat2, val_in2)
    # Both
    val_hat <- test_operatingModel_get_target_value_hat2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_in1, val_in2))

})

# Values in control object 
test_that("get_target_value - straight value", {
    niters <- 10 
    data(ple4)
    FCB <- array(c(1,1,1,2,1,2), dim=c(2,3))
    colnames(FCB) <- c("F","C","B")
    om <- make_test_operatingModel(ple4, FCB, nseasons = 4, recruitment_seasons = c(1,3), recruitment_age = 1, niters = niters, sd = 0.1)
    flfs <- om[["fisheries"]]
    flbs <- om[["biols"]]
    # Make a fwdControl with a two targets
    dims <- dim(n(om[["biols"]][[1]][["biol"]]))
    years <- sort(rep(round(runif(2, min=1,max=dims[2])),each=2))
    seasons <- sort(rep(round(runif(2, min=1,max=dims[4])),each=2))
    value <- abs(rnorm(4))
    # 1 iter in control, many in OM - should blow up control iters internally
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], 
                        quant = c("catch","catch"), value = value[1:2],
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2))
    trgt2 <- data.frame(year = years[3:4], season = seasons[3:4], 
                        quant = c("landings","discards"), value = value[3:4],
                        fishery = c(NA,1), catch = c(NA,2), biol = c(1,NA))
    fwc <- fwdControl(rbind(trgt1,trgt2))
    fwc@target$order <- c(1,1,2,2) 
    fwc@FCB <- FCB
    # No iters in control - but iters in OM
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
    fwc@target$order <- c(1,1,2,2)
    fwc@iters[,"value",] <- abs(rnorm(4*niters))
    fwc@FCB <- FCB
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
    fwc@target$order <- c(1,1,2,2)
    fwc@iters[,"value",] <- abs(rnorm(4*(niters-1)))
    fwc@FCB <- FCB
    expect_error(test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1))
    # Many iters in control object, 1 in OM
    # Just throws error
    om <- make_test_operatingModel(ple4, FCB, nseasons = 4, recruitment_seasons = c(1,3), recruitment_age = 1, niters = 1, sd = 0.1)
    flfs <- om[["fisheries"]]
    flbs <- om[["biols"]]
    fwc <- fwdControl(rbind(trgt1, trgt2), niters)
    fwc@target$order <- c(1,1,2,2)
    fwc@iters[,"value",] <- abs(rnorm(4*niters))
    fwc@FCB <- FCB
    expect_error(test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1))
})

test_that("get_target_value - min / max values", {
    # Min / Max tests
    niters <- 10 
    data(ple4)
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    om <- make_test_operatingModel(ple4, FCB, nseasons = 4, recruitment_seasons = c(1,3), recruitment_age = 1, niters = niters, sd = 0.1)
    flfs <- om[["fisheries"]]
    flbs <- om[["biols"]]
    # Make a fwdControl with a two targets
    dims <- dim(n(om[["biols"]][[1]][["biol"]]))
    years <- sort(rep(round(runif(2, min=1,max=dims[2])),each=2))
    seasons <- sort(rep(round(runif(2, min=1,max=dims[4])),each=2))
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
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], 
                        quant = c("catch","catch"),
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2))
    fwc <- fwdControl(trgt1, niters)
    fwc@target$order <- 1
    fwc@iters[1,"max",] <- ctrl_catch1
    fwc@iters[2,"max",] <- ctrl_catch2
    fwc@FCB <- FCB
    # only the small iters should equal values in control - Others same as in OM
    val_hat1 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 1)
    expect_equal(val_hat1[small_iters], unname(fwc@iters[1,"max",small_iters]))
    expect_equal(val_hat1[big_iters], catch1[big_iters])
    val_hat2 <- test_operatingModel_get_target_value(flfs, flbs, fwc, 1, 2)
    expect_equal(val_hat2[small_iters], unname(fwc@iters[2,"max",small_iters]))
    expect_equal(val_hat2[big_iters], catch2[big_iters])
    val_hat <- test_operatingModel_get_target_value2(flfs, flbs, fwc, 1)
    expect_equal(val_hat, c(val_hat1, val_hat2))

    # Max only - iters in OM, only 1 in control
    # Set max target so half of them are maxed out
    fwc <- fwdControl(trgt1, 1)
    fwc@target$order <- 1
    max1 <- median(catch1)
    max2 <- median(catch2)
    fwc@iters[1,"max",] <- max1
    fwc@iters[2,"max",] <- max2
    fwc@FCB <- FCB
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
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], 
                        quant = c("catch","catch"),
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2))
    fwc <- fwdControl(trgt1, niters)
    fwc@target$order <- 1
    fwc@iters[1,"min",] <- ctrl_catch1
    fwc@iters[2,"min",] <- ctrl_catch2
    fwc@FCB <- FCB
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
    fwc@target$order <- 1
    min1 <- median(catch1)
    min2 <- median(catch2)
    fwc@iters[1,"min",] <- min1
    fwc@iters[2,"min",] <- min2
    fwc@FCB <- FCB
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
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], 
                        quant = c("catch","catch"),
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2))
    fwc <- fwdControl(trgt1, niters)
    fwc@target$order <- 1
    fwc@iters[1,"max",] <- max_catch1
    fwc@iters[1,"min",] <- min_catch1
    fwc@iters[2,"max",] <- max_catch2
    fwc@iters[2,"min",] <- min_catch2
    fwc@FCB <- FCB
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
    trgt1 <- data.frame(year = years[1:2], season = seasons[1:2], 
                        quant = c("catch","catch"),
                        fishery = c(1,NA), catch = c(1,NA), biol = c(NA,2))
    fwc <- fwdControl(trgt1, 1)
    fwc@target$order <- 1
    fwc@iters[1,"max",] <- max1
    fwc@iters[1,"min",] <- min1
    fwc@iters[2,"max",] <- max2
    fwc@iters[2,"min",] <- min2
    fwc@FCB <- FCB
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

test_that("operatingModel get_target_hat_indices", {
    niters <- 10 
    data(ple4)
    FCB <- array(c(1,1,1), dim=c(1,3))
    colnames(FCB) <- c("F","C","B")
    # Multiple seasons and units
    nseasons <- round(runif(1,min=4,max=12))
    nunits <- round(runif(1,min=2,max=nseasons))
    rec_seasons <- sample(1:nseasons, nunits)
    rec_age <- round(runif(1,min=0, max=3))
    om <- make_test_operatingModel(ple4, FCB, nseasons = nseasons, recruitment_seasons = rec_seasons, recruitment_age = rec_age, niters = niters, sd = 0.1)
    flfs <- om[["fisheries"]]
    flbs <- om[["biols"]]
    dims <- dim(n(flbs[[1]][["biol"]]))
    # Targets
    min_age <- round(runif(1,min=rec_age, max=(dims[1]/2)))
    max_age <- round(runif(1,min=min_age, max=dims[1]-1-rec_age))
    year <- round(runif(1,min=1, max=dims[2]))
    season1 <- round(runif(1,min=1, max=dims[4]))
    season2 <- round(runif(1,min=season1, max=dims[4]))
    trgt <- data.frame(year = year, season = c(season1, season1, season2), 
                        quant = c("f", "catch", "f"),
                        fishery = c(NA,1, 1), catch = c(NA,1,1), biol = c(1,NA,NA),
                        minAge = c(min_age, NA,NA), maxAge = c(max_age, NA, max_age))
    fwc <- fwdControl(trgt)
    fwc@target$order <- c(1,1,2)
    fwc@FCB <- FCB
    # Test 1 - age structure
    out <- test_operatingModel_get_target_hat_indices(flfs, flbs, fwc, 1, 1, FALSE)
    indices_min <- c(min_age-rec_age+1,year,1,season1,1,1)
    indices_max <- c(max_age-rec_age+1,year,dims[3],season1,1,dims[6])
    expect_identical(c(indices_min,indices_max), c(out[["indices_min"]], out[["indices_max"]]))
    # Test 2 - no age structure
    out <- test_operatingModel_get_target_hat_indices(flfs, flbs, fwc, 1, 2, FALSE)
    indices_min <- c(year,1,season1,1,1)
    indices_max <- c(year,dims[3],season1,1,dims[6])
    expect_identical(c(indices_min,indices_max), c(out[["indices_min"]], out[["indices_max"]]))
    # Test 3 - error min age but no max age
    expect_error(test_operatingModel_get_target_hat_indices(flfs, flbs, fwc, 2, 1, FALSE))
    # Relative targets
    min_age <- round(runif(1,min=rec_age, max=(dims[1]/2)))
    max_age <- round(runif(1,min=min_age, max=dims[1]-1-rec_age))
    year <- round(runif(1,min=1, max=dims[2]))
    season1 <- round(runif(1,min=1, max=dims[4]))
    season2 <- round(runif(1,min=1, max=dims[4]))
    trgt <- data.frame(year = NA, season = NA, 
                        quant = c("f", "catch"),
                        fishery = NA, catch = NA, biol = NA,
                        minAge = NA, maxAge = NA,
                        relYear = year, relSeason=c(season1, season2),
                        relFishery = c(NA,1), relCatch = c(NA,1), relBiol = c(1,NA),
                        relMinAge = c(min_age,NA), relMaxAge = c(max_age,NA) 
                        )
    fwc <- fwdControl(trgt)
    fwc@target$order <- c(1,2)
    fwc@FCB <- FCB
    # Test 1 - will fail as relative flag not set
    expect_error(test_operatingModel_get_target_hat_indices(flfs, flbs, fwc, 1, 1, FALSE))
    out <- test_operatingModel_get_target_hat_indices(flfs, flbs, fwc, 1, 1, TRUE)
    indices_min <- c(min_age-rec_age+1,year,1,season1,1,1)
    indices_max <- c(max_age-rec_age+1,year,dims[3],season1,1,dims[6])
    expect_identical(c(indices_min,indices_max), c(out[["indices_min"]], out[["indices_max"]]))
    # Test 2 - no age structure
    out <- test_operatingModel_get_target_hat_indices(flfs, flbs, fwc, 2, 1, TRUE)
    indices_min <- c(year,1,season2,1,1)
    indices_max <- c(year,dims[3],season2,1,dims[6])
    expect_identical(c(indices_min,indices_max), c(out[["indices_min"]], out[["indices_max"]]))
})


#test_that("operatingModel get_target_age_range", {
#    min_age_name <- round(runif(1,min=0,max=10))
#    #flq <- random_FLQuant_generator(min_dims=c(5,5,1,1,1,1), min_age_name = min_age_name)
#    flq <- random_FLQuant_generator(min_dims=c(5,5,1,1,1,1), fixed_dims=c(NA,NA,1,rep(NA,3)), min_age_name = min_age_name)
#    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 2, fixed_dims = dim(flq), min_age_name=min_age_name)
#    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2, min_age_name=min_age_name)
#    ages <- as.numeric(dimnames(flq)$age)
#    min_age <- round(runif(1,min=ages[1],max=max(ages)))
#    max_age <- round(runif(1,min=min_age, max=max(ages)))
#    # With biol no
#    trgt1 <- data.frame(year = 1, season = 1, 
#                        quant = "f",
#                        fishery = NA, catch = NA, biol = 1,
#                        minAge = min_age, maxAge = max_age)
#    fwc <- fwdControl(trgt1)
#    fwc@target$order <- 1
#    FCB <- array(1, dim=c(3,3))
#    fwc@FCB <- FCB
#    ind_out <- test_operatingModel_get_target_age_range_indices(flfs, flbs, fwc, 1, 1) 
#    ind_in <- c(which(ages %in% min_age), which(ages %in% max_age)) - 1
#    expect_identical(ind_out, ind_in)
#    # With catch no
#    trgt1 <- data.frame(year = 1, season = 1, 
#                        quant = "f",
#                        fishery = 1, catch = 1, biol = NA,
#                        minAge = min_age, maxAge = max_age)
#    fwc <- fwdControl(trgt1)
#    fwc@target$order <- 1
#    FCB <- array(1, dim=c(3,3))
#    fwc@FCB <- FCB
#    ind_out <- test_operatingModel_get_target_age_range_indices(flfs, flbs, fwc, 1, 1) 
#    ind_in <- c(which(ages %in% min_age), which(ages %in% max_age)) - 1
#    expect_identical(ind_out, ind_in)
#    # minAge outside age range - error
#    trgt1 <- data.frame(year = 1, season = 1, 
#                        quant = "f",
#                        fishery = 1, catch = 1, biol = NA,
#                        minAge = min_age_name - 1, maxAge = max_age)
#    fwc <- fwdControl(trgt1)
#    fwc@target$order <- 1
#    fwc@FCB <- FCB
#    expect_error(test_operatingModel_get_target_age_range_indices(flfs, flbs, fwc, 1, 1))
#    # maxAge outside age range - error
#    trgt1 <- data.frame(year = 1, season = 1, 
#                        quant = "f",
#                        fishery = 1, catch = 1, biol = NA,
#                        minAge = min_age, maxAge = max(as.numeric(dimnames(flq)$age))+1)
#    fwc <- fwdControl(trgt1)
#    fwc@FCB <- FCB
#    expect_error(test_operatingModel_get_target_age_range_indices(flfs, flbs, fwc, 1, 1))
#    # Without catch or biol in control - error
#    trgt1 <- data.frame(year = 1, season = 1, 
#                        quant = "f",
#                        fishery = NA, catch = NA, biol = NA,
#                        minAge = min_age, maxAge = max_age)
#    fwc <- fwdControl(trgt1)
#    fwc@target$order <- 1
#    FCB <- array(1, dim=c(3,3))
#    fwc@FCB <- FCB
#    expect_error(test_operatingModel_get_target_age_range_indices(flfs, flbs, fwc, 1, 1))
#})
