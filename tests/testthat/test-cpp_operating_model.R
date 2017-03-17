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
    expect_identical(out[["biols"]], flbs_in)
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

test_that("operatingModel fbar methods",{
    # Two catches on a biol
    flq <- random_FLQuant_generator(fixed_dims = c(10,2,2,2,2,20))
    flbs <- random_fwdBiols_list_generator(min_biols = 1, max_biols = 1, fixed_dims = dim(flq))
    # Pull out just FLBiols for testing
    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2)
    # fwdControl and FCB needed for constructor but not actually used to test F
    fc <- random_fwdControl_generator(years = 1, niters = dim(flq)[6])
    FCB <- array(c(1,2,1,1,1,1), dim = c(2,3))
    fc@FCB <- FCB
    dim_max <- dim(flq)
    dim_min <- round(runif(6, min=1, max = dim_max))
    # Fbar on whole biol
    # F summed over a unit
    #fin <- test_operatingModel_unit_f_B_subset(flfs, flbs, fc, 1, dim_min, dim_max)
    fin <- test_operatingModel_get_f_B_subset(flfs, flbs, fc, 1, dim_min, dim_max)
    fbar_in <- apply(fin, 2:6, mean)
    fbar_out <- test_operatingModel_fbar_B(flfs, flbs, fc, 1, dim_min, dim_max)
    expect_FLQuant_equal(fbar_in, fbar_out)
    # Fbar from single catch
    #fin <- test_operatingModel_unit_f_FCB_subset(flfs, flbs, fc, 1,1,1, dim_min, dim_max)
    fin <- test_operatingModel_get_f_FCB_subset(flfs, flbs, fc, 1,1,1, dim_min, dim_max)
    fbar_in <- apply(fin, 2:6, mean)
    fbar_out <- test_operatingModel_fbar_FCB(flfs, flbs, fc, 1,1,1, dim_min, dim_max)
    expect_FLQuant_equal(fbar_in, fbar_out)
    #fin <- test_operatingModel_unit_f_FCB_subset(flfs, flbs, fc, 2,1,1, dim_min, dim_max)
    fin <- test_operatingModel_get_f_FCB_subset(flfs, flbs, fc, 2,1,1, dim_min, dim_max)
    fbar_in <- apply(fin, 2:6, mean)
    fbar_out <- test_operatingModel_fbar_FCB(flfs, flbs, fc, 2,1,1, dim_min, dim_max)
    expect_FLQuant_equal(fbar_in, fbar_out)
})

test_that("operatingModel ssb target methods",{
    FCB <- array(c(1,1,1), dim=c(1,3))
    colnames(FCB) <- c("F","C","B")
    data(ple4)
    # Annual model
    min_age <- 1
    niters <- 20
    om <- make_test_operatingModel(ple4, FCB, nseasons = 1, recruitment_age = min_age, niters = niters, sd = 0.1)
    dms <- dim(n(om[["biols"]][[1]][["biol"]]))
    year <- round(runif(1,min=1, max= dms[2]-1))
    season <- round(runif(1,min=1, max= dms[4]))
    indices_min <- c(year,1,season,1,1)
    indices_max <- c(year,dms[3],season,dms[5],dms[6])
    survivors <- test_operatingModel_survivors(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, year)
    ssb <- quantSums(survivors * (wt(om[["biols"]][[1]][["biol"]]) * mat(om[["biols"]][[1]][["biol"]]))[,year,,season,])
    ssb_out <-  test_operatingModel_ssb_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, indices_min, indices_max)
    expect_FLQuant_equal(ssb, ssb_out)
    # Indices too long
    bad_indices_min <- c(1,indices_min)
    bad_indices_max <- c(1,indices_max)
    expect_error(test_operatingModel_ssb_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, bad_indices_min, indices_max))
    expect_error(test_operatingModel_ssb_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, indices_min, bad_indices_max))
    # Indices not the same time step
    indices_min[1] <- indices_min[1]+1
    expect_error(test_operatingModel_ssb_target(om[["fisheries"]], om[["biols"]], om[["fwc"]], 1, indices_min, indices_max))
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

## eval_om work with units - important as the target calculations use unit_sum()
#test_that("operatingModel eval_om units", {
#    # Two fisheries, two biols
#    # FC11 -> B1
#    # FC12 and FC21 -> B2
#    #nunits <- 8
#    nunits <- 1
#    niters <- 10 
#    flq <- random_FLQuant_generator(fixed_dims = c(5,20,nunits,4,1,niters))
#    flbs <- random_fwdBiols_list_generator(min_biols = 2, max_biols = 2, fixed_dims = dim(flq))
#    # Biol 1 spwn in season 1 only, Biol 2 spwns in season 3 but spwn = 0
#    flbs[[1]][["biol"]]@spwn[1,,,1] <- runif(niters * dim(flq)[2], min=0.1, max = 1)
#    flbs[[1]][["biol"]]@spwn[1,,,2:4] <- NA
#    flbs[[2]][["biol"]]@spwn[1,,,3] <- 0
#    flbs[[2]][["biol"]]@spwn[1,,,c(1,2,4)] <- NA
#    # Pull out just FLBiols for testing
#    flbs_in <- lapply(flbs, function(x) return(x[["biol"]]))
#    flfs <- random_FLFisheries_generator(fixed_dims = dim(flq), min_fisheries=2, max_fisheries=2, min_catches=2, max_catches=2)
#    fwc <- random_fwdControl_generator(niters=1)
#    FCB <- array(NA, dim=c(3,3))
#    FCB[1,] <- c(1,1,1)
#    FCB[2,] <- c(1,2,2)
#    FCB[3,] <- c(2,1,2)
#    fwc@FCB <- FCB
#    # Get full indices - but the last year
#    dim_max <- dim(n(flbs[[1]][["biol"]]))
#    dim_min <- round(runif(6,min=1,max=dim_max))
#    #  Catch FC11
#    cout11 <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", 1, 1, as.integer(NA), dim_min, dim_max)
#    cin11 <- unitSums(catch(flfs[[1]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
#    expect_FLQuant_equal(cout11,cin11)
#    #  Catch FC12
#    cout12 <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", 1, 2, as.integer(NA), dim_min, dim_max)
#    cin12 <- unitSums(catch(flfs[[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
#    expect_FLQuant_equal(cout12,cin12)
#    #  Catch FC21
#    cout21 <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", 2, 1, as.integer(NA), dim_min, dim_max)
#    cin21 <- unitSums(catch(flfs[[2]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
#    expect_FLQuant_equal(cout21,cin21)
#    #  Catch B1
#    coutb1 <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", as.integer(NA),as.integer(NA),1, dim_min, dim_max)
#    expect_FLQuant_equal(coutb1,cin11)
#    #  Catch B2
#    coutb2 <- test_operatingModel_eval_om(flfs, flbs, fwc, "catch", as.integer(NA),as.integer(NA),2, dim_min, dim_max)
#    expect_FLQuant_equal(coutb2,cin12+cin21)
#    #  Landings FC11
#    lout11 <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", 1, 1, as.integer(NA), dim_min, dim_max)
#    lin11 <- unitSums(landings(flfs[[1]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
#    expect_FLQuant_equal(lout11,lin11)
#    #  Landings FC12
#    lout12 <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", 1, 2, as.integer(NA), dim_min, dim_max)
#    lin12 <- unitSums(landings(flfs[[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
#    expect_FLQuant_equal(lout12,lin12)
#    #  Landings FC21
#    lout21 <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", 2, 1, as.integer(NA), dim_min, dim_max)
#    lin21 <- unitSums(landings(flfs[[2]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
#    expect_FLQuant_equal(lout21,lin21)
#    #  Landings B1
#    loutb1 <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", as.integer(NA),as.integer(NA),1, dim_min, dim_max)
#    expect_FLQuant_equal(loutb1,lin11)
#    #  Landings B2
#    loutb2 <- test_operatingModel_eval_om(flfs, flbs, fwc, "landings", as.integer(NA),as.integer(NA),2, dim_min, dim_max)
#    expect_FLQuant_equal(loutb2,lin12+lin21)
#    #  Discards FC11
#    dout11 <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", 1, 1, as.integer(NA), dim_min, dim_max)
#    din11 <- unitSums(discards(flfs[[1]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
#    expect_FLQuant_equal(dout11,din11)
#    #  Discards FC12
#    dout12 <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", 1, 2, as.integer(NA), dim_min, dim_max)
#    din12 <- unitSums(discards(flfs[[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
#    expect_FLQuant_equal(dout12,din12)
#    #  Discards FC21
#    dout21 <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", 2, 1, as.integer(NA), dim_min, dim_max)
#    din21 <- unitSums(discards(flfs[[2]][[1]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])
#    expect_FLQuant_equal(dout21,din21)
#    #  Discards B1
#    doutb1 <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", as.integer(NA),as.integer(NA),1, dim_min, dim_max)
#    expect_FLQuant_equal(doutb1,din11)
#    #  Discards B2
#    doutb2 <- test_operatingModel_eval_om(flfs, flbs, fwc, "discards", as.integer(NA),as.integer(NA),2, dim_min, dim_max)
#    expect_FLQuant_equal(doutb2,din12+din21)
#    # Fbar B1
#    foutb1 <- test_operatingModel_eval_om(flfs, flbs, fwc, "f", as.integer(NA),as.integer(NA),1, dim_min, dim_max)
#    finb1 <- test_operatingModel_fbar_B(flfs, flbs, fwc, 1, dim_min, dim_max) # Assume is correct
#    expect_FLQuant_equal(foutb1,finb1)
#    # Fbar FC11
#    foutfc11 <- test_operatingModel_eval_om(flfs, flbs, fwc, "f", 1,1,1, dim_min, dim_max)
#    finfc11 <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 1,1,1, dim_min, dim_max) # Assume is correct
#    expect_FLQuant_equal(foutfc11,finfc11)
#    # Fbar B2
#    foutb2 <- test_operatingModel_eval_om(flfs, flbs, fwc, "f", as.integer(NA),as.integer(NA),1, dim_min, dim_max)
#    finb2 <- test_operatingModel_fbar_B(flfs, flbs, fwc, 1, dim_min, dim_max) # Assume is correct
#    expect_FLQuant_equal(foutb1,finb1)
#    # Fbar FC12
#    foutfc12 <- test_operatingModel_eval_om(flfs, flbs, fwc, "f", 1,2,1, dim_min, dim_max)
#    finfc12 <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 1,2,1, dim_min, dim_max) # Assume is correct
#    expect_FLQuant_equal(foutfc12,finfc12)
#    # Fbar FC22
#    foutfc21 <- test_operatingModel_eval_om(flfs, flbs, fwc, "f", 2,1,1, dim_min, dim_max)
#    finfc21 <- test_operatingModel_fbar_FCB(flfs, flbs, fwc, 2,1,1, dim_min, dim_max) # Assume is correct
#    expect_FLQuant_equal(foutfc21,finfc21)

#    # SSB targets
#    # SRP B1
#    dim_min[4] <- 1 # Season 1 - should be OK
#    srp_outB1 <- test_operatingModel_eval_om(flfs, flbs, fwc, "ssb", as.integer(NA),as.integer(NA),1, dim_min, dim_max)
#    srp_inB1 <- test_operatingModel_SRP_FLQ_subset(flfs, flbs, fwc, 1, dim_min[-1], dim_max[-1])
#    expect_FLQuant_equal(srp_outB1,srp_inB1)
#    dim_min[4] <- 2 # Season 2 - first timestep has spwn = NA - throws error
#    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb", as.integer(NA),as.integer(NA),1, dim_min_temp, dim_max))
#
#    # SRP B2 - can only do a single timestep as spwn = 0
#    expect_error(test_operatingModel_eval_om(flfs, flbs, fwc, "ssb", as.integer(NA),as.integer(NA),2, dim_min, dim_max))
#    # Season 3, Year 1, all iters and units
#    dim_min <- c(1,1,1,3,1,1)
#    dim_max <- c(dim(flq)[1],1,dim(flq)[3],3,1,dim(flq)[6])
#    srp_outB2 <- test_operatingModel_eval_om(flfs, flbs, fwc, "ssb", as.integer(NA),as.integer(NA),2, dim_min, dim_max)
#    # Should be SRP in next timestep if spwn = 0
#    dim_min_plus <- c(1,1,1,4,1,1)
#    dim_max_plus <- c(dim(flq)[1],1,dim(flq)[3],4,1,dim(flq)[6])
#    # Set spwn to 0 etc
#    flbs_temp <- flbs
#    flbs_temp[[2]][["biol"]]@spwn[] <- 0.0
#    srp_inB2 <- test_operatingModel_SRP_FLQ_subset(flfs, flbs_temp, fwc, 2, dim_min_plus[-1], dim_max_plus[-1])
#    expect_FLQuant_equal(srp_outB2,srp_inB2)
#    # But spwn in returned object should be same as orig
#    om_out <- test_operatingModel_eval_om2(flfs, flbs, fwc, "ssb", as.integer(NA),as.integer(NA),2, dim_min, dim_max)
#    expect_FLQuant_equal(om_out[["biols"]][[2]]@spwn, flbs[[2]][["biol"]]@spwn)

#    # Economic indicators
#    for (fishery_no in 1:length(flfs)){
#        dim_max <- dim(landings.wt(flfs[[fishery_no]][[1]]))
#        dim_min <- round(runif(6,min=1,max=dim_max))
#        # Revenue at catch level
#        for(catch_no in 1:length(flfs[[fishery_no]])){
#            rev_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "revenue", fishery_no,catch_no,as.integer(NA), dim_min, dim_max)
#            rev_in2 <- test_FLCatch_revenue_subset(flfs[[fishery_no]][[catch_no]], dim_min[-1], dim_max[-1])
#            rev_in <- unitSums(quantSums((landings.n(flfs[[fishery_no]][[catch_no]]) * landings.wt(flfs[[fishery_no]][[catch_no]]) * price(flfs[[fishery_no]][[catch_no]]))[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]))
#            expect_FLQuant_equal(rev_in, rev_out)
#            expect_FLQuant_equal(rev_in2, rev_out)
#        }
#        # Revenue from fishery
#        rev_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "revenue", fishery_no, as.integer(NA),as.integer(NA), dim_min, dim_max)
#        rev_in2 <- test_FLFishery_revenue_subset(flfs[[fishery_no]], dim_min[-1], dim_max[-1])
#        rev_in <- lapply(flfs[[fishery_no]], function(x){return(unitSums(quantSums((landings.n(x) * landings.wt(x) * price(x))[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]])))})
#        rev_in <- Reduce("+",rev_in)
#        expect_FLQuant_equal(rev_in, rev_out)
#        expect_FLQuant_equal(rev_in2, rev_out)
#        # Effort from fishery
#        eff_out <- test_operatingModel_eval_om(flfs, flbs, fwc, "effort", fishery_no, as.integer(NA),as.integer(NA), dim_min, dim_max)
#        eff_in <- flfs[[fishery_no]]@effort[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]]
#        expect_FLQuant_equal(eff_in, eff_out)
#    }
#})

#test_that("operatingModel eval_om simple", {
#    # Based on ple4 simple
#    # Add more as target types added
#    niters <- 10 
#    data(ple4)
#    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
#    colnames(FCB) <- c("F","C","B")
#    om <- make_test_operatingModel(ple4, FCB, nseasons = 4, niters=niters)
#    dim_max <- dim(n(om[["biols"]][[1]][["biol"]]))
#    dim_min <- round(runif(6, min=1, max=dim_max))
#    # Catch
#    # 1, 1, 1 Cannot ask for biol and a catch
#    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", 1, 1, 1, dim_min, dim_max))
#    # 1, 2, NA
#    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", 1, 2, as.integer(NA), dim_min, dim_max)
#    cin <- catch(om[["fisheries"]][[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]] 
#    expect_FLQuant_equal(cout, cin)
#    # NA, NA, 2
#    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", as.integer(NA), as.integer(NA), 2, dim_min, dim_max)
#    cin <- test_operatingModel_catches_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, dim_min[-1], dim_max[-1])
#    expect_FLQuant_equal(cout, cin)
#    # NA, NA, 3 error
#    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", as.integer(NA), as.integer(NA), 3, dim_min, dim_max))
#    # If catch but no fishery - error
#    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "catch", as.integer(NA), 1, as.integer(NA), dim_min, dim_max))
#    # Landings
#    # 1, 1, 1 Cannot ask for biol and a catch
#    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", 1, 1, 1, dim_min, dim_max))
#    # 1, 2, NA
#    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", 1, 2, as.integer(NA), dim_min, dim_max)
#    cin <- landings(om[["fisheries"]][[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]] 
#    expect_FLQuant_equal(cout, cin)
#    # NA, NA, 2
#    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", as.integer(NA), as.integer(NA), 2, dim_min, dim_max)
#    cin <- test_operatingModel_landings_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, dim_min[-1], dim_max[-1])
#    expect_FLQuant_equal(cout, cin)
#    # NA, NA, 3 error
#    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "landings", as.integer(NA), as.integer(NA), 3, dim_min, dim_max))
#    # Discards
#    # 1, 1, 1 Cannot ask for biol and a catch
#    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", 1, 1, 1, dim_min, dim_max))
#    # 1, 2, NA
#    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", 1, 2, as.integer(NA), dim_min, dim_max)
#    cin <- discards(om[["fisheries"]][[1]][[2]])[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]] 
#    expect_FLQuant_equal(cout, cin)
#    # NA, NA, 2
#    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", as.integer(NA), as.integer(NA), 2, dim_min, dim_max)
#    cin <- test_operatingModel_discards_subset(om[["fisheries"]], om[["biols"]], om[["fwc"]], 2, dim_min[-1], dim_max[-1])
#    expect_FLQuant_equal(cout, cin)
#    # NA, NA, 3 error
#    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "discards", as.integer(NA), as.integer(NA), 3, dim_min, dim_max))
#    # Effort
#    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "effort", 1, as.integer(NA), as.integer(NA), dim_min, dim_max)
#    expect_FLQuant_equal(om[["fisheries"]][[1]]@effort[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], cout)
#    cout <- test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "effort", 2, as.integer(NA), as.integer(NA), dim_min, dim_max)
#    expect_FLQuant_equal(om[["fisheries"]][[2]]@effort[,dim_min[2]:dim_max[2], dim_min[3]:dim_max[3], dim_min[4]:dim_max[4], dim_min[5]:dim_max[5], dim_min[6]:dim_max[6]], cout)
#    # Effort but no fishery no specified
#    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "effort", as.integer(NA), as.integer(NA), 1, dim_min, dim_max))
#    expect_error(test_operatingModel_eval_om(om[["fisheries"]], om[["biols"]], om[["fwc"]], "effort", as.integer(NA), 1, as.integer(NA), dim_min, dim_max))
#})

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

#    # SSB target - try each season in year 1
#    years <- 1
#    seasons <- c(1,1,2,2,3,3,4,4)
#    ssb_trgt1 <- data.frame(year = years, season = seasons, 
#                        quant = "ssb",
#                        fishery = NA, catch = NA, biol = rep(c(1,2),4))
#    fwc <- fwdControl(ssb_trgt1)
#    fwc@target$order <- seasons
#    fwc@FCB <- FCB
#    # Season 1 - Biol 1 - Spwn > 0
#    val_hat1 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 1, 1)
#    val_in1 <- c(test_operatingModel_SRP_FLQ_subset(flfs, flbs, fwc, 1, c(years[1],1,seasons[1],1,1), c(years[1],1,seasons[1],1,niters)))
#    expect_equal(val_hat1, val_in1)
#    # Season 1 - Biol 2 - Spwn is NA - error
#    expect_error(test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 1, 2))
#    # Seasons 2 and 4 - both Biols Spwn is NA
#    expect_error(test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 2, 1))
#    expect_error(test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 2, 2))
#    expect_error(test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 4, 1))
#    expect_error(test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 4, 2))
#    # Season 3 - Biol 1 - Spwn is NA
#    expect_error(test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 3, 1))
#    # Season 3 - Biol 2 - Spwn is 0 so we get the SRP at the start of season 4
#    val_hat2 <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 3, 2)
#    # Spwn at start of season 4 - need to set spwn to 0 in season 4 as currently in NA
#    flbs_temp <- flbs
#    flbs_temp[[2]][["biol"]]@spwn[] <- 0.0
#    val_in2 <- c(test_operatingModel_SRP_FLQ_subset(flfs, flbs_temp, fwc, 2, c(years[1],1,seasons[6]+1,1,1), c(years[1],1,seasons[6]+1,1,niters)))
#    expect_equal(val_hat2, val_in2)
#
#    # Relative SSB target - SSB relative to year before
#    ssb_trgt2 <- data.frame(year = 2, season = 1, 
#                            relYear = 1, relSeason = 1,
#                        quant = "ssb",
#                        fishery = NA, catch = NA, biol = 1,
#                        relFishery = NA, relCatch = NA, relBiol = 1)
#    fwc <- fwdControl(ssb_trgt2)
#    fwc@target$order <- 1
#    fwc@FCB <- FCB
#    val_hat <- test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 1, 1)
#    val_in1 <- c(test_operatingModel_SRP_FLQ_subset(flfs, flbs, fwc, 1, c(1,1,1,1,1), c(1,1,1,1,niters)))
#    val_in2 <- c(test_operatingModel_SRP_FLQ_subset(flfs, flbs, fwc, 1, c(2,1,1,1,1), c(2,1,1,1,niters)))
#    expect_equal(val_in2 / val_in1, val_hat)
#
#    # Relative SSB target - SSB relative to season before - fails
#    ssb_trgt3 <- data.frame(year = 2, season = 1, 
#                            relYear = 1, relSeason = 4,
#                        quant = "ssb",
#                        fishery = NA, catch = NA, biol = 1,
#                        relFishery = NA, relCatch = NA, relBiol = 1)
#    fwc <- fwdControl(ssb_trgt3)
#    fwc@target$order <- 1
#    fwc@FCB <- FCB
#    expect_error(test_operatingModel_get_target_value_hat(flfs, flbs, fwc, 1, 1))
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
