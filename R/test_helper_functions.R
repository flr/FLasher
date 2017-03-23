# 
# Copyright 2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, JRC
#

#' Generate randomly sized and filled FLQuant objects
#'
#' Generate a randomly or fixed sized FLQuant filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing.
#' 
#' @param fixed_dims A vector of length 6 with the fixed length of each of the FLQuant dimensions. If any value is NA it is randomly set using the max_dims argument. Default value is rep(NA,6).
#' @param min_dims A vector of length 6 with minimum size of each of the FLQuant dimensions. Default value is c(1,1,1,1,1,1).
#' @param max_dims A vector of length 6 with maximum size of each of the FLQuant dimensions. Default value is c(5,10,5,4,4,5).
#' @param min_age_name The name of the first age group.
#' @param sd The standard deviation of the random numbers. Passed to rnorm() Default is 100.
#' @export
#' @return An FLQuant
#' @examples
#' flq <- random_FLQuant_generator()
#' dim(flq)
#' summary(flq)
#' flq <- random_FLQuant_generator(fixed_dims = c(NA,10,1,4,1,NA))
#' dim(flq)
#' summary(flq)
random_FLQuant_generator <- function(fixed_dims = rep(NA,6), min_dims = rep(1,6), max_dims = pmax(min_dims, c(5,10,5,4,4,5)), min_age_name = 1, sd = 100){
    flq_dims <- fixed_dims
    for (i in 1:6){
        if (is.na(fixed_dims[i])){
            flq_dims[i] <- round(runif(1,min=min_dims[i], max=max_dims[i]))
        }
    }
    values <- rnorm(prod(flq_dims), sd = sd)
    flq <- FLQuant(values, dimnames = list(age = min_age_name:(min_age_name + flq_dims[1] - 1), year = 1:flq_dims[2], unit = 1:flq_dims[3], season = 1:flq_dims[4], area = 1:flq_dims[5], iter = 1:flq_dims[6]))
    units(flq) <- as.character(signif(abs(rnorm(1)),3))
    return(flq)
}

#' Generate lists of randomly sized and filled FLQuant objects
#'
#' Generate a list of FLQuant objects filled with normally distributed random numbers with a mean of 0.
#' FLQuant objects can be randomly sized, depening on arguments passed to random_FLQuant_generator().
#' Used for automatic testing, particularly of the FLQuant7_base<T> class in CPP.
#' 
#' @param min_elements The minimum number of elements in the list. Default is 1. 
#' @param max_elements The maximum number of elements in the list. Default is 10. 
#' @param ... Other arguments to pass to random_FLQuant_generator(), e.g. those that fix the size of the objects.
#' @export
#' @return A list of FLQuant objects
#' @examples
#' flq_list <- random_FLQuant_list_generator()
#' length(flq_list)
#' summary(flq_list)
#' lapply(flq_list, summary)
random_FLQuant_list_generator <- function(min_elements = 1, max_elements = 10, ...){
    nelements <- round(runif(1,min=min_elements, max=max_elements))
    op <- list()
    for (i in 1:nelements){
        op[[i]] <- random_FLQuant_generator(...)
    }
    return(op)
}

#' Generate randomly sized and filled FLBiolcpp objects
#'
#' Generate an FLBiolcpp of random size and filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing, particularly of the fwdBiol class in CPP.
#' 
#' @param sd The standard deviation of the random numbers. Passed to rnorm() Default is 100.
#' @param ... Other arguments to pass to random_FLQuant_generator().
#' @export
#' @return An FLBiolcpp
#' @examples
#' flb <- random_FLBiolcpp_generator()
#' summary(flb)
random_FLBiolcpp_generator <- function(sd=100, ...){
    flq <- abs(random_FLQuant_generator(sd=sd, ...))
    biol <- FLBiol(n = flq)
    biol <- as(biol, "FLBiolcpp")
    biol@n <- biol@n * 1000000
    biol@m[] <- abs(rnorm(prod(dim(flq)),sd=sd)) / sd
    biol@wt[] <- abs(rnorm(prod(dim(flq)),sd=sd))
    biol@fec[] <- abs(rnorm(prod(dim(flq)),sd=sd))
    biol@mat[] <- abs(rnorm(prod(dim(flq)),sd=sd))
    biol@spwn[] <- runif(prod(dim(biol@spwn)), min=0, max=1)
    name(biol) <- as.character(signif(rnorm(1)*1000,3))
    desc(biol) <- as.character(signif(rnorm(1)*1000,3))
    # Set the units to something sensible
    units(biol@m) <- "m"
    units(biol@wt) <- "kg"
    units(biol@fec) <- "prop"
    units(biol@mat) <- "prop"
    units(biol@spwn) <- "prop"
    units(biol@n) <- "10^3"
    return(biol)
}

#' Generate randomly sized and filled FLCatch objects
#'
#' Generate an FLCatch of random size and filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing, particularly of the FLCatch class in CPP.
#' 
#' @param sd The standard deviation of the random numbers. Passed to rnorm() Default is 100.
#' @param ... Other arguments passed to random_FLQuant_generator().
#' @export
#' @return An FLCatch
#' @examples
#' flc <- random_FLCatch_generator()
#' summary(flc)
random_FLCatch_generator <- function(sd=100, ...){
    flq <- abs(random_FLQuant_generator(sd=sd, ...))
    catch <- FLCatch(landings.n = flq)
    landings.wt(catch)[] <- abs(rnorm(prod(dim(flq)),sd=sd))
    discards.n(catch)[] <- abs(rnorm(prod(dim(flq)),sd=sd))
    discards.wt(catch)[] <- abs(rnorm(prod(dim(flq)),sd=sd))
    catch.sel(catch)[] <- abs(rnorm(prod(dim(flq)),sd=sd))
    price(catch)[] <- abs(rnorm(prod(dim(flq)),sd=sd))
    catch.q(catch) <- FLPar(abs(rnorm(2 * dim(flq)[6])), dimnames = list(params = c("alpha","beta"), iter = 1:dim(flq)[6]))
    catch.q(catch)["beta",] <- 0.0
    name(catch) <- as.character(signif(rnorm(1)*1000,3))
    desc(catch) <- as.character(signif(rnorm(1)*1000,3))
    # set the units to something sensible
    units(landings.wt(catch)) <- "kg"
    units(discards.wt(catch)) <- "kg"
    units(landings.n(catch)) <- "10^3"
    units(discards.n(catch)) <- "10^3"
    units(catch.sel(catch)) <- "prop"
    return(catch)
}

#' Generates an FLCatches object - a list of randomly sized and filled FLCatch objects 
#'
#' Generates a list of identically sized FLCatch objects filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing, particularly of the FLCatches_base<T> class in CPP.
#' 
#' @param min_catches The minimum number of catches. Default is 2. 
#' @param max_catches The maximum number of catches. Default is 5. 
#' @param ... Other arguments passed to random_FLQuant_generator().
#' @export
#' @return An FLCatches objects
#' @examples
#' flcs <- random_FLCatches_generator()
#' length(flcs)
#' summary(flcs)
#' lapply(flcs, summary)
random_FLCatches_generator <- function(min_catches = 2, max_catches = 5, ...){
    ncatches <- round(runif(1,min=min_catches, max=max_catches))
    op_list <- list()
    flq <- random_FLQuant_generator(...)
    # Catches are the same size - good if they could vary in the first dim
    fixed_dims <- dim(flq)
    args <- list(...)
    args[["fixed_dims"]] <- fixed_dims
    for (i in 1:ncatches){
        op_list[[i]] <- do.call(random_FLCatch_generator,args)    
    }
    names(op_list) <- paste("Catch ",as.character(1:ncatches),sep="")
    op <- FLCatches(op_list)
    op@desc <- as.character(signif(rnorm(1)*1000,3))
    return(op)
}

#' Generate a randomly filled and sized FLFishery object
#'
#' Generate a randomly sized FLFishery object filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing, particularly of the FLFishery_base<T> class in CPP.
#' 
#' @param min_catches The minimum number of catches. Default is 2. 
#' @param max_catches The maximum number of FLCatches in the catches list. Default is 5. 
#' @param sd Standard deviation of the randomly generated FLQuant slots.
#' @param ... Other arguments passed to random_FLCatches_generator().
#' @export
#' @return An FLFishery object 
#' @examples
#' flf <- random_FLFishery_generator(fixed_dims = c(NA,10,1,1,1,1))
#' lapply(flf, summary)
#' flf <- random_FLFishery_generator(fixed_dims = c(NA,10,1,1,1,1), max_dims = c(100,NA,NA,NA,NA,NA))
random_FLFishery_generator <- function(min_catches = 2, max_catches = 5, sd = 1, ...){
    catches <- random_FLCatches_generator(min_catches, max_catches, sd=sd, ...)
    fishery <- FLFishery(catches)
    fishery@hperiod[1,] <- runif(prod(dim(fishery@hperiod)[2:6]),min=0, max=1)
    fishery@hperiod[2,] <- runif(prod(dim(fishery@hperiod)[2:6]),min=fishery@hperiod[1,], max=1)
    # hperiod should only have length 1 in the unit dimension
    fishery@hperiod <- fishery@hperiod[,,1,]
    # Effort should only have length 1 in the unit dimension
    dimnames <- dimnames(fishery@effort)
    dimnames[[3]] <- "all"
    fishery@effort <- FLQuant(NA, dimnames=dimnames)
    # fill up effort, vcost and fcost
    fishery@effort <- FLQuant(abs(rnorm(prod(dim(fishery@effort)),sd=sd)), dimnames=dimnames)
    vcost(fishery)[] <- abs(rnorm(prod(dim(vcost(fishery))),sd=sd))
    fcost(fishery)[] <- abs(rnorm(prod(dim(fcost(fishery))),sd=sd))
    fishery@desc <- as.character(signif(rnorm(1)*1000,3))
    fishery@name <- as.character(signif(rnorm(1)*1000,3))
    return(fishery)
}

#' Generate a randomly filled and sized FLFisheries object
#'
#' Generate a randomly sized FLFisheries object filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing, particularly of the FLFisheries_base<T> class in CPP.
#' 
#' @param min_fisheries The minimum number of FLFisheries in the fisheries list. Default is 2. 
#' @param max_fisheries The maximum number of FLFisheries in the fisheries list. Default is 5. 
#' @param ... Other arguments to pass to random_FLFishery_generator().
#' @export
#' @return An FLFishery object 
#' @examples
#' flf <- random_FLFisheries_generator(fixed_dims = c(NA,10,1,1,1,1))
#' lapply(flf, summary)
random_FLFisheries_generator <- function(min_fisheries = 2, max_fisheries = 5, ...){
    fisheries_list <- list()
    nfisheries <- round(runif(1,min=min_fisheries, max=max_fisheries))
    for (i in 1:nfisheries){
        fisheries_list[[i]] <- random_FLFishery_generator(...)
    }
    names(fisheries_list) <- paste("Fishery ",as.character(1:nfisheries),sep="")
    fisheries <- FLFisheries(fisheries_list)
    fisheries@desc <- as.character(signif(rnorm(1)*1000,3))
    return(fisheries)
}

#' Generates a list that can be passed to the CPP fwdBiols constructor 
#'
#' The fwdBiols constructor takes a list (fwdbiols_list). Each element of fwdbiols_list is a list of:
#' FLBiolcpp, SRR model name, SRR params, SRR timelag, SRR residuals and SRR residuals mult.
#' This function generates randomly filled FLBiolcpp objects. Objects may be of different sizes unless appropriate arguments to random_FLBiolcpp_generator() are specified.
#' Used for automatic testing, particularly of the fwdBiols<T> class in CPP.
#' 
#' @param min_biols The minimum number of fwdBiols in the list. Default is 1. 
#' @param max_biols The maximum number of fwdBiols in the list. Default is 5. 
#' @param ... Other arguments passed to random_FLBiolcpp_generator().
#' @export
#' @return A list object 
#' @examples
#' fwdBiols <- random_fwdBiols_list_generator()
random_fwdBiols_list_generator <- function(min_biols = 1, max_biols = 5, ...){
    nbiols <- round(runif(1,min=min_biols,max=max_biols))
    biols <- list()
    for (i in 1:nbiols){
        biol_bits <- list()
        biol_bits[["biol"]] <- random_FLBiolcpp_generator(...)
        biol_bits[["srr_model_name"]] <- "bevholt"
        biol_bits[["srr_params"]] <- FLQuant(abs(rnorm(2)), dimnames=list(params=c("a","b")))
        biol_bits[["srr_residuals"]] <- n(biol_bits[["biol"]])[1,]
        biol_bits[["srr_residuals_mult"]] <- TRUE
        biols[[as.character(signif(abs(runif(1,min=100,max=999)),3))]] <- biol_bits
    }
    return(biols)
}


#' Random fwdControl object creator
#'
#' Creates a random fwdControl object for testing purposes
#'
#' @param years numeric vector of years in the control object. Default value is 1:random interger (max = 10).
#' @param nseasons number of seasons in the projection
#' @param max_nsim_target maximum number of simultaneous targets in each timestep
#' @param niters the number of iterations. Default number is random integer (max = 10).
#' 
#' @export
#' @return A fwdControl object
random_fwdControl_generator <- function(years = 1:round(runif(1, min=2,max=3)), nseasons = 2, max_nsim_target = 3, niters = round(runif(1,min=5,max=10))){
    quantities <- c("f","catch","landings","discards")#, "ssb", "biomass")
    abundance_quantities <- c("ssb", "biomass", "f")
    f_quantities <- c("f") # either B, or FCB
    # Randomly set simultaneous targets
    nsim_each <- round(runif(length(years) * nseasons, min = 1, max = max_nsim_target))
    years_col <- rep(years, each = nseasons)
    years_col <- rep(years_col, nsim_each)
    seasons_col <- rep(1:nseasons, length(years))
    seasons_col <- rep(seasons_col, nsim_each)
    target <- data.frame(year=years_col,
                          quant=NA,
                          season = seasons_col,
                          minAge = 1,
                          maxAge = 5,
                          fishery = NA,
                          catch = NA,
                          biol = NA
                          )
    target$minAge <- as.integer(round(runif(dim(target)[1], min=1, max = 10)))
    target$maxAge <- as.integer(target$minAge * 2)
    # Randomly pick some quantities
    target$quant <- quantities[round(runif(nrow(target),min=1,max=length(quantities)))]
    # But force last one to be an abundance target to help with testing
    #target$quant[length(target$quant)] <- "biomass"
    # And for catch too
    target$quant[length(target$quant)-1] <- "catch"
    # Randomly set FCB (based on target)
    # Fishery targets
    fish_targets <- which(!(target$quant %in% abundance_quantities))
    # Half of these are FC, the others B
    FC_targets <- sample(fish_targets, ceiling(length(fish_targets) / 2))
    B_targets <- fish_targets[!(fish_targets %in% FC_targets)]
    target[FC_targets,c("fishery")] <- round(runif(length(FC_targets), min = 1, max = 2))
    target[FC_targets,c("catch")] <- round(runif(length(FC_targets), min = 1, max = 2))
    target[B_targets,"biol"] <- round(runif(length(B_targets), min = 1, max = 3))
    # Abundance targets are biol only
    biol_targets <- which(target$quant %in% abundance_quantities)
    target[biol_targets, "biol"] <- round(runif(length(biol_targets), min = 1, max = 3))
    # Fix f targets, either B, or FCB
    f_targets <- which(target$quant %in% f_quantities)
    f_FCB_targets <- sample(f_targets, ceiling(length(f_targets) / 2))
    if (length(f_FCB_targets > 0)){
        target[f_FCB_targets,c("fishery","catch")] <- round(runif(length(f_FCB_targets)*2, min = 1, max = 3))
    }
    # Force integers - should be done in fwd() dispatch or constructor
    target$fishery <- as.integer(target$fishery)
    target$catch <- as.integer(target$catch)
    target$biol <- as.integer(target$biol)
    target$year <- as.integer(target$year)
    target$season <- as.integer(target$season)
    target$quant <- as.character(target$quant)
    # Some targets are min and max 
    min_max_row <- sample(1:nrow(target), ceiling(nrow(target) / 2))
    min_row <- min_max_row[1:(length(min_max_row)/2)]
    max_row <- min_max_row[!(min_max_row %in% min_row)]
    value_row <- which(!( (1:nrow(target)) %in% min_max_row))
    # Make iter values - better creator than this too
    target_iters <- array(NA, dim=c(nrow(target),3,niters), dimnames=list(row=1:nrow(target), val=c("min","value","max"), iter=1:niters))
    target_iters[value_row,"value",] <- runif(niters, min=0.3, max=0.4)
    target_iters[min_row,"min",] <- runif(niters, min=0.3, max=0.4)
    target_iters[max_row,"max",] <- runif(niters, min=0.3, max=0.4)

    # Order the targets
    #target <- target[order(target$year, target$season),]

    # Add order column - should group targets with same year and season together
    # Make it random so that get_target_row is properly tested
    #target$order <- sample(1:nrow(target), nrow(target))

    # Data.frame constructor - use other constructor here?
    fwc <- fwdControl(target=target, iters=target_iters)

    # Add FCB array 
    FCB <- array(c(1,1,2,2,1,2,1,2,1,2,2,3), dim=c(4,3))
    colnames(FCB) <- c("F","C","B")
    fwc@FCB <- FCB
    return(fwc)
}

#' Make a test operating model from a single FLStock object
#'
#' Number of fisheries, catches and biols are taken from the FCB argument.
#' @param fls The FLStock that the OM is based on
#' @param FCB The FCB matrix
#' @param nseasons The number of seasons
#' @param spawning_seasons A vector of seasons in which spawning occurs (1 - 4)
#' @param recruitment_age The age of recruitment to the fishery
#' @param niters The number of iterations. 
#' @param sd The standard deviation when applying random lognormal noise to some of the slots.
#'
#' @export
#' @return A list of objects for sending to C++
make_test_operatingModel <- function(fls, FCB, nseasons = 1, recruitment_seasons = 1, recruitment_age = 1, niters = 1000, sd = 0.1){
    # Interrogate FCB to get number of biols, fisheries and catches
    nbiols <- max(FCB[,"B"])
    nfisheries <- max(FCB[,"F"])
    # Get the right dimensions for the FLQuant slots
    # Could base all this on LH: pass in Linf / K / t0 and LW (a and b) to get weights and m
    dmns <- dimnames(stock.n(fls))
    dmns$season <- 1:nseasons
    nunits <- length(recruitment_seasons)
    dmns$unit <- 1:nunits # Each spawning season gets its own unit
    dmns$age <- as.character(recruitment_age:(recruitment_age + length(dmns$age) - 1))
    dmns$iter <- as.character(1:niters)
    seed_flq <- FLQuant(NA, dimnames=dmns)
    # Make the biols
    # Same SRR for all
    srr <- fmle(as.FLSR(fls, model="bevholt"),control = list(trace=0))
    res_temp <- window(exp(residuals(srr)), start = 1957)
    res_temp[,"1957"] <- res_temp[,"1958"]
    res_temp <- propagate(res_temp, niters)
    res_dmns <- dmns
    res_dmns$age <- "all"
    res <- FLQuant(NA, dimnames = res_dmns)
    res[] <- res_temp
    res <- res * abs(rnorm(prod(dim(res)), mean = 1, sd = sd))
    srr_params <- FLQuant(NA, dimnames=list(params = c("a","b"), unit = 1:nunits, season = 1:nseasons, iter=1:niters))
    for (unit_count in 1:length(recruitment_seasons)){
        srr_params[,,unit_count,recruitment_seasons[unit_count]] <- params(srr)
    }
    srr_params <- srr_params * abs(rnorm(prod(dim(srr_params)), mean = 1, sd = sd))
    biols <- list()
    for (bno in 1:nbiols){
        newb <- FLBiol(n=seed_flq)
        n(newb)[] <- stock.n(fls)
        n(newb) <- n(newb) * abs(rnorm(prod(dim(n(newb))), mean = 1, sd = sd))
        m(newb)[] <- m(fls)
        m(newb) <- m(newb) * abs(rnorm(prod(dim(m(newb))), mean = 1, sd = sd))
        wt(newb)[] <- stock.wt(fls)
        wt(newb) <- wt(newb) * abs(rnorm(prod(dim(wt(newb))), mean = 1, sd = sd))
        mat(newb)[] <- mat(fls)
        mat(newb) <- mat(newb) * abs(rnorm(prod(dim(mat(newb))), mean = 1, sd = sd))
        # fec - what does this do?

        # Spawning depends on first age.
        # If first age > 0, spawning occurs nseasons * first age ago
        # If first age == 0 and model is annual, spawning occurs in same timestep as recruitment
        # If first age == 0 and model is seasonal, spawning is timestep - 1
        if (recruitment_age > 0){
            spawning_seasons <- recruitment_seasons #  but a year lag
        }
        if (recruitment_age == 0 & nseasons == 1){
            spawning_seasons <- 1
        }
        if (recruitment_age == 0 & nseasons > 1){
            spawning_seasons = recruitment_seasons - 1
            # If less than 1, then it's the max season in last year
            spawning_seasons[spawning_seasons < 1] <- nseasons
        }
        # spwn - if it spawns then it does so at beginning of the season - all units set to the same
        spwn(newb)[,,,spawning_seasons,] <- 0
        newb <- as(newb, "FLBiolcpp")
        name(newb) <- paste("biol", bno, sep="")
        desc(newb) <- paste("biol", bno, sep="")
        # Add noise to residuals
        res_temp <- res * abs(rnorm(prod(dim(res)), mean = 1, sd = sd))
        # Make the list of FLBiolcpp bits
        biol_bits <- list(biol = newb, srr_model_name = "bevholt", srr_params = srr_params, srr_residuals = res_temp, srr_residuals_mult = TRUE)
        biols[[paste("biol", bno, sep="")]] <- biol_bits
    }
    # Make the fisheries
    flfs <- list()
    for (fno in 1:nfisheries){
        # Make the catches of the fishery
        ncatches <- max(FCB[FCB[,"F"]==1,"C"]) 
        catches <- new("FLCatches")
        for (cno in 1:ncatches){
            newc <- FLCatch(landings.n=seed_flq)
            name(newc) <- paste("catch", fno, cno, sep="")
            desc(newc) <- paste("catch", fno, cno, sep="")
            landings.n(newc)[] <- landings.n(fls)
            landings.n(newc) <- landings.n(newc) * abs(rnorm(prod(dim(landings.n(newc))), mean = 1, sd = sd))
            landings.wt(newc)[] <- landings.wt(fls)
            landings.wt(newc) <- landings.wt(newc) * abs(rnorm(prod(dim(landings.wt(newc))), mean = 1, sd = sd))
            discards.n(newc)[] <- discards.n(fls)
            discards.n(newc) <- discards.n(newc) * abs(rnorm(prod(dim(discards.n(newc))), mean = 1, sd = sd))
            discards.wt(newc)[] <- discards.wt(fls)
            discards.wt(newc) <- discards.wt(newc) * abs(rnorm(prod(dim(discards.wt(newc))), mean = 1, sd = sd))
            # selectivity based on harvest then scaled so max is 1
            catch.sel(newc)[] <- harvest(fls)
            catch.sel(newc) <- catch.sel(newc) * abs(rnorm(prod(dim(catch.sel(newc))), mean = 1, sd = sd))
            catch.sel(newc)[] <- c(apply(catch.sel(newc), 2:6, function(x) x / max(x))) # Weird apply bug
            # no beta parameter atm
            catch.q(newc) <- FLPar(c(1.0), dimnames=list(params=c("alpha", "beta"), iter = 1))
            catch.q(newc)["beta",] <- 0.0 # Fix F calculation in code
            # price - just fill with +ve rnorm
            price(newc)[] <- abs(rnorm(prod(dim(price(newc))), mean=1, sd=sd))
            catches[[paste("catch", cno, sep="")]] <- newc
        }
        newf <- FLFishery(catches) # range not set correctly
        desc(newf) <- paste("fishery", fno, sep="")
        name(newf) <- paste("fishery", fno, sep="")
        # Effort is only allowed to have 1 unit
        newf@effort <- newf@effort[,,1,,,]
        newf@effort[] <- abs(rnorm(prod(dim(newf@effort)),mean=1,sd=sd))
        # Fish all through the season
        hperiod(newf)[1,] <- 0
        hperiod(newf)[2,] <- 1
        flfs[[paste("fishery", fno, sep="")]] <- newf
    }
    fisheries <- FLFisheries(flfs)
    fisheries@desc <- "fisheries"
    # Add a random control for completeness
    fwc <- random_fwdControl_generator(niters=niters)
    fwc@FCB <- FCB
    return(list(fisheries = fisheries, biols=biols, fwc=fwc))
}

#' Create an operating model based on Indian Ocean skipjack tuna
#'
#' Two FLFishery objects with 1 FLCatch each fishing on a single FLBiolcpp.
#' The model has 4 seasons with 2 units.
#' Spawning happens in seasons 1 and 3.
#' @param niters The number of iterations. 
#' @param sd The standard deviation when applying random lognormal noise to some of the slots.
#'
#' @export
#' @return A list of objects for sending to C++
make_skipjack_operatingModel <- function(niters = 1000, sd = 0.1){
    data(skjBiol)
    # Make the biol
    biol <- FLBiol(wt=skj[["wt"]])
    biol <- as(biol, "FLBiolcpp")
    mat(biol) <- skj[["mat"]]
    m(biol) <- skj[["m"]]
    # spwn used to have only 1 age, This has many 
    spwn(biol) <- skj[["spwn"]]
    # Blow up to have iters
    biol <- propagate(biol, niters)
    # Add noise to wts
    wt(biol) <- rlnorm(wt(biol), sdlog=sd)
    # SRR Residuals
    dim <- dim(n(biol))
    dim[1] <- 1
    res1 <- FLQuant(1, dim=dim)
    # Make the biol bits
    srr_model_name <- srr[["model"]]
    srr_params <- as(srr[["params"]], "FLQuant")
    biol_bits1 <- list(biol = biol, srr_model_name = srr_model_name, srr_params = srr_params, srr_residuals = res1, srr_residuals_mult = TRUE)
    biols <- list(biol1 = biol_bits1)

    # Make the Fisheries
    # Fishery 1
    catch1 <- FLCatch(landings.wt = wt(biol), discards.wt = wt(biol), name="Catch1", desc="Prince Fatty")
    sel <- 1 / (1 + exp(-0.5 * (1:10 - 5))) # logisitic
    catch.sel(catch1)[] <- sel # Ideally based on F
    catch.q(catch1) <- FLPar(alpha = 1, beta=0.1) # Same catchability on both units
    catch.q(catch1)["beta",] <- 0.0
    fishery1 <- FLFishery(skj = catch1, name="Cork Ball Dub", desc="Brute Dub")
    # Force effort to have 1 unit
    dmns <- dimnames(effort(fishery1))
    dmns[[3]] <- "all"
    effort(fishery1) <- FLQuant(1, dimnames=dmns)

    # Fishery 2
    catch2 <- FLCatch(landings.wt = wt(biol), discards.wt = wt(biol), name="Catch2", desc="Back off Dub")
    sel <- 1 / (1 + exp(-1 * (1:10 - 5))) # logisitic
    catch.sel(catch2)[] <- sel # Ideally based on F
    catch.q(catch2) <- FLPar(alpha = 1.5, beta=0.05)
    catch.q(catch2)["beta",] <- 0.0
    fishery2 <- FLFishery(skj = catch2, name="Torah Dub", desc="Devil Dub")
    # Force effort to have 1 unit
    dmns <- dimnames(effort(fishery2))
    dmns[[3]] <- "all"
    effort(fishery2) <- FLQuant(1, dimnames=dmns)
    # Fisheries
    fisheries <- FLFisheries(fishery1 = fishery1, fishery2 = fishery2)
    fisheries@desc <- "Idi Amin Dub"

    # fwdControl
    fwc <- random_fwdControl_generator(niters=niters)
    # Make the FCB matrix
    FCB <- array(c(1,2,1,1,1,1), dim=c(2,3))
    colnames(FCB) <- c("F","C","B")
    fwc@FCB <- FCB
    return(list(fisheries = fisheries, biols = biols, fwc = fwc))
}



#' Return 1D element index of an FLQuant
#'
#' Given an FLQuant and the indices, returns the 1D element accessor.
#' @param flq The FLQuant
#' @param indices The indices (integer vector, length 6)
#'
#' @export
get_FLQuant_element <- function(flq, indices){
    dim <- dim(flq)
	element <- indices[1] +
        (dim[1] * (indices[2]-1)) +
        (dim[2] * dim[1] * (indices[3]-1)) +
        (dim[3] * dim[2] * dim[1] * (indices[4]-1)) +
        (dim[4] * dim[3] * dim[2] * dim[1] * (indices[5]-1)) +
        (dim[5] * dim[4] * dim[3] * dim[2] * dim[1] * (indices[6]-1)) 
    return(element)
}

#' Return 1D vector of element indices of an FLQuant
#'
#' Given an FLQuant and the indices range, returns the vector of indices
#' @param flq The FLQuant
#' @param indices_min The min indices (integer vector, length 6)
#' @param indices_max The max indices (integer vector, length 6)
#'
#' @export
get_FLQuant_elements <- function(flq, indices_min, indices_max){
    length_out <- prod(indices_max - indices_min + 1)
    elements <- rep(NA, length_out)
    element_count <- 0
    for (icount in indices_min[6]:indices_max[6]){
        for (acount in indices_min[5]:indices_max[5]){
            for (scount in indices_min[4]:indices_max[4]){
                for (ucount in indices_min[3]:indices_max[3]){
                    for (ycount in indices_min[2]:indices_max[2]){
                        for (qcount in indices_min[1]:indices_max[1]){
                            element_count = element_count + 1
                            elements[element_count] = get_FLQuant_element(flq, c(qcount, ycount, ucount, scount, acount, icount))
    }}}}}}
    return(elements)
}

