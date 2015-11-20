# 
# Copyright 2013 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Finlay Scott, JRC
#

#' Generate randomly sized and filled FLQuant objects
#'
#' Generate a randomly or fixed sized FLQuant filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing.
#' All dimensions have at least length 2.
#' 
#' @param fixed_dims A vector of length 6 with the fixed length of each of the FLQuant dimensions. If any value is NA it is randomly set using the max_dims argument. Default value is rep(NA,6).
#' @param min_dims A vector of length 6 with minimum size of each of the FLQuant dimensions. Default value is c(1,1,1,1,1,1).
#' @param max_dims A vector of length 6 with maximum size of each of the FLQuant dimensions. Default value is c(5,10,5,4,4,5).
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
random_FLQuant_generator <- function(fixed_dims = rep(NA,6), min_dims = rep(1,6), max_dims = c(5,10,5,4,4,5), sd = 100){
    nage <- ifelse(is.na(fixed_dims[1]),round(runif(1,min=min_dims[1], max=max_dims[1])),fixed_dims[1])
    nyear <- ifelse(is.na(fixed_dims[2]),round(runif(1,min=min_dims[2], max=max_dims[2])),fixed_dims[2])
    nunit <- ifelse(is.na(fixed_dims[3]),round(runif(1,min=min_dims[3], max=max_dims[3])),fixed_dims[3])
    nseason <- ifelse(is.na(fixed_dims[4]),round(runif(1,min=min_dims[4], max=max_dims[4])),fixed_dims[4])
    narea <- ifelse(is.na(fixed_dims[5]),round(runif(1,min=min_dims[5], max=max_dims[5])),fixed_dims[5])
    niter <- ifelse(is.na(fixed_dims[6]),round(runif(1,min=min_dims[6], max=max_dims[6])),fixed_dims[6])
    values <- rnorm(nage*nyear*nunit*nseason*narea*niter, sd = sd)
    flq <- FLQuant(values, dimnames = list(age = 1:nage, year = 1:nyear, unit = 1:nunit, season = 1:nseason, area = 1:narea, iter = 1:niter))
    units(flq) <- as.character(signif(abs(rnorm(1)),3))
    return(flq)
}

#' Generate lists of randomly sized and filled FLQuant objects
#'
#' Generate a list of randomly sized FLQuant objects filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing, particularly of the FLQuant7_base<T> class in CPP.
#' 
#' @param min_elements The minimum number of elements in the list. Default is 1. 
#' @param max_elements The maximum number of elements in the list. Default is 10. 
#' @param fixed_dims A vector of length 6 with the fixed length of each of the FLQuant dimensions. If any value is NA it is randomly set using the max_dims argument. Default value is rep(NA,6).
#' @param max_dims A vector of length 6 with maximum size of each of the FLQuant dimensions. Default value is c(5,5,5,4,4,10).
#' @param sd The standard deviation of the random numbers. Passed to rnorm() Default is 100.
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

#' Generate randomly sized and filled FLBiol objects
#'
#' Generate an FLBiol of random size and filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing, particularly of the fwdBiol class in CPP.
#' 
#' @param fixed_dims A vector of length 6 with the fixed length of each of the FLQuant dimensions. If any value is NA it is randomly set using the max_dims argument. Default value is rep(NA,6).
#' @param max_dims A vector of length 6 with maximum size of each of the FLQuant dimensions. Default value is c(5,5,5,4,4,10).
#' @param sd The standard deviation of the random numbers. Passed to rnorm() Default is 100.
#' @export
#' @return An FLBiol
#' @examples
#' flb <- random_FLBiol_generator()
#' summary(flb)
random_FLBiol_generator <- function(sd=100, ...){
    flq <- abs(random_FLQuant_generator(sd=sd, ...))
    biol <- FLBiol(n = flq)
    m(biol) <- abs(rnorm(prod(dim(flq)),sd=sd))
    wt(biol) <- abs(rnorm(prod(dim(flq)),sd=sd))
    fec(biol) <- abs(rnorm(prod(dim(flq)),sd=sd))
    mat(biol) <- abs(rnorm(prod(dim(flq)),sd=sd))
    spwn(biol) <- runif(prod(dim(flq)), min=0, max=1)
    name(biol) <- as.character(signif(rnorm(1)*1000,3))
    desc(biol) <- as.character(signif(rnorm(1)*1000,3))
    # set the units to something sensible
    units(m(biol)) <- "m"
    units(wt(biol)) <- "kg"
    units(fec(biol)) <- "prop"
    units(mat(biol)) <- "prop"
    units(spwn(biol)) <- "prop"
    units(n(biol)) <- "10^3"
    return(biol)
}

#' Generate randomly sized and filled FLCatch objects
#'
#' Generate an FLCatch of random size and filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing, particularly of the FLCatch class in CPP.
#' 
#' @param fixed_dims A vector of length 6 with the fixed length of each of the FLQuant dimensions. If any value is NA it is randomly set using the max_dims argument. Default value is rep(NA,6).
#' @param max_dims A vector of length 6 with maximum size of each of the FLQuant dimensions. Default value is c(5,5,5,4,4,10).
#' @param sd The standard deviation of the random numbers. Passed to rnorm() Default is 100.
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
#' Generates a list of randomly sized FLCatch objects filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing, particularly of the FLCatches_base<T> class in CPP.
#' 
#' @param min_catches The minimum number of catches. Default is 2. 
#' @param max_catches The maximum number of catches. Default is 5. 
#' @param fixed_dims A vector of length 6 with the fixed length of each of the FLQuant dimensions. If any value is NA it is randomly set using the max_dims argument. 
#' @param max_dims A vector of length 6 with maximum size of each of the FLQuant dimensions. Default value is c(5,5,5,4,4,10).
#' @param sd The standard deviation of the random numbers. Passed to rnorm() Default is 100.
#' @export
#' @return An FLCatches objects
#' @examples
#' flcs <- random_FLCatches_generator()
#' length(flcs)
#' summary(flcs)
#' lapply(flcs, summary)
random_FLCatches_generator <- function(min_catches = 2, max_catches = 5, ...){
    args <- list(...)
    ncatches <- round(runif(1,min=min_catches, max=max_catches))
    op_list <- list()
    flq <- random_FLQuant_generator(...)
    # cat("dim flq: ", dim(flq), "\n")
    fixed_dims <- dim(flq)
    #fixed_dims[1] <- NA
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
#' @param fixed_dims A vector of length 6 with the fixed length of each of the FLQuant dimensions. If any value is NA it is randomly set using the max_dims argument. Default value is rep(NA,6).
#' @param max_dims A vector of length 6 with maximum size of each of the FLQuant dimensions. Default value is c(5,5,5,4,4,10).
#' @param sd The standard deviation of the random numbers. Passed to rnorm() Default is 100.
#' @export
#' @return An FLFishery object 
#' @examples
#' flf <- FLasher:::random_FLFishery_list_generator()
#' summary(flf)
#' flf <- random_FLFishery_generator(fixed_dims = c(NA,10,1,1,1,1))
#' lapply(flf, summary)
#' flf <- random_FLFishery_generator(fixed_dims = c(NA,10,1,1,1,1), max_dims = c(100,NA,NA,NA,NA,NA))
random_FLFishery_generator <- function(min_catches = 2, max_catches = 5, sd = 100,  ...){
    catches <- random_FLCatches_generator(min_catches, max_catches, ...)
    fishery <- FLFishery(catches)
    # fill up effort, vcost and fcost
    effort(fishery)[] <- rnorm(prod(dim(effort(fishery))),sd=sd)
    vcost(fishery)[] <- rnorm(prod(dim(vcost(fishery))),sd=sd)
    fcost(fishery)[] <- rnorm(prod(dim(fcost(fishery))),sd=sd)

    fishery@hperiod[] <- rnorm(prod(dim(fishery@hperiod)),sd=sd)
    # hperiod 1 must be <= 2

    fishery@desc <- as.character(signif(rnorm(1)*1000,3))
    fishery@name <- as.character(signif(rnorm(1)*1000,3))


    # Fix and set fperiod for each fishery

    return(fishery)
}


#' Generate a randomly filled and sized FLFisheries object
#'
#' Generate a randomly sized FLFisheries object filled with normally distributed random numbers with a mean of 0.
#' Used for automatic testing, particularly of the FLFisheries_base<T> class in CPP.
#' 
#' @param min_fisheries The minimum number of FLFisheries in the fisheries list. Default is 2. 
#' @param max_fisheries The maximum number of FLFisheries in the fisheries list. Default is 5. 
#' @param min_catches The minimum number of catches. Default is 2. 
#' @param max_catches The maximum number of FLCatches in the catches list. Default is 5. 
#' @param fixed_dims A vector of length 6 with the fixed length of each of the FLQuant dimensions. If any value is NA it is randomly set using the max_dims argument. Default value is rep(NA,6).
#' @param max_dims A vector of length 6 with maximum size of each of the FLQuant dimensions. Default value is c(5,5,5,4,4,10).
#' @param sd The standard deviation of the random numbers. Passed to rnorm() Default is 100.
#' @export
#' @return An FLFishery object 
#' @examples
#' flf <- FLasher:::random_FLFishery_list_generator()
#' summary(flf)
#' flf <- random_FLFishery_generator(fixed_dims = c(NA,10,1,1,1,1))
#' lapply(flf, summary)
#' flf <- random_FLFishery_generator(fixed_dims = c(NA,10,1,1,1,1), max_dims = c(100,NA,NA,NA,NA,NA))
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
#' FLBiol, SRR model name, SRR params, SRR timelag, SRR residuals and SRR residuals mult.
#' This function generates randomly filled FLBiol objects, of the same size.
#' Used for automatic testing, particularly of the fwdBiols<T> class in CPP.
#' 
#' @param min_biols The minimum number of fwdBiols in the list. Default is 1. 
#' @param max_biols The maximum number of fwdBiols in the list. Default is 5. 
#' @param fixed_dims A vector of length 6 with the fixed length of each of the FLQuant dimensions. If any value is NA it is randomly set using the max_dims argument. Default value is rep(NA,6).
#' @param max_dims A vector of length 6 with maximum size of each of the FLQuant dimensions. Default value is c(5,5,5,4,4,10).
#' @param sd The standard deviation of the random numbers. Passed to rnorm() Default is 100.
#' @export
#' @return A list object 
#' @examples
#' fwdBiols <- random_fwdBiols_list_generator()
random_fwdBiols_list_generator <- function(min_biols = 1, max_biols = 5, ...){
    nbiols <- round(runif(1,min=min_biols,max=max_biols))
    biols <- list()
    # All biols must have same dims
    seed_biol <- random_FLBiol_generator(...)
    for (i in 1:nbiols){
        biol_bits <- list()
        biol_bits[["biol"]] <- random_FLBiol_generator(fixed_dims=dim(n(seed_biol)))
        biol_bits[["srr_model_name"]] <- "bevholt"
        biol_bits[["srr_params"]] <- FLQuant(abs(rnorm(2)), dimnames=list(params=c("a","b")))
        biol_bits[["srr_residuals"]] <- n(biol_bits[["biol"]])[1,]
        biol_bits[["srr_residuals_mult"]] <- TRUE
        biols[[as.character(signif(abs(runif(1,min=100,max=999)),3))]] <- biol_bits
    }
    return(biols)
}

#' Simple projection with minimal checks
#'
#' Given FLFisheries, FLBiol, FLSR, F and f.spwn, project over timesteps
#' No dimension checks are made!
#' Recruitment is annual only. Happens at start of the year. SSB is calculated in previous year (or years depending on recruitment age).
#' 
#' @param flfs FLFisheries (with a single FLFishery with a single FLCatch)
#' @param flb FLBiol
#' @param f List of fishing mortality FLQuant objects (only a list of length 1 to start with)
#' @param f_spwn List of fishing timing FLQuant objects (only a list of length 1 to start with) - not used at the moment - part of the SSB calculation
#' @param sr_residuals FLQuant of residuals for the recruitment
#' @param sr_residuals_mult Are the residuals multiplicative (TRUE)  or additive (FALSE)?
#' @param timesteps Continuous sequence of integers (years and seasons)
#' @export
#' @return A list of FLFisheries and FLBiol objects
simple_fisheries_project <- function(flfs, flb, flsr, f, f_spwn, sr_residuals, sr_residuals_mult, timesteps){
    nseason <- dim(n(flb))[4]
    nages <- 1:dim(n(flb))[1]
    last_age <- nages[length(nages)]
    #timesteps <- ((years[1] - 1) * nseason + 1):(years[length(years)] * nseason)
    nfishery <- 1
    ncatch <- 1
    #recruitment_timelag <- 
    z <- f[[nfishery]] + m(flb)
    rec_age <- as.numeric(dimnames(rec(flsr))$age) # recruitment age in years
    for (timestep in timesteps){
        year <- floor((timestep - 1) / nseason + 1)
        season <- (timestep - 1) %% nseason + 1
        next_year <- floor(((timestep+1) - 1) / nseason + 1)
        next_season <- ((timestep+1) - 1) %% nseason + 1
        # Update fishery
        catch <- (f[[nfishery]] / z) * (1 - exp(-z)) * n(flb)
        # Must calculate ln and dn before loading, OR, make a copy of discards.ratio
        # else discards.ratio is affected by ln and dn
        ln <- catch * (1 - discards.ratio(flfs[[nfishery]][[ncatch]]))
        dn <- catch * (discards.ratio(flfs[[nfishery]][[ncatch]]))
        landings.n(flfs[[nfishery]]@.Data[[ncatch]]) <- ln
        discards.n(flfs[[nfishery]]@.Data[[ncatch]]) <- dn 
        # Update Biol
        # Recruitment
        rec <- 0
        if (season == 1){ # Recruitment happens
            ssb_all <- quantSums(n(flb) * exp(-(f[[nfishery]] * f_spwn[[nfishery]] + m(flb) * spwn(flb))) * wt(flb) * fec(flb))
            ssb_for_rec <- ssb_all[1,year-rec_age+1,1,season,1,] # SSB for rec in next year
            rec <- predict(flsr,ssb=FLQuant(ssb_for_rec))
            if (sr_residuals_mult==TRUE){
                rec <- rec * sr_residuals[1,next_year,1,next_season,1,]
            }
            else{
                rec <- rec + sr_residuals[1,next_year,1,next_season,1,]
            }
        }
        #ssb(flb)[
        n(flb)[1,next_year,1,next_season,1,] <- rec

        # Abundances
        n(flb)[nages[-1],next_year,1,next_season,1,] <- n(flb)[-last_age,year,1,season,1,] * exp(-z[-last_age,year,1,season,1,])
        # plusgroup
        n(flb)[last_age,next_year,1,next_season,1,] <- n(flb)[last_age,next_year,1,next_season,1,] + n(flb)[last_age,year,1,season,1,] * exp(-z[last_age,year,1,season,1,])
    }
    return(list(flfs = flfs, flb = flb))
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
    quantities <- c("f","catch","landings","discards", "ssb", "biomass")
    abundance_quantities <- c("ssb", "biomass", "f")
    f_quantities <- c("f") # either B, or FCB
    # Randomly set simultaneous targets
    nsim_each <- round(runif(length(years) * nseasons, min = 1, max = max_nsim_target))
    years_col <- rep(years, each = nseasons)
    years_col <- rep(years_col, nsim_each)
    seasons_col <- rep(1:nseasons, length(years))
    seasons_col <- rep(seasons_col, nsim_each)
    target <- data.frame(year=years_col,
                          value=rlnorm(years_col),
                          min = NA,
                          max = NA,
                          quantity=NA,
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
    target$quantity <- quantities[round(runif(nrow(target),min=1,max=length(quantities)))]
    # But force last one to be an abundance target to help with testing
    target$quantity[length(target$quantity)] <- "biomass"
    # And for catch too
    target$quantity[length(target$quantity)-1] <- "catch"

    # Randomly set FCB (based on target)
    # Fishery targets
    fish_targets <- which(!(target$quantity %in% abundance_quantities))
    # Half of these are FC, the others B
    FC_targets <- sample(fish_targets, ceiling(length(fish_targets) / 2))
    B_targets <- fish_targets[!(fish_targets %in% FC_targets)]
    target[FC_targets,c("fishery","catch")] <- round(runif(length(FC_targets)*2, min = 1, max = 4))
    target[B_targets,"biol"] <- round(runif(length(B_targets), min = 1, max = 4))
    # Abundance targets are biol only
    biol_targets <- which(target$quantity %in% abundance_quantities)
    target[biol_targets, "biol"] <- round(runif(length(biol_targets), min = 1, max = 4))
    # Fix f targets, either B, or FCB
    f_targets <- which(target$quantity %in% f_quantities)
    f_FCB_targets <- sample(f_targets, ceiling(length(f_targets) / 2))
    if (length(f_FCB_targets > 0)){
        target[f_FCB_targets,c("fishery","catch")] <- round(runif(length(f_FCB_targets)*2, min = 1, max = 4))
    }

    # Force integers - should be done in fwd() dispatch or constructor
    target$fishery <- as.integer(target$fishery)
    target$catch <- as.integer(target$catch)
    target$biol <- as.integer(target$biol)
    target$year <- as.integer(target$year)
    target$season <- as.integer(target$season)
    target$quantity <- as.character(target$quantity)

    # Some targets are min and max 
    min_max_row <- sample(1:nrow(target), ceiling(nrow(target) / 2))
    min_row <- min_max_row[1:(length(min_max_row)/2)]
    max_row <- min_max_row[!(min_max_row %in% min_row)]
    target[min_row, "value"] <- NA
    target[min_row, "min"] <- rnorm(length(min_row))
    target[max_row, "value"] <- NA
    target[max_row, "max"] <- rnorm(length(max_row))
    value_row <- which(!( (1:nrow(target)) %in% min_max_row))

    # Make iter values - better creator than this too
    target_iters <- array(NA, dim=c(nrow(target),3,niters), dimnames=list(target_no=1:nrow(target), c("min","value","max"), iter=1:niters))
    target_iters[value_row,"value",] <- runif(niters, min=0.3, max=0.4)
    target_iters[min_row,"min",] <- runif(niters, min=0.3, max=0.4)
    target_iters[max_row,"max",] <- runif(niters, min=0.3, max=0.4)

    # Add target and timestep column - not set by user - should be added before dispatching to C++
    target$timestep <- (target$year-1) * nseasons + target$season
    target <- target[order(target$timestep),]
    tsteps <- unique(target$timestep)
    names(tsteps) <- 1:length(tsteps)
    # Look away!
    for (i in 1:length(tsteps)){
        target[target[,"timestep"] %in% tsteps[i], "target"] <- as.integer(names(tsteps)[i])
    }

    fwc <- fwdControl(target=target, iters=target_iters)

    # Add fake FCB array - will be constructed on R side before calling fwd()
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    attr(fwc@target, "FCB") <- FCB

    return(fwc)
}

#' Create a complex annual test operating model
#'
#' Creates a test operating model for testing FLFishery / FLCatch / FLBiol interactions.
#' Implements all possible type of FCB interactions.
#' Two FLFishery objects with 4 FLCatch objects between them, fishing 4 FLBiol objects.
#' All objects are based on ple4.
#'
#' @export
#' @return A list of objects for sending to C++
make_test_operatingModel1 <- function(niters = 1000){
    # Sort out the FLBiols
    data(ple4)

    # blow up
    ple4_iters <- propagate(ple4, niters)
    seed_biol <- as(ple4_iters,"FLBiol")
    flbiols <- list()
    for (i in 1:5){
        biol <- seed_biol
        n(biol) <- n(biol) * abs(rnorm(prod(dim(n(biol))), mean = 1, sd = 0.1))
        m(biol) <- m(biol) * abs(rnorm(prod(dim(m(biol))), mean = 1, sd = 0.1))
        desc(biol) <- "biol"
        name(biol) <- "biol"
        flbiols[[i]] <- biol
    }

    # Make SRRs
    srr1 <- fmle(as.FLSR(ple4, model="bevholt"),control = list(trace=0))
    srr2 <- fmle(as.FLSR(ple4, model="ricker"),control = list(trace=0))
    res1 <- window(residuals(srr1), start = 1957)
    res1[,"1957"] <- res1[,"1958"]
    res1 <- propagate(res1, niters)
    res1 <- res1 * abs(rnorm(prod(dim(res1)), mean = 1, sd = 0.1))
    res2 <- window(residuals(srr2), start = 1957)
    res2[,"1957"] <- res2[,"1958"]
    res2 <- propagate(res2, niters)
    res2 <- res2 * abs(rnorm(prod(dim(res2)), mean = 1, sd = 0.1))

    # Make the lists of FLBiol bits
    biol_bits1 <- list(biol = flbiols[[1]], srr_model_name = "bevholt", srr_params = as(params(srr1), "FLQuant"), srr_residuals = res1, srr_timelag = 1, srr_residuals_mult = TRUE)
    biol_bits2 <- list(biol = flbiols[[2]], srr_model_name = "ricker", srr_params = as(params(srr2), "FLQuant"), srr_residuals = res2, srr_timelag = 1, srr_residuals_mult = TRUE)
    biol_bits3 <- list(biol = flbiols[[3]], srr_model_name = "bevholt", srr_params = as(params(srr1), "FLQuant"), srr_residuals = res1, srr_timelag = 1, srr_residuals_mult = TRUE)
    biol_bits4 <- list(biol = flbiols[[4]], srr_model_name = "ricker", srr_params = as(params(srr2), "FLQuant"), srr_residuals = res2, srr_timelag = 1, srr_residuals_mult = TRUE)
    biol_bits5 <- list(biol = flbiols[[5]], srr_model_name = "ricker", srr_params = as(params(srr2), "FLQuant"), srr_residuals = res2, srr_timelag = 1, srr_residuals_mult = TRUE)
    biols <- list(biol1 = biol_bits1, biol2 = biol_bits2, biol3 = biol_bits3, biol4 = biol_bits4, biol5 = biol_bits5)

    # Make the Catches
    catch_seed <- as(ple4_iters, "FLCatch")
    catch_list <- list()
    for (i in 1:4){
        catch <- catch_seed
        name(catch) <- paste("catch",i,sep="")
        desc(catch) <- paste("catch",i,sep="")
        landings.n(catch) <- landings.n(catch) * abs(rnorm(prod(dim(landings.n(catch))), mean = 1, sd = 0.1))
        discards.n(catch) <- discards.n(catch) * abs(rnorm(prod(dim(discards.n(catch))), mean = 1, sd = 0.1))
        catch.sel(catch) <- catch.sel(catch) * abs(rnorm(prod(dim(catch.sel(catch))), mean = 1, sd = 0.1))
        sweep(catch.sel(catch), 2:6, apply(catch.sel(catch), 2:6, max), "/")
        catch.q(catch) <- FLPar(c(1,0.5), dimnames=list(params=c("alpha","beta"), iter = 1))
        catch_list[[i]] <- catch
    }

    # Make fishery bits
    fishery1 <- FLFishery(catch1=catch_list[[1]], catch2 = catch_list[[2]])
    desc(fishery1) <- "fishery1"
    effort(fishery1)[] <- 1

    fishery2 <- FLFishery(catch1=catch_list[[3]], catch2 = catch_list[[4]])
    desc(fishery2) <- "fishery2"
    effort(fishery2)[] <- 1
    fisheries <- FLFisheries(fishery1 = fishery1, fishery2 = fishery2)
    fisheries@desc <- "fisheries"

    # Fix and set hperiod for each fishery
    # hperiod 1 < hperiod 2

    # fwdControl
    fwc <- random_fwdControl_generator(niters=niters)
    # Make a temporary FCB attribute - add to class later
    FCB <- array(c(1,1,2,2,2,1,2,1,2,2,1,2,2,3,4), dim=c(5,3))
    colnames(FCB) <- c("F","C","B")
    attr(fwc@target, "FCB") <- FCB

    return(list(fisheries = fisheries, biols = biols, fwc = fwc))
}

#' Create a simple annual test operating model
#'
#' Creates a test operating model for testing FLFishery / FLCatch / FLBiol interactions.
#' Two FLFishery objects with 1 FLCatch objects each fishing on the same, single FLBiol.
#' All objects are based on ple4.
#'
#' @export
#' @return A list of objects for sending to C++
make_test_operatingModel2 <- function(niters = 1000){
    # Sort out the FLBiols
    data(ple4)

    # blow up niters and make FLBiol
    ple4_iters <- propagate(ple4, niters)
    seed_biol <- as(ple4_iters,"FLBiol")
    flbiols <- list()
    biol <- seed_biol
    n(biol) <- n(biol) * abs(rnorm(prod(dim(n(biol))), mean = 1, sd = 0.1))
    m(biol) <- m(biol) * abs(rnorm(prod(dim(m(biol))), mean = 1, sd = 0.1))
    desc(biol) <- "biol"
    name(biol) <- "biol"
    flbiols[[1]] <- biol
    
    # Make SRR
    srr1 <- fmle(as.FLSR(ple4, model="bevholt"),control = list(trace=0))
    res1 <- window(residuals(srr1), start = 1957)
    res1[,"1957"] <- res1[,"1958"]
    res1 <- propagate(res1, niters)
    res1 <- res1 * abs(rnorm(prod(dim(res1)), mean = 1, sd = 0.1))

    # Make the lists of FLBiol bits
    biol_bits1 <- list(biol = flbiols[[1]], srr_model_name = "bevholt", srr_params = as(params(srr1), "FLQuant"), srr_residuals = res1, srr_timelag = 1, srr_residuals_mult = TRUE)
    biols <- list(biol1 = biol_bits1)

    # Make the FLCatches
    catch_seed <- as(ple4_iters, "FLCatch")
    catch_list <- list()
    for (i in 1:2){
        catch <- catch_seed
        name(catch) <- paste("catch",i,sep="")
        desc(catch) <- paste("catch",i,sep="")
        landings.n(catch) <- landings.n(catch) * abs(rnorm(prod(dim(landings.n(catch))), mean = 1, sd = 0.1))
        discards.n(catch) <- discards.n(catch) * abs(rnorm(prod(dim(discards.n(catch))), mean = 1, sd = 0.1))
        catch.sel(catch) <- catch.sel(catch) * abs(rnorm(prod(dim(catch.sel(catch))), mean = 1, sd = 0.1))
        sweep(catch.sel(catch), 2:6, apply(catch.sel(catch), 2:6, max), "/")
        catch.q(catch) <- FLPar(c(1,0.5), dimnames=list(params=c("alpha","beta"), iter = 1))
        catch_list[[i]] <- catch
    }
    # Make fishery bits
    fishery1 <- FLFishery(catch1=catch_list[[1]])
    desc(fishery1) <- "fishery1"
    effort(fishery1)[] <- 1
    fishery2 <- FLFishery(catch1=catch_list[[2]])
    desc(fishery2) <- "fishery2"
    effort(fishery2)[] <- 1
    fisheries <- FLFisheries(fishery1 = fishery1, fishery2 = fishery2)
    fisheries@desc <- "fisheries"
    # hperiod 1 < hperiod 2

    # fwdControl
    fwc <- random_fwdControl_generator()
    # Make a temporary FCB attribute - add to class later
    FCB <- array(c(1,2,1,1,1,1), dim=c(2,3))
    colnames(FCB) <- c("F","C","B")
    attr(fwc@target, "FCB") <- FCB

    return(list(fisheries = fisheries, biols = biols, fwc = fwc))
}

#' Tests if two FLFishery objects are the same
#'
#' Tests each component seperately - allows flexibility
#'
#' @export
test_FLFishery_equal <- function(flf1, flf2){
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

#' Tests if two FLFisheries objects are the same
#'
#' Tests each component seperately - allows flexibility
#'
#' @export
test_FLFisheries_equal <- function(flfs1, flfs2){
    expect_identical(flfs1@desc, flfs2@desc)
    expect_identical(flfs1@names, flfs2@names)
    for (i in 1:length(FLFisheries)){
        test_FLFishery_equal(flfs1[[i]], flfs2[[i]])
    }
}

#' Return 1D element index of FLQuant
#'
#' Given an FLQuant the indices, returns the 1D element accessor.
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

#' Return 1D element index of FLQuant
#'
#' Given an FLQuant the and the indices range, returns the vector of indices
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

#' Tests if two fwdControl objects are the same
#'
#' Tests each component seperately - allows flexibility
#' Avoids problems with FCB being an attribute
#'
#' @export
test_fwdControl_equal <- function(fwc1, fwc2){
    expect_identical(fwc1@target@iters, fwc2@target@iters)
    expect_identical(fwc1@target@element, fwc2@target@element)
}

#' Tests if two fwdQuant objects are the same
#'
#' Tests each component seperately - allows flexibility
#' Just checks dims and values
#'
#' @export
test_FLQuant_equal <- function(flq1, flq2){
    expect_identical(dim(flq1), dim(flq2))
    expect_equal(c(flq1), c(flq2))
}


