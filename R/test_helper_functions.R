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
random_FLQuant_generator <- function(fixed_dims = rep(NA,6), max_dims = c(5,10,5,4,4,5), sd = 100){
    nage <- ifelse(is.na(fixed_dims[1]),round(runif(1,min=2, max=max_dims[1])),fixed_dims[1])
    nyear <- ifelse(is.na(fixed_dims[2]),round(runif(1,min=2, max=max_dims[2])),fixed_dims[2])
    nunit <- ifelse(is.na(fixed_dims[3]),round(runif(1,min=2, max=max_dims[3])),fixed_dims[3])
    nseason <- ifelse(is.na(fixed_dims[4]),round(runif(1,min=2, max=max_dims[4])),fixed_dims[4])
    narea <- ifelse(is.na(fixed_dims[5]),round(runif(1,min=2, max=max_dims[5])),fixed_dims[5])
    niter <- ifelse(is.na(fixed_dims[6]),round(runif(1,min=2, max=max_dims[6])),fixed_dims[6])
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
    spwn(biol) <- abs(rnorm(prod(dim(flq)),sd=sd))
    name(biol) <- as.character(signif(rnorm(1)*1000,3))
    desc(biol) <- as.character(signif(rnorm(1)*1000,3))
    # set the units to something sensible
    units(m(biol)) <- "m"
    units(wt(biol)) <- "kg"
    units(fec(biol)) <- "prop"
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
    # catch.q(catch) # undefined right now
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
#' flf <- random_FLFishery_list_generator()
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
#' @param min_catches The minimum number of catches. Default is 2. 
#' @param max_catches The maximum number of FLCatches in the catches list. Default is 5. 
#' @param fixed_dims A vector of length 6 with the fixed length of each of the FLQuant dimensions. If any value is NA it is randomly set using the max_dims argument. Default value is rep(NA,6).
#' @param max_dims A vector of length 6 with maximum size of each of the FLQuant dimensions. Default value is c(5,5,5,4,4,10).
#' @param sd The standard deviation of the random numbers. Passed to rnorm() Default is 100.
#' @export
#' @return An FLFishery object 
#' @examples
#' flf <- random_FLFishery_list_generator()
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


# This should use the fwdControl constructor (when we have one)
#' Dummy fwdControl object creator
#'
#' Creates a dummy fwdControl object for testing purposes
#'
#' @param years numeric vector of years in the control object. Default value is 1:random interger (max = 10).
#' @param niters the number of iterations. Default number is random integer (max = 10).
#' 
#' @export
#' @return A fwdControl object
dummy_fwdControl_generator <- function(years = 1:round(runif(1, min=1,max=10)), niters = round(runif(1,min=1,max=10))){
    # We need to have a proper R version of the class
    # And an automatic generator
    ctrl_df <- data.frame(
                       year = years,
                       season = 1L, # Each target must have a timestep, if season is NA it implies that no seasons in projection so only 1 season in FLQuants so season = 1 
                       # fix this before dispatching to C++
                       quantity = "f", # the target type
                       value =  0.2, # this don't get used in the C++ code
                       min_value = NA,
                       max_value = NA,
                       min_age = NA,
                       max_age = NA,
                       rel_year = NA,
                       rel_season = NA,
                       fishery = NA, # what is this? a number or name? Some way of referring 
                       rel_fishery = NA, # as above
                       catch = NA, # what is this? a number or name? Some way of referring 
                       rel_catch = NA # as above
                       )
    ctrl_df$quantity <- as.character(ctrl_df$quantity)

    target_iters <- array(NA, dim=c(nrow(ctrl_df),3,niters), dimnames=list(target_no=1:nrow(ctrl_df), c("min","value","max"), iter=1:niters))
    #target_iters[,"value",] <- 0.2
    target_iters[,"min",] <- NA#runif(dim(target_iters)[1] * dim(target_iters)[3], min=0.1, max=0.2)
    target_iters[,"value",] <- runif(dim(target_iters)[1] * dim(target_iters)[3], min=0.3, max=0.4)
    target_iters[,"max",] <- NA#runif(dim(target_iters)[1] * dim(target_iters)[3], min=0.5, max=0.6)

    # Force integers
    ctrl_df$fishery <- as.integer(ctrl_df$fishery)
    ctrl_df$rel_fishery <- as.integer(ctrl_df$rel_fishery)
    ctrl_df$year <- as.integer(ctrl_df$year)
    ctrl_df$season <- as.integer(ctrl_df$season)

    fc <- new("fwdControl",
        target = ctrl_df,
        target_iters = target_iters)
    return(fc)
}
