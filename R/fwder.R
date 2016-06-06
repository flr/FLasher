# Rough draft for the fwd() method.
# To start with, just a function that is based on an FLStock.

#setGeneric('getPartialFs', function(fishery, biol, FCB, ...)
#    standardGeneric('getPartialFs'))
#
## Return a list (same structure as the FLFishery) of FLQuant of partial Fs
#setMethod('getPartialFs', signature(fishery='FLFishery', biol='list', FCB='matrix'),
#    function(fishery, biol, FCB, ...){
#
#})
#
## Return list of lists (same structure as the FLFisheries) of FLQuant of partial Fs
#setMethod('getPartialFs', signature(fishery='FLFisheries', biol='list', FCB='matrix'),
#    function(fishery, biol, FCB, ...){
#
#})

# F = alpha * Biomass ^ -beta * sel * effort
calc_F <- function(catch, biol, effort){
    biomass <- quantSums(biol@n * biol@wt)
    F <- (catch@catch.q['alpha',] * biomass ^ (-catch@catch.q['beta',]) * effort) %*% catch@catch.sel
    return(F)
}

FLStock_to_FLBiolcpp_and_FLFishery <- function(fls){
    # Make the biol
    flb <- as(fls, "FLBiol")
    flb <- as(flb,"FLBiolcpp")
    # Check biol
    #expect_identical(flb@n, fls@stock.n)
    #expect_identical(flb@m, fls@m)
    #expect_identical(flb@wt, fls@stock.wt)
    #expect_identical(flb@mat, fls@mat)
    #expect_identical(flb@name, fls@name)
    #expect_identical(flb@desc, fls@desc)
    #expect_identical(flb@spwn, fls@m.spwn[1,]) # m happens continuously through year so m.spwn = spwn
    # flb@fec and flb@range not used yet so ignore

    # Make the catch
    flc <- as(fls, "FLCatch")
    # Check catch
    #expect_identical(flc@landings.n, fls@landings.n)
    #expect_identical(flc@landings.wt, fls@landings.wt)
    #expect_identical(flc@discards.n, fls@discards.n)
    #expect_identical(flc@discards.wt, fls@discards.wt)
    #expect_identical(flc@catch.sel, fls@harvest %/% apply(fls@harvest, 2:6, max)) # Selectivity is harvest scaled to 1 each year
    # Fix name and desc
    flc@name <- fls@name
    flc@desc <- fls@desc
    # flc@price and flc@range not used

    # Sort out catch.q
    niter <- dim(harvest(fls))[6]
    flc@catch.q <- FLPar(0.0, dimnames=list(params=c("alpha","beta"), iter = 1:niter))
    # F = alpha * B ^-beta * sel * effort
    # Assume:
    # effort in first year is 1 and other years are relative to that
    # beta is 0 (F not dependent on Biomass)
    # catchability params are fixed in time
    alpha <- c(harvest(fls)[1,1] / catch.sel(flc)[1,1])
    flc@catch.q['alpha',] <- alpha

    # Make the fishery and set some slots
    flf <- FLFishery(catch = flc)
    flf@name <- "Bang"
    flf@desc <- "FLFishery"
    flf@effort <- (fls@harvest / (flc@catch.q['alpha',] * flc@catch.sel))[1,] # effort is 1 in first year, all others relative to it
    names(dimnames(flf@effort))[1] <- "quant"
    dimnames(flf@effort)$quant <- "all"
    # Timing of fishing. When does fishing start?
    # How much fishing happens between start of fishing and spawning - f.spwn slot
    # When does spawning happen - spwn slot
    # We don't know the duration of fishing - it doesn't matter so long as the proportion before and after spawning are correct
    # set so that it is the same length of time between spwn and end - this way fend does not > 1
    fduration <- 1 - flb@spwn[1,]
    flf@hperiod[1,] <- flb@spwn - (fls@harvest.spwn[1,] * fduration) # proportion of duration before spwn
    flf@hperiod[2,] <- flb@spwn + ((1 - fls@harvest.spwn[1,]) * fduration) # proportion of duration after spwn

    return(list(flf = flf, flb = flb))
}

fwder <- function(fls,  ctrl, sr, sr.residuals = FLQuant(1, dimnames=dimnames(fls@stock)), sr.residuals.mult = TRUE){
    # Turn fls into an FLBiolCpp and FLFishery
    bits <- FLStock_to_FLBiolcpp_and_FLFishery(fls)

    # Sort out SRR bits
    # sr is a list with:
    # model = model name
    # params = FLPar of params
    srr_params <- as(sr$params,"FLQuant")
    # srr residuals must have the same size as the fls (iters 1 or n)
    dms <- dimnames(fls@stock)
    sr.residuals <- window(sr.residuals, start=min(as.numeric(dms$year)), end=max(as.numeric(dms$year)))

    # Put biol bits together
    biol_bits <- list(biol = bits$flb,
                      srr_model_name = sr$model,
                      srr_params = srr_params,
                      srr_residuals = sr.residuals,
                      srr_residuals_mult = sr.residuals.mult)
    # Need a list of all biols bits. Here we just have one.
    biols <- list(biol1 = biol_bits)

    # Make the FLFisheries
    flfs <- FLFisheries(fishery = bits$flf)
    flfs@desc <- "Season of the Witch"

    # Sort out control
    # Overwrite fishery, catch and biol columns to reflect that we have only one biol being fished by one catch
    # This saves having to set it in the control object before sending to fwder
    ctrl@target$fishery <- as.integer(NA)
    ctrl@target$catch <- as.integer(NA)
    ctrl@target$biol <- 1L
    # Correct years to indices
    year_index <- ctrl@target$year - min(as.numeric(dimnames(fls@stock)$year)) + 1
    ctrl@target$year <- year_index
    # Fix seasons if necessary
    nseason <- dim(stock(fls))[4]
    if (nseason == 1){
        ctrl@target$season <- 1L
    }
    # Fix indices of relative years and seasons
    rel_year_index <- ctrl@target$relYear - min(as.numeric(dimnames(fls@stock)$year)) + 1
    ctrl@target$relYear <- rel_year_index
    rel_year_rows <- !is.na(ctrl@target$relYear)
    if (nseason == 1){
        ctrl@target$relSeason[rel_year_rows] <- 1L
    }
    # Force relFishery and relCatch to be NA and relBiol to be 1 (reflects that we have only one biol being fished by one catch)
    ctrl@target$relFishery[rel_year_rows] <- as.integer(NA)
    ctrl@target$relCatch[rel_year_rows] <- as.integer(NA)
    ctrl@target$relBiol[rel_year_rows] <- 1L
    # Add minAge and maxAge from fbar range
    frows <- ctrl@target$quant == "f"
    ctrl@target[frows,"minAge"] <- fls@range["minfbar"]
    ctrl@target[frows,"maxAge"] <- fls@range["maxfbar"]
    # add FCB
    FCB <- array(c(1,1,1), dim=c(1,3))
    colnames(FCB) <- c("F","C","B")
    attr(ctrl, "FCB") <- FCB
    
    ctrl@target$order <- seq(1, nrow(ctrl@target))

    # Call FLasher run()
    out <- test_operatingModel_run2(flfs, biols, ctrl, effort_mult_initial = 1.0, indep_min = 0.0, indep_max = 1e12, nr_iters = 50)

    # Scrape output back into FLStock
    flc_out <- out$om$fisheries[[1]][[1]]
    fls@catch <- catch(flc_out)
    fls@catch.n <- catch.n(flc_out)
    fls@catch.wt <- catch.wt(flc_out) # Needs to be updated too for internal consistency - calculated by FLasher and in FLCatch
    fls@landings <- landings(flc_out)
    fls@landings.n <- landings.n(flc_out)
    fls@discards <- discards(flc_out)
    fls@discards.n <- discards.n(flc_out)
    # wts, m, mat, *.spwn are the same
    # Need to calc F
    fls@harvest <- calc_F(flc_out, out$om$biols[[1]], out$om$fisheries[[1]]@effort)
    units(fls@harvest) <- "f"
    # from biol
    fls@stock.n <- out$om$biols[[1]]@n
    fls@stock <- quantSums(fls@stock.n * fls@stock.wt)

    return(fls)
}


