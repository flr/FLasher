# fwd.R - DESC
# fwd.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# fwd(FLBiols, FLFisheries, fwdControl) {{{

setMethod("fwd", signature(biols="FLBiols", fisheries="FLFisheries",
  control="fwdControl"),
  
  function(biols, fisheries, control,
    residuals=lapply(lapply(biols, spwn), "[<-", value=1)) {

  # CHECK length and names of biols and residuals
  if(!all.equal(names(biols), names(residuals)))
    stop("Names of biols and residuals must match exactly")

  # CHECK years and dimensions match
  dib <- do.call(rbind, lapply(biols, function(x) as.data.frame(dims(x))))
  dif <- do.call(rbind, lapply(fisheries, function(x) as.data.frame(dims(x))))
  dnb <- dimnames(n(biols[[1]]))
  
  # ERROR if seasons are different in FLBiols or FLFisheries
  if(!all(c(dib$season, dif$season) == dib$season[1]))
    stop("All FLBiol and FLFishery objects must have the same number of seasons")

  # ERROR if multiple areas
  if(max(dib$area) > 1 | max(dif$area > 1))
    stop("fwd() cannot deal (yet) with multiple areas")

  # CONVERT biols to list(list(biols, name, params, residuals, mult))
  biolscpp <- lapply(biols, as, "list")

  # ADD residuals
  for(i in names(biolscpp)) {
    biolscpp[[i]][["srr_residuals"]] <- residuals[[i]]
  }

  # PARSE control
  trg <- target(control)

  # CONVERT to numeric 'season', 'area', 'unit'
  if (!is.numeric(trg$season))
    trg[,"season"] <- as.integer(match(trg[,"season"], dnb[["season"]]))
  if (!is.numeric(trg$unit))
    trg[,"unit"] <- as.integer(match(trg[,"unit"], dnb[["unit"]]))
  if (!is.numeric(trg$area))
    trg[,"area"] <- as.integer(match(trg[,"area"], dnb[["area"]]))

  # CONVERT to numeric 'fishery', ...
  if (!is.numeric(trg$fishery))
    trg[,"fishery"] <- match(trg[,"fishery"], rownames(dif))
  if(nrow(dif) == 1 & all(is.na(trg["fishery"])))
    trg[,"fishery"] <- 1L

  # ... 'catch', ...
  cns <- lapply(fisheries, function(x) names(x))
  for(i in names(cns))
    trg[,"catch"] <- as.integer(match(trg[,"catch"], cns[[i]]))
  
  # ... and 'biol'
  if (!is.numeric(trg$biol))
    trg[,"biol"] <- match(trg[,"biol"], rownames(dib))
  if(nrow(dib) == 1 & all(is.na(trg["biol"])))
    trg[,"biol"] <- 1L

  # CONVERT 'years' and 'relYear' to position indices
  mny <- min(dib[,"minyear"]) + 1
  trg <- transform(trg, year=year - mny)
  trg <- transform(trg, relYear=relYear - mny)
  
  # ADD order column  
  trg$order <- seq(1, nrow(trg))

  # REPLACE target
  target(control) <- trg

  # TODO CHECK rel*

  # TODO ADD minAge and maxAge from fbar range by FLB/FLC

  
  # CREATE FCB, if missing and possible
  if(dim(control@FCB)[1] == 1 & all(is.na(control@FCB)))
    control@FCB <- fcb2int(fcb(biols, fisheries), biols, fisheries)
  
  # TODO CHECK FCB combinations by quant
  # TODO CHECK dimensions of FCB combinations

  # CALL oMRun
  out <- operatingModelRun(fisheries, biolscpp, control,
    effort_mult_initial = 1.0, indep_min = 0.0, indep_max = 1e12, nr_iters = 50)

  # UPDATE biols w/ new biolscpp@n
  for(i in names(biols))
    n(biols[[i]]) <- out$om$biols[[i]]@n

  # RETURN list(biols, fisheries, control)
  out <- list(biols=biols, fisheries=out$om$fisheries, control=control)

  return(out)
  }

) # }}}

# fwd(FLBiols, FLFishery, fwdControl)
# fwd(FLBiol, FLFisheries, fwdControl)
# fwd(FLBiol, FLFishery, fwdControl)
# fwd(FLBiol, FLFishery, missing)

# fwd(FLStock, missing, fwdControl) {{{

setMethod("fwd", signature(biols="FLStock", fisheries="missing",
  control="fwdControl"),
  
  function(biols, control, sr=predictModel(model=rec~a, params=FLPar(a=1)),
    residuals=rec(biols)/rec(biols)) {

    # biols
    B <- as(biols, "FLBiol")
    rec(B) <- predictModel(model=model(sr), params=params(sr))
    Bs <- FLBiols(B=B)

    # fisheries
    F <- as(biols, 'FLFishery')
    name(F) <- "F"
    names(F) <- "B"

    Fs <- FLFisheries(F=F)
    Fs@desc <- "F"

    # RUN
    out <- fwd(Bs, Fs, control, residuals=FLQuants(B=residuals))

    # PARSE output
    Fc <- out$fisheries[[1]][[1]]
    eff <- out$fisheries[[1]]@effort
    Bo <- out$biols[[1]]

    # catch.n
    biols@catch <- catch(Fc)
    # catch.n
    biols@catch.n <- catch.n(Fc)
    # catch.wt
    biols@catch.wt <- catch.wt(Fc)
    # landings.n
    biols@landings.n <- Fc@landings.n
    # landings.wt
    biols@landings.wt <- Fc@landings.wt
    # discards.n
    biols@discards.n <- Fc@discards.n
    # discards.wt
    biols@discards.wt <- Fc@discards.wt
    # harvest (F)
    biols@harvest <- FLasher:::calc_F(Fc, Bo, eff)
    units(biols@harvest) <- "f"
    # stock.n
    biols@stock.n <- Bo@n
    # stock
    biols@stock <- quantSums(biols@stock.n * biols@stock.wt)

    return(biols)
  }
) # }}}

# fwd(FLStock, missing, missing, ...) {{{

setMethod("fwd", signature(biols="FLStock", fisheries="missing",
  control="missing"),
  
  function(biols, ..., sr=predictModel(model=rec~a, params=FLPar(a=1)),
    residuals=rec(biols)/rec(biols)) {
    
    # PARSE ...
    args <- list(...)
    
    # Does ... exist?
    if(length(args) < 1)
      stop("No fwdControl provided and no FLQuant targets given, cannot do anything!")

    # NAMES in qlevels?
    if(!names(args) %in% FLasher:::qlevels)
      stop(paste0("Names of input FLQuant(s) do not match current allowed targets: ",
            paste(FLasher:::qlevels, collapse=", ")))

    args <- FLQuants(args)

    control <- as(args, "fwdControl")

    out <- fwd(biols, control=control, residuals=residuals, sr=sr)

    return(out)
  }
) # }}}

# F = alpha * Biomass ^ -beta * sel * effort
calc_F <- function(catch, biol, effort){
    biomass <- quantSums(biol@n * biol@wt)
    F <- (catch@catch.q['alpha',] * biomass ^ (-catch@catch.q['beta',]) * effort) %*% catch@catch.sel
    return(F)
}
