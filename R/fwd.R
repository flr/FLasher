# fwd.R - DESC
# fwd.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# fwd(FLBiols, FLFisheries, fwdControl, FLQuants) {{{

setMethod("fwd", signature(biols="FLBiols", fisheries="FLFisheries",
  control="fwdControl", residuals="FLQuants"),
  
  function(biols, fisheries, control, residuals) {

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

  target(control) <- trg

  # TODO CHECK rel*

  # TODO ADD minAge and maxAge from fbar range by FLB/FLC

  # CHECK FCB combinations by quant
  
  # TODO CREATE FCB, if missing and possible
  
  # CHECK dimensions of FCB combinations

  # CALL oMRun
  out <- operatingModelRun(fisheries, biolscpp, control,
    effort_mult_initial = 1.0, indep_min = 0.0, indep_max = 1e12, nr_iters = 50)

  # UPDATE biols w/ new biolscpp@n
  for(i in names(biols))
    n(biols[[i]]) <- out$om$biols[[i]]@n

  # RETURN list(biols, fisheries)
  out <- list(biols=biols, fisheries=out$om$fisheries)
  return(out)
  }

) # }}}


#' summary(as(as(ple4, "FLBiol"), 'list'))
setAs("FLBiol", "list",
  function(from) {

    list(
      biol = as(from, "FLBiolcpp"),
      srr_model_name = SRModelName(from@rec@model),
      srr_params = as(from@rec@params, "FLQuant"),
      srr_residuals = FLQuant(),
      srr_residuals_mult = TRUE)
  })
