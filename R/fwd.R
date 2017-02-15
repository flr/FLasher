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

  # CREATE FCB, if missing and possible
  if(dim(control@FCB)[1] == 1 & all(is.na(control@FCB)))
    control@FCB <- fcb2int(fcb(biols, fisheries), biols, fisheries)

  # PARSE control
  trg <- target(control)

  # CONVERT to numeric 'season', 'area', 'unit'
  if (!is.numeric(trg$season))
    trg[,"season"] <- as.integer(match(trg[,"season"], dnb[["season"]]))
  if (!is.numeric(trg$unit))
    trg[,"unit"] <- as.integer(match(trg[,"unit"], dnb[["unit"]]))
  if (!is.numeric(trg$area))
    trg[,"area"] <- as.integer(match(trg[,"area"], dnb[["area"]]))
  
  # CHECK fcb combinations by quant
  tfcb <- trg[,c("quant", "fishery", "catch", "biol")]
  tfcb[,-1] <- !is.na(tfcb[,-1])
  idx <- merge(tfcb, .vfcb)
  if(nrow(idx) != nrow(tfcb))
    stop("Misspecified target(s)")

  # CONVERT to numeric 'fishery', ...
  if (!is.numeric(trg$fishery))
    trg[,"fishery"] <- match(trg[,"fishery"], rownames(dif))
  if(nrow(dif) == 1 & all(is.na(trg["fishery"])))
    trg[,"fishery"] <- 1L

  # ... 'catch', ...
  if (!is.numeric(trg$catch)) {
    cns <- lapply(fisheries, function(x) names(x))
    for(i in names(cns))
      trg[,"catch"] <- as.integer(match(trg[,"catch"], cns[[i]]))
  }
  
  # ... and 'biol'
  if (!is.numeric(trg$biol))
    trg[,"biol"] <- match(trg[,"biol"], rownames(dib))
  # If we have 2 fisheries on 1 catch, catch target is set through catch / fishery only, not biol AND catch / fishery
  #if(nrow(dib) == 1 & all(is.na(trg["biol"])))
  #  trg[,"biol"] <- 1L

  # CONVERT 'years' and 'relYear' to position indices
  mny <- min(dib[,"minyear"]) - 1
  trg <- transform(trg, year=year - mny)
  trg <- transform(trg, relYear=relYear - mny)
 
  # TODO CHECK rel*
  # Allow all NA
  # IF annual model & relY, then relS == 1 A XNOR B
  # relY AND relS? Then check relF, relC, relB as FCB


browser()

    # Do we have any relYears AND it is annual
    # check relSeason is NA or 1
    # If not - error
    # If NA - 1
    annual_model <- dim(n(biols[[1]]))[4] == 1
    if (any(!is.na(trg$relYear)) & annual_model) {
        relYear_rows <- which(!is.na(trg$relYear))
        # If relSeason is not NA or 1 throw an error
        if (!all(trg$relSeason[relYear_rows] %in% c(NA, 1))){
            stop("With an annual model, if you have a relative target, relSeason must be set to 1 or NA")
        }
        # If relSeason is NA, set to 1
        trg$relSeason[relYear_rows[is.na(trg$relSeason[relYear_rows])]] <- 1
    }

    # If relYear must have relSeason AND if relSeason must have relYear
    if (any(!(!is.na(trg$relYear) & !is.na(trg$relSeason)))){
        stop("If you have a reYear you must also have a relSeason, and vice versa")
    }

    # If relYear must also have relFishery / relCatch / relBiol
    # Might get tricky for a specific check - add general check for any of them here
    # Can be:
    #   just relFishery
    #   just relBiol
    #   relCatch AND relFishery
    # add FLStock specific check to fwd(FLStock, ...)

    if (any(!is.na(trg$relYear))){

    }


  # REPLACE target
  target(control) <- trg

  # If order is not specified, attempt to sort it out
  if(!(any(colnames(control@target) %in% "order"))){
      control <- add_target_order(control)
  }

  # FIX empty character slots
  if(length(fisheries@desc) == 0)
    fisheries@desc <- character(1)
  if(length(biols@desc) == 0)
    biols@desc <- character(1)


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

# fwd(FLBiols, FLFishery, fwdControl) {{{

setMethod("fwd", signature(biols="FLBiols", fisheries="FLFishery",
  control="fwdControl"),
  
  function(biols, fisheries, control, ...) {
    res <- fwd(biols=biols, fisheries=FLFisheries(F=fisheries),
      control=control, ...)
    res$fisheries <- res$fisheries[[1]]
    return(res)
  }
) # }}}

# fwd(FLBiol, FLFisheries, fwdControl) {{{

setMethod("fwd", signature(biols="FLBiol", fisheries="FLFisheries",
  control="fwdControl"),
  
  function(biols, fisheries, control, ...) {
 
    # IF   
    len <- unlist(lapply(fisheries, length))
    if(any(len > 1))
      stop("")
    nms <- names(fisheries[[1]])[1]
    
    biols <- FLBiols(B=biols)
    names(biols) <- nms
    
    res <- fwd(biols=biols, fisheries=fisheries,
      control=control, ...)
    res$biols <- res$biols[[1]]
    return(res)
  }
) # }}}

# fwd(FLBiol, FLFishery, fwdControl) {{{

setMethod("fwd", signature(biols="FLBiol", fisheries="FLFishery",
  control="fwdControl"),
  
  function(biols, fisheries, control,
    residuals=FLQuant(1, dimnames=dimnames(rec(biols)))) {

    # COERCE to FLBiols and FLFisheries
    Bs <- FLBiols(B=biols)
    Fs <- FLFisheries(F=fisheries)
    Fs@desc <- "F"

    # SET @FCB
    control@FCB <- matrix(1, ncol=3, nrow=1, dimnames=list(1, c("F", "C", "B")))

    # SET @target[fcb]
    control@target[c("fishery", "catch", "biol")] <- rep(c(NA, NA, 1), each=dim(control@target)[1])

    # RUN
    out <- fwd(Bs, Fs, control, residuals=FLQuants(B=residuals))

    # PARSE output
    Fc <- out$fisheries[[1]][[1]]
    Bo <- out$biols[[1]]

    return(list(biols=Bo, fisheries=Fc))
  }
) # }}}

# fwd(FLBiol, FLFishery, missing) {{{

setMethod("fwd", signature(biols="FLBiol", fisheries="FLFishery",
  control="missing"),
  
  function(biols, fisheries, ..., residuals=FLQuant(1, dimnames=dimnames(m(biols)))) {
    
    # PARSE ...
    args <- list(...)
    
    # Does ... exist?
    if(length(args) < 1)
      stop("No fwdControl provided and no FLQuant targets given, cannot do anything!")

    # NAMES in qlevels?
    if(!names(args) %in% .qlevels)
      stop(paste0("Names of input FLQuant(s) do not match current allowed targets: ",
            paste(.qlevels, collapse=", ")))

    args <- FLQuants(args)

    control <- as(args, "fwdControl")

    out <- fwd(biols, fisheries, control=control, residuals=residuals)

    return(out)
  }
) # }}}

# fwd(FLStock, missing, fwdControl) {{{

setMethod("fwd", signature(biols="FLStock", fisheries="missing",
  control="fwdControl"),
  
  function(biols, control, sr=predictModel(model=rec~a, params=FLPar(a=1)),
    residuals=FLQuant(1, dimnames=dimnames(rec(biols)))) {

    # DEAL with iters
    its <- dims(biols)$iter
    if(its > 1)
      biols <- propagate(biols, its)

    # COERCE to FLBiols
    B <- as(biols, "FLBiol")
    if(is(sr, "predictModel") | is(sr, "FLSR"))
      rec(B) <- predictModel(model=model(sr), params=params(sr))
    else if(is(sr, "list")) {
      B@rec@model <- do.call(sr$model, list())[["model"]]
      B@rec@params <- sr$params
    }
    Bs <- FLBiols(B=B)

    # COERCE to FLFisheries
    F <- as(biols, 'FLFishery')
    name(F) <- "F"
    names(F) <- "B"

    Fs <- FLFisheries(F=F)
    Fs@desc <- "F"

    # SET @FCB
    control@FCB <- matrix(1, ncol=3, nrow=1, dimnames=list(1, c("F", "C", "B")))

    # SET @target[fcb]
    control@target[c("fishery", "catch", "biol")] <- rep(c(NA, NA, 1), each=dim(control@target)[1])

    # IF minAge and maxAge are NA, then range(min, max)
    arng <- control@target[,c("minAge", "maxAge")]
    arng[,1] <- ifelse(is.na(arng[,1]), range(biols, "minfbar"), arng[,1])
    arng[,2] <- ifelse(is.na(arng[,2]), range(biols, "maxfbar"), arng[,2])
    control@target[,c("minAge", "maxAge")] <- arng
  
    # RUN
    out <- fwd(Bs, Fs, control, residuals=FLQuants(B=residuals))

    # PARSE output
    Fc <- out$fisheries[[1]][[1]]
    eff <- out$fisheries[[1]]@effort
    Bo <- out$biols[[1]]

    # PROJECTION years
    miny <- min(control@target$year)
    maxy <- max(control@target$year)
    pyrs <- as.character(seq(miny, maxy))

    # landings
    biols@landings <- landings(Fc)
    # landings.n
    biols@landings.n <- Fc@landings.n
    # landings.wt
    biols@landings.wt <- Fc@landings.wt
    # discards
    biols@discards <- discards(Fc)
    # discards.n
    biols@discards.n <- Fc@discards.n
    # discards.wt
    biols@discards.wt <- Fc@discards.wt
    # catch
    biols@catch <- catch(Fc)
    # catch.n
    biols@catch.n <- catch.n(Fc)
    # catch.wt
    biols@catch.wt <- catch.wt(Fc)
    # harvest (F)
    biols@harvest[, pyrs] <- calc_F(Fc, Bo, eff)[, pyrs]
    units(biols@harvest) <- "f"
    # stock.n
    biols@stock.n <- Bo@n
    # stock
    biols@stock <- quantSums(biols@stock.n * biols@stock.wt)

    return(biols)
  }
) # }}}

# fwd(FLStock, missing, missing, ...) {{{

setMethod("fwd", signature(biols="FLStock", fisheries="ANY",
  control="missing"),
  
  function(biols, fisheries=missing, ..., sr=predictModel(model=rec~a, params=FLPar(a=1)),
    residuals=FLQuant(1, dimnames=dimnames(rec(biols)))) {
    
    # PARSE ...
    args <- list(...)
    
    # HACK: deal with f assigned to fisheries, might fail
    if(!missing(fisheries)) {

      if(!is(fisheries, "FLQuant"))
        stop("targets can only be of class FLQuant if no fwdControl is provided")
      narg <- names(sys.calls()[[1]])
      narg <- narg[!narg %in% c("", "biols", "sr",
        grep("^[f].*", .qlevels, value=TRUE, invert=TRUE))]
      args[[narg]] <- fisheries
    }
    
    # Does ... exist?
    if(length(args) < 1)
      stop("No fwdControl provided and no FLQuant targets given, cannot do anything!")

    # NAMES in qlevels?
    if(any(!names(args) %in% .qlevels))
      stop(paste0("Names of input FLQuant(s) do not match current allowed targets: ",
            paste(.qlevels, collapse=", ")))

    args <- FLQuants(args)

    # COERCE to fwdControl
    control <- as(args, "fwdControl")
    
    return(fwd(biols, control=control, residuals=residuals, sr=sr))
  }
) # }}}


#' Add the order column to the control target
#'
#' Add the order column to the control target data.frame so that targets are processed in the correct order.
#'
#' It is important that the targets in the control object are processed in the correct order.
#' Targets can happen simultaneously. For example, if there are multiple FLFishery objects in
#' operating model each will need to have a target to solve for at the same time as the others.
#' The targets are processed in a time ordered sequence (year / season).
#' However, within the same year and season it is necessary for the min and max targets to be processed
#' separatley and after the other targets.
#'
#' @param control A fwdControl object
#' @return A fwdControl object with an order column.
#' @export
add_target_order <- function(control){
    # Add temporary original order column - order gets messed about with merge
    control@target$orig_order <- 1:nrow(control@target)
    # Add temporary minmax column
    control@target$minmax <- is.na(control@iters[,"value",1])
    sim_targets <- unique(control@target[,c("year","season","minmax")])
    # Order by year / season / minmax
    sim_targets <- sim_targets[order(sim_targets$year, sim_targets$season, sim_targets$minmax),]
    sim_targets$order <- 1:nrow(sim_targets)
    # Problem - merge reorders by order column
    control@target <- merge(control@target, sim_targets) # order should be the same
    # Reorder by original order so that target and iters slots are consistent
    control@target <- control@target[order(control@target$orig_order),]
    # Reorder target and iters slots by new order
    new_order <- order(control@target$order, control@target$fishery, control@target$catch, control@target$biol)
    control@target <- control@target[new_order,]
    control@iters <- control@iters[new_order,,,drop=FALSE]
    # Remove minmax and orig_order columns
    control@target <- control@target[,colnames(control@target) != "minmax"]
    control@target <- control@target[,colnames(control@target) != "orig_order"]
    return(control)
}
