# fwd.R - DESC
# fwd.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# fwd(FLBiols, FLFisheries, fwdControl) {{{

setMethod("fwd", signature(object="FLBiols", fishery="FLFisheries", control="fwdControl"),
    function(object, fishery, control, residuals=lapply(lapply(object, spwn), "[<-", value=1)) {

  # CHECK length and names of biols and residuals
  if(!all.equal(names(object), names(residuals)))
    stop("Names of biols and residuals must match exactly")

  # CHECK years and dimensions match
  dib <- do.call(rbind, lapply(object, function(x) as.data.frame(dims(x))))
  dif <- do.call(rbind, lapply(fishery,
    function(x) data.frame(dims(effort(x))[c("season", "area")])))
  dnb <- dimnames(n(object[[1]]))
  
  # ERROR if seasons are different in FLBiols or FLFisheries
  if(!all(c(dib$season, dif$season) == dib$season[1]))
    stop("All FLBiol and FLFishery objects must have the same number of seasons")

  # ERROR if multiple areas
  if(max(dib$area) > 1 | max(dif$area > 1))
    stop("fwd() cannot deal (yet) with multiple areas")

  # CONVERT biols to list(list(object, name, params, residuals, mult))
  biolscpp <- lapply(object, as, "list")

  # Residuals must be same dim 2-5 as the biol
  # ADD residuals
  for(i in names(biolscpp)) {
    bdnms <- dimnames(n(biolscpp[[i]][["biol"]]))
    year_range <- range(as.numeric(bdnms$year)) 
    # Need a window equivalent for year and season
    residuals[[i]] <- window(residuals[[i]], start=year_range[1], end=year_range[2])
    biolscpp[[i]][["srr_residuals"]] <- residuals[[i]]
  }

  # CREATE FCB, if missing and possible
  if(dim(control@FCB)[1] == 1 & all(is.na(control@FCB)))
    control@FCB <- fcb2int(fcb(object, fishery), object, fishery)

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
  #if(nrow(dif) == 1 & all(is.na(trg["fishery"])))
  #  trg[,"fishery"] <- 1L

  # ... 'catch', ...
  if (!is.numeric(trg$catch)) {
    cns <- lapply(fishery, function(x) names(x))
    for(i in names(cns))
      trg[,"catch"] <- as.integer(match(trg[,"catch"], cns[[i]]))
  }
  
  # ... and 'biol'
  if (is.character(trg$biol))
    trg[,"biol"] <- match(trg[,"biol"], rownames(dib))

  # If we have 2 fisheries on 1 catch, catch target is set through catch / fishery only, not biol AND catch / fishery
  #if(nrow(dib) == 1 & all(is.na(trg["biol"])))
  #  trg[,"biol"] <- 1L

  # CONVERT 'years' and 'relYear' to position indices
  year <- relYear <- NULL # quiet R CMD check about no visible binding for gloabl variable
  mny <- min(dib[,"minyear"]) - 1
  trg <- transform(trg, year=year - mny)
  trg <- transform(trg, relYear=relYear - mny)

  # TODO: CONVERT biol column to integer if character
 
  # TODO CHECK rel*
  # Allow all NA
  # IF annual model & relY, then relS == 1 A XNOR B
  # relY AND relS? Then check relF, relC, relB as FCB

    # Check if we if have any relYears AND it is an annual model
    # If so, check relSeason is NA or 1
    # If not -> error
    # If NA -> 1
    annual_model <- dim(n(object[[1]]))[4] == 1
    if (any(!is.na(trg$relYear)) & annual_model) {
        relYear_rows <- !is.na(trg$relYear)
        # If relSeason is not NA or 1 throw an error
        if (!all(trg$relSeason[relYear_rows] %in% c(NA, 1))){
            stop("With an annual model, if you have a relative target, relSeason must be set to 1 or NA")
        }
        # If relYear is present and relSeason is NA, set to 1
        trg$relSeason[is.na(trg$relSeason) & relYear_rows] <- 1
    }

    # If relYear must have relSeason and vice versa
    if (any(!(is.na(trg$relYear) == is.na(trg$relSeason)))){
        stop("If you have a reYear you must also have a relSeason, and vice versa")
    }


    # Check relative objects
    # If relYear must also have either 
    #   just relFishery
    #   just relBiol
    #   relCatch AND relFishery
    if (any(!is.na(trg$relYear))){
        relYear_rows <- !is.na(trg$relYear)
        relBiol <- !is.na(trg$relBiol)
        relFishery <- !is.na(trg$relFishery)
        # Must have at least a relBiol or a relFishery or a (FLCatch & FLFishery)
        # If relCatch must have relFishery
        relCatch <- !is.na(trg$relCatch) & !is.na(trg$relFishery)
        if (any(!((relBiol | relFishery | relCatch)[relYear_rows]))){
            stop("If relYear set must also set a relBiol, relFishery or FLFishery and relCatch")
        }
    }

  # ENSURE biol is a list
  trg$biol <- as.list(trg$biol)

  # REPLACE target
  target(control) <- trg

  # If order is not specified, attempt to sort it out
  if(!(any(colnames(control@target) %in% "order"))){
      control <- add_target_order(control)
  }

  # FIX empty character slots
  if(length(fishery@desc) == 0)
    fishery@desc <- character(1)
  if(length(object@desc) == 0)
    object@desc <- character(1)
  
  # CALL oMRun
  out <- operatingModelRun(fishery, biolscpp, control,
    effort_mult_initial = 1.0, indep_min = 1e-6, indep_max = 5.0, nr_iters = 50)
  # UPDATE object w/ new biolscpp@n
  for(i in names(object))
    n(object[[i]]) <- out$om$biols[[i]]@n

  # RETURN list(object, fishery, control)
  out <- list(biols=object, fisheries=out$om$fisheries, control=control, flag=out$solver_codes)

  return(out)
  }

) # }}}

# fwd(FLBiols, FLFishery, fwdControl) {{{

setMethod("fwd", signature(object="FLBiols", fishery="FLFishery",
  control="fwdControl"),
  
  function(object, fishery, control, ...) {
    res <- fwd(object=object, fishery=FLFisheries(F=fishery),
      control=control, ...)
    res$fisheries <- res$fisheries[[1]]
    return(res)
  }
) # }}}

# fwd(FLBiol, FLFisheries, fwdControl) {{{

setMethod("fwd", signature(object="FLBiol", fishery="FLFisheries",
  control="fwdControl"),
  
  function(object, fishery, control, ...) {
 
    # IF   
    len <- unlist(lapply(fishery, length))
    if(any(len > 1))
      stop("")
    nms <- names(fishery[[1]])[1]
    
    object <- FLBiols(B=object)
    names(object) <- nms
    
    res <- fwd(object=object, fishery=fishery,
      control=control, ...)
    res$biols <- res$biols[[1]]
    return(res)
  }
) # }}}

# fwd(FLBiol, FLFishery, fwdControl) {{{

setMethod("fwd", signature(object="FLBiol", fishery="FLFishery",
  control="fwdControl"),
  
  function(object, fishery, control,
    residuals=FLQuant(1, dimnames=dimnames(rec(object)))) {

    # COERCE to FLBiols and FLFisheries
    Bs <- FLBiols(B=object)
    Fs <- FLFisheries(F=fishery)
    Fs@desc <- "F"

    # SET @FCB
    control@FCB <- matrix(1, ncol=3, nrow=1, dimnames=list(1, c("F", "C", "B")))

    # SET @target[fcb]
    control@target[c("fishery", "catch", "biol")] <- rep(c(NA, NA, 1),
      each=dim(control@target)[1])

    # RUN
    out <- fwd(Bs, Fs, control, residuals=FLQuants(B=residuals))

    # PARSE output
    Fc <- out$fisheries[[1]][[1]]
    Bo <- out$biols[[1]]

    return(list(biols=Bo, fisheries=Fc))
  }
) # }}}

# fwd(FLBiol, FLFishery, missing) {{{

setMethod("fwd", signature(object="FLBiol", fishery="FLFishery",
  control="missing"),
  
  function(object, fishery, ..., residuals=FLQuant(1, dimnames=dimnames(m(object)))) {
    
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

    out <- fwd(object, fishery, control=control, residuals=residuals)

    return(out)
  }
) # }}}

# fwd(FLStock, missing, fwdControl) {{{

setMethod("fwd", signature(object="FLStock", fishery="missing",
  control="fwdControl"),
  
  function(object, control, sr=predictModel(model=rec~a, params=FLPar(a=1)),
    residuals=FLQuant(1, dimnames=dimnames(rec(object)))) {
    
    # DEAL with iters
    its <- dims(object)$iter
    if(its > 1)
      object <- propagate(object, its)

    # COERCE to FLBiols
    B <- as(object, "FLBiol")
    if(is(sr, "predictModel"))
      rec(B) <- sr
    else if(is(sr, "FLSR"))
      rec(B) <- predictModel(model=model(sr), params=params(sr))
    else if(is(sr, "list")) {
      B@rec@model <- do.call(sr$model, list())[["model"]]
      B@rec@params <- sr$params
    }
    Bs <- FLBiols(B=B)

    # COERCE to FLFisheries
    F <- as(object, 'FLFishery')
    name(F) <- "F"
    names(F) <- "B"

    Fs <- FLFisheries(F=F)
    Fs@desc <- "F"

    # SET @FCB
    control@FCB <- matrix(1, ncol=3, nrow=1, dimnames=list(1, c("F", "C", "B")))

    # SET @target[fcb]
    control@target[c("fishery", "catch", "biol")] <- rep(c(NA, NA, 1), each=dim(control@target)[1])

    # Some targets require minAge and maxAge to be set
    # IF minAge and maxAge are NA and target is one of them, then range(min, max)
    # Fbar and F
    age_range_targets <- c("f", "fbar")
    control@target[,"minAge"] <- ifelse(is.na(control@target[,"minAge"]) & (control@target[,"quant"] %in% age_range_targets), range(object, "minfbar"), control@target[,"minAge"])
    control@target[,"maxAge"] <- ifelse(is.na(control@target[,"maxAge"]) & (control@target[,"quant"] %in% age_range_targets), range(object, "maxfbar"), control@target[,"maxAge"])

    # If relative targets (relYear) then we must also have relBiol (all FLStock targets can be related directly to the biol)
    if (any(!is.na(control@target$relYear))){
        relYear_rows <- !is.na(control@target$relYear)
        control@target$relBiol[relYear_rows] <- 1
    }

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
    object@landings <- landings(Fc)
    # landings.n
    object@landings.n <- Fc@landings.n
    # landings.wt
    object@landings.wt <- Fc@landings.wt
    # discards
    object@discards <- discards(Fc)
    # discards.n
    object@discards.n <- Fc@discards.n
    # discards.wt
    object@discards.wt <- Fc@discards.wt
    # catch
    object@catch <- catch(Fc)
    # catch.n
    object@catch.n <- catch.n(Fc)
    # catch.wt
    object@catch.wt <- catch.wt(Fc)
    # harvest (F)
    object@harvest[, pyrs] <- calc_F(Fc, Bo, eff)[, pyrs]
    units(object@harvest) <- "f"
    # stock.n
    object@stock.n <- Bo@n
    # stock
    object@stock <- quantSums(object@stock.n * object@stock.wt)

    return(object)
  }
) # }}}

# fwd(FLStock, missing, missing, ...) {{{

setMethod("fwd", signature(object="FLStock", fishery="ANY",
  control="missing"),
  
  function(object, fishery=missing, ..., sr=predictModel(model=rec~a, params=FLPar(a=1)),
    residuals=FLQuant(1, dimnames=dimnames(rec(object)))) {
    
    # PARSE ...
    args <- list(...)
    
    # HACK: deal with f assigned to fishery, might fail
    if(!missing(fishery)) {

      if(!is(fishery, "FLQuant"))
        stop("targets can only be of class FLQuant if no fwdControl is provided")
      narg <- names(sys.calls()[[length(sys.calls())-1]])
      narg <- narg[!narg %in% c("", "object", "sr",
        grep("^[f].*", .qlevels, value=TRUE, invert=TRUE))]
      args[[narg]] <- fishery
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
    
    return(fwd(object=object, control=control, residuals=residuals, sr=sr))
  }
) # }}}

# add_target_order {{{
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
} # }}}
