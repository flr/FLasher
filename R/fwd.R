# fwd.R - DESC
# fwd.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

#' Method for running fishery projections
#'
#' fwd() projects the fishery through time and attempts to hit the specified
#' targets by finding the appropriate fishing effort.
#'
#' A projection is run on either an FLStock object (for a single species, single
#' fishery projection), or a pair of FLBiol(s) and FLFishery(ies) objects
#' (for more advanced mixed fisheries projections).
#'
#' The projection is controlled by the fwdControl object (although it is also
#' possible to control the projection of an FLStock using a different interface).
#' In each timestep of the projection, the fishing effort of each FLFishery (or F 
#' multiplier if object is an FLStock) is found so that the targets specified in
#' the fwdControl object are hit.
#'
#' For more details and examples, see the vignettes in the package and also the
#' tutorial at: http://www.flr-project.org/doc/Forecasting_on_the_Medium_Term_for_advice_using_FLasher.html 
#'
#' @param object An FLStock, an FLBiol or an FLBiols object.
#' @param fishery If object is an FLBiol(s), a FLFishery(ies). Else this argument is ignored.
#' @param control A fwdControl object.
#' @param effort_max Maximum yearly rate of change in effort for each fishery
#' @param residuals An FLQuant of residuals for the stock recruitment relationship (if object is an FLStock).
#' @param sr a predictModel, FLSr or list that describes the stock recruitment relationship (if object is an FLStock).
#' @param ... Stormbending.
#'
#' @return Either an FLStock, or a list of FLFishery and FLBiol objects.
#'
#' @name fwd
#' @rdname fwd-methods
#' @aliases fwd,FLBiols,FLFisheries,fwdControl-method
#' 
#' @author The FLR Team
#' @seealso \link{FLComp}
#' @keywords classes

# fwd(FLBiols, FLFisheries, fwdControl) {{{

setMethod("fwd", signature(object="FLBiols", fishery="FLFisheries", control="fwdControl"),
    function(object, fishery, control, effort_max=rep(1e5, length(fishery)),
      residuals=lapply(lapply(object, spwn), "[<-", value=1)) {
 
  # CHECK valid fwdControl
  if(!validObject(control))
    stop("control object is not valid, please check")

  # CHECK length and names of biols and residuals
  if(!all.equal(names(object), names(residuals)))
    stop("Names of biols and residuals must match exactly")

  # CHECK for NAs in biol: m, n, wt
  bnas <- unlist(lapply(object, verify,
    m=~!is.na(m), n=~!is.na(n), wt=~!is.na(wt), report=FALSE))
 
  if(!all(bnas))
    stop("NAs present in the 'm', 'n' or 'wt' slots. Check object using verify()")  

  # CHECK years and dimensions match
  dib <- do.call(rbind, lapply(object, function(x) as.data.frame(dims(x))))
  dif <- do.call(rbind, lapply(fishery,
    function(x) data.frame(dims(effort(x))[c("season", "area")])))
  dnb <- dimnames(n(object[[1]]))

  # CHECK srparams years match control years
  cyrs <- unique(control$year)
  
  ysrp <- unlist(lapply(object, function(x) {
    # GET rec@params dimnames
    spdn <- dimnames(rec(x, "params"))$year
    # IF 'year' in dimnames
    if(!is.null(spdn)){
      # CHECK control years are covered 
      # If final projection year is not final year of biol, then fwd projects
      # the biol in the final+1 control year
      # e.g. if there is room to update biol in final control year +1
      if (max(cyrs) < max(as.numeric(dimnames(x@n)$year))){
          cyrs <- c(cyrs, max(cyrs)+1)
      }
      allyrs <- all(cyrs %in% as.numeric(spdn))
      # CHECK all params in the control years are not NA
      nayrs <- FALSE
      if(allyrs){
          nayrs <- all(!is.na(rec(x, "params")[,ac(cyrs)]))
      }
      return(nayrs & allyrs)
    }
    else
      TRUE
  }))
  
  if(!all(ysrp))
    stop("'years' specified in params(rec) do not match those in control or are NA (note that the 'final control year + 1' is projected if there is room)")
  
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
    # No NAs
    residuals[[i]][is.na(residuals[[i]])] <- 1
    biolscpp[[i]][["srr_residuals"]] <- residuals[[i]]
  }
  
  # CREATE FCB, if missing and possible
  if(dim(control@FCB)[1] == 1 & all(is.na(control@FCB)))
    control@FCB <- fcb2int(guessfcb(object, fishery), object, fishery)

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

  # ENSURE F,C,B cols are lists
  fcbcols <- c("biol", "fishery", "catch", "relBiol", "relFishery", "relCatch")
  for (i in fcbcols){
      trg <- do.call("$<-", list(trg, i, as.list(do.call("$", list(trg, i)))))
  }

  # Turn F, C and B names in control columns to integer positions
  trg <- match_posns_names(trg, names(object), lapply(fishery, names))

  # CONVERT 'years' and 'relYear' to position indices
  # quiet R CMD check about no visible binding for global variable
  year <- relYear <- NULL
  mny <- min(dib[,"minyear"]) - 1
  trg <- transform(trg, year=year - mny)
  trg <- transform(trg, relYear=relYear - mny)

  # Check relative columns make sense
  # If you have any of relFishery, relCatch or reBiol, you have must have relYear
  # any row with at least 1 relBiol that is not NA
  relBiol <- unlist(lapply(trg$relBiol, function(x) any(!is.na(x))))
  relFishery <- unlist(lapply(trg$relFishery, function(x) any(!is.na(x)))) 
  relCatch <- unlist(lapply(trg$relCatch, function(x) any(!is.na(x)))) 
  relYear <- !is.na(trg$relYear)
  relSeason <- !is.na(trg$relSeason)

  # Check if we if have any relYears AND it is an annual model
  # If so, check relSeason is NA or 1
  # If not -> error
  # If NA -> 1
  annual_model <- dim(n(object[[1]]))[4] == 1
  if (any(relYear) & annual_model) {
    # If relSeason is not NA or 1 throw an error
    if (!all(trg$relSeason[relYear] %in% c(NA, 1))){
      stop("With an annual model, if you have a relative target, relSeason must be set to 1 or NA")
    }
    # If relYear is present and relSeason is NA, set to 1
    trg$relSeason[!relSeason & relYear] <- 1
  }
  # Update this
  relSeason <- !is.na(trg$relSeason)

  # If relYear must have relSeason and vice versa
  if (any(relYear != relSeason)){
    stop("If you have a reYear you must also have a relSeason, and vice versa")
  }

  # if relYear, must have relFishery or relBiol, or (relCatch and relFishery)
  if(any(relYear != (relFishery | relBiol | (relCatch & relFishery)))){
    stop("If relYear set must also set a relBiol or relFishery or (FLFishery and relCatch), and vice versa")
  }
  
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

  # CHANGE zero effort to small value
  fishery <- lapply(fishery, function(x){
    x@effort[x@effort == 0] <- 1e-6
    return(x)
  })
  # CALL oMRun
  out <- operatingModelRun(fishery, biolscpp, control, effort_max=effort_max,
    effort_mult_initial = 1.0, indep_min = 1e-6, indep_max = 1e12, nr_iters = 50)

  # UPDATE object w/ new biolscpp@n
  for(i in names(object))
    n(object[[i]]) <- out$om$biols[[i]]@n

  # RETURN list(object, fishery, control)
  out <- list(biols=object, fisheries=out$om$fisheries, control=control,
    flag=out$solver_codes)

  # WARNING for effort_max
  if(any(unlist(
  lapply(out$fisheries, function(x) max(effort(x), na.rm=TRUE))) == effort_max))
    warning("Maximum effort limit reached in one or more fisheries")

  return(out)
  }

) # }}}

# fwd(FLBiols, FLFishery, fwdControl) {{{
#' @rdname fwd-methods
#' @aliases fwd,FLBiols,FLFishery,fwdControl-method
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
#' @rdname fwd-methods
#' @aliases fwd,FLBiol,FLFisheries,fwdControl-method
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
#' @rdname fwd-methods
#' @aliases fwd,FLBiol,FLFishery,fwdControl-method

setMethod("fwd", signature(object="FLBiol", fishery="FLFishery",
  control="fwdControl"),
  
  function(object, fishery, control,
    residuals=FLQuant(1, dimnames=dimnames(rec(object))), ...) {

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
    out <- fwd(Bs, Fs, control, residuals=FLQuants(B=residuals), ...)

    # PARSE output
    Fc <- out$fisheries[[1]][[1]]
    Bo <- out$biols[[1]]

    return(list(biols=Bo, fisheries=Fc))
  }
) # }}}

# fwd(FLBiol, FLFishery, missing) {{{

#' @rdname fwd-methods
#' @aliases fwd,FLBiols,FLFisheries,missing-method
setMethod("fwd", signature(object="FLBiol", fishery="FLFishery",
  control="missing"),
  
  function(object, fishery, ..., effort_max=10,
    residuals=FLQuant(1, dimnames=dimnames(m(object)))) {
    
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

    out <- fwd(object, fishery, control=control, residuals=residuals,
      effort_max=effort_max)

    return(out)
  }
) # }}}

# fwd(FLStock, missing, fwdControl) {{{

#' @rdname fwd-methods
#' @aliases fwd,FLStock,missing,fwdControl-method
setMethod("fwd", signature(object="FLStock", fishery="missing",
  control="fwdControl"),
  
  function(object, control, sr, effort_max=4,
    residuals=FLQuant(1, dimnames=dimnames(rec(object))), ...) {  
    
    # CHECK for NAs in stock: m, stock.n, stock.wt
    snas <- verify(object, m=~!is.na(m), stock.n=~!is.na(stock.n),
      stock.wt=~!is.na(stock.wt), report=FALSE)
 
    if(!all(snas))
      stop("NAs present in the 'm', 'stock.n' or 'stock.wt' slots. Check object using verify()")  

    # DEAL with iters
    its <- max(dims(object)$iter, dim(iters(control))[3])
    if(its > 1) {
      # TODO propagate only necessary slots (stock.n, catch.n, harvest)
      object <- propagate(object, its)
    }

    # COERCE to FLBiols
    B <- as(object, "FLBiol")

    # PARSE sr and ADD to B
    if(is(sr, "predictModel")) {
      rec(B) <- sr
    } else if(is(sr, "FLSR")){
      rec(B) <- predictModel(model=model(sr), params=params(sr))
    #  if(missing(residuals)) {
        # CHECK logerror
    #    if(sr@logerror)
          # TODO CHECK dims
    #      residuals <- exp(residuals(sr))
    #  }
    } else if(is(sr, "FLBRP")) {
      B@rec@model <- model(sr)
      B@rec@params <- params(sr)
    } else if(is(sr, "list")) {
      if(is(sr$model, "character")) {
        B@rec@model <- do.call(sr$model, list())[["model"]]
      } else if(is(sr$model, "formula")) {
        B@rec@model <- sr$model
      }
      B@rec@params <- sr$params
    }
    
    Bs <- FLBiols(B=B)
    
    # COERCE to FLFisheries
    F <- as(object, 'FLFishery')
    name(F) <- "F"
    names(F) <- "B"

    # RESCALE effort
    idx <- effort(F)[, ac(control$year[1] - 1)]
    effort(F)[, ac(control$year[1] - 1)][idx==0] <- 1e-6
    Fs <- FLFisheries(F=F)
    Fs@desc <- "F"

    # SET @FCB
    control@FCB <- matrix(1, ncol=3, nrow=1, dimnames=list(1, c("F", "C", "B")))

    # SET @target[fcb]
    control@target[c("fishery", "catch", "biol")] <- rep(c(NA, NA, 1),
      each=dim(control@target)[1])

    # Some targets require minAge and maxAge to be set
    # IF minAge and maxAge are NA and target is one of them, then range(min, max)
    # Fbar and F
    age_range_targets <- c("f", "fbar")
    control@target[,"minAge"] <- ifelse(
      is.na(control@target[,"minAge"]) & (control@target[,"quant"] %in% age_range_targets),
        range(object, "minfbar"), control@target[,"minAge"])
    control@target[,"maxAge"] <- ifelse(
      is.na(control@target[,"maxAge"]) & (control@target[,"quant"] %in% age_range_targets),
        range(object, "maxfbar"), control@target[,"maxAge"])

    # If relative targets (relYear) then we must also have relBiol, as
    #   all FLStock targets can be related directly to the biol
    if (any(!is.na(control@target$relYear))){
        relYear_rows <- !is.na(control@target$relYear)
        control@target$relBiol[relYear_rows] <- 1
        # And if target needs minAge and maxAge we also need relMaxAge etc
        # Set as minAge and maxAge of control
        control@target[,"relMinAge"] <- ifelse(
          is.na(control@target[,"relMinAge"]) & (control@target[,"quant"] %in% age_range_targets),
            control@target[,"minAge"], control@target[,"relMinAge"])
        control@target[,"relMaxAge"] <- ifelse(
          is.na(control@target[,"relMaxAge"]) & (control@target[,"quant"] %in% age_range_targets),
            control@target[,"maxAge"], control@target[,"relMaxAge"])
    }

    # RUN
    out <- fwd(Bs, Fs, control, residuals=FLQuants(B=residuals),
      effort_max=effort_max, ...)
    
    # PARSE output
    Fc <- out$fisheries[[1]][[1]]
    eff <- out$fisheries[[1]]@effort
    Bo <- out$biols[[1]]
    
    # PROJECTION years
    miny <- min(control@target$year)
    maxy <- max(control@target$year)
    
    # RETURN one more year if ssb_flash and  *.spwn == 0
    if(any(control[control$year == maxy,]$quant == "ssb_flash") &
      sum(spwn(Bo)[,ac(maxy)]) == 0) {
      maxy <- min(maxy + 1, dims(Bo)$maxyear)
    }
    
    pyrs <- as.character(seq(miny, maxy))
    
    # landings
    object@landings[,pyrs] <- landings(Fc)[,pyrs]
    # landings.n
    object@landings.n[,pyrs] <- Fc@landings.n[,pyrs]
    # landings.wt
    object@landings.wt[,pyrs] <- Fc@landings.wt[,pyrs]
    # discards
    object@discards[,pyrs] <- discards(Fc)[,pyrs]
    # discards.n
    object@discards.n[,pyrs] <- Fc@discards.n[,pyrs]
    # discards.wt
    object@discards.wt[,pyrs] <- Fc@discards.wt[,pyrs]
    # catch
    object@catch[,pyrs] <- catch(Fc)[,pyrs]
    # catch.n
    object@catch.n[,pyrs] <- catch.n(Fc)[,pyrs]
    # catch.wt
    object@catch.wt[,pyrs] <- catch.wt(Fc)[,pyrs]
    # harvest (F)
    object@harvest[, pyrs] <- calc_F(Fc, Bo, eff)[, pyrs]
    units(object@harvest) <- "f"
    
    # stock.n
    object@stock.n[,pyrs] <- Bo@n[,pyrs]
    # stock
    object@stock <- quantSums(object@stock.n * object@stock.wt)
    return(object)
  }
) # }}}

# fwd(FLStock, missing, missing, ...) {{{

#' @rdname fwd-methods
#' @aliases fwd,FLStock,missing,missing-method
setMethod("fwd", signature(object="FLStock", fishery="ANY", control="missing"),
  function(object, fishery=missing, sr, effort_max=10,
    residuals=FLQuant(1, dimnames=dimnames(rec(object))), ...) {  
    
    # PARSE ...
    args <- list(...)

    # ERROR for ctrl
    if("ctrl" %in% names(args))
      stop("Did you mean to give me a fwdControl object? Use 'control=' instead.")
    
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
    
    return(fwd(object=object, control=control, residuals=residuals, sr=sr,
      effort_max=effort_max))
  }
) # }}}
