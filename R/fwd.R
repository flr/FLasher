# fwd.R - Forecasting the future
# FLasher/R/fwd.R

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
#' In each timestep of the projection, the fishing effort of each FLFishery (or
#' the equivalent F  multiplier if object is an FLStock) is found so that the
#' targets specified in the fwdControl object are hit.
#'
#' For more details and examples, see the vignettes in the package and also the
#' tutorial at: http://www.flr-project.org/doc/Forecasting_on_the_Medium_Term_for_advice_using_FLasher.html 
#'
#' @param object An FLStock, an FLBiol or an FLBiols object.
#' @param fishery If object is an FLBiol(s), a FLFishery(ies). Else this argument is ignored.
#' @param control A fwdControl object.
#' @param effort_max Sets a maximum effort limit by fishery as a multiplier over the maximum observed effort.
#' @param maxF Maximum yearly fishing mortality, when called on an FLStock object.
#' @param deviances An FLQuant of deviances for the stock recruitment relationship (if object is an FLStock).
#' @param residuals Old argument name for deviances, to be deleted
#' @param sr a predictModel, FLSR or list that describes the stock recruitment relationship (if object is an FLStock). Also an FLQuant with actual recruitment values.
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
    function(object, fishery, control, effort_max=rep(100, length(fishery)),
      deviances=residuals, residuals=lapply(lapply(object, spwn),
      "[<-", value=1)) {
  
  # CHECK valid fwdControl
  if(!validObject(control))
    stop("control object is not valid, please check")

  # CHECK length and names of biols and deviances
  if(!all.equal(names(object), names(deviances)))
    stop("Names of biols and deviances must match exactly")

  # TODO CHECK iters biols, fisheries, control
  
  # TODO years
  # CHECK for NAs in biol: m, n, wt
  # bnas <- unlist(lapply(object, verify,
  #   m=~!is.na(m), n=~!is.na(n), wt=~!is.na(wt), report=FALSE))
 
  # if(!all(bnas))
  #   stop("NAs present in the 'm', 'n' or 'wt' slots. Check object using verify()")  

  # CHECK years and dimensions match
  dib <- do.call(rbind, lapply(object, function(x) as.data.frame(dims(x))))
  dif <- do.call(rbind, lapply(fishery,
    function(x) data.frame(dims(effort(x))[c("season", "area")])))
  dnb <- dimnames(n(object[[1]]))

  # CHECK srparams years match control years
  cyrs <- unique(control$year)
  
  ysrp <- unlist(lapply(object, function(x) {
                          
    # GET rec@params dimnames
    spdn <- dimnames(params(sr(x)))$year
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
      nayrs <- TRUE
      # DEBUG year with units, seasons
#      if(allyrs){
#          nayrs <- all(!is.na(rec(x, "params")[,ac(cyrs)]))
#      }
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

  # FIND iters with no NAs in target
  idn <- !c(unlist(apply(is.na(control@iters), 3,
    function(x) any(rowSums(x) == 3))))

  # STOP if all iters have NAs in target
  if(all(!idn))
    stop("all target iter contain only NA.")

  # CONVERT biols to list(list(object, name, params, deviances, mult)), no NAs
  biolscpp <- lapply(object, function(x) as(iter(x, idn), "list"))
  
  # SUBSET idn on deviances
  deviances <- iter(FLQuants(deviances), idn)
  
  # deviances must be same dim 2-5 as the biol
  # ADD deviances
  for(i in names(biolscpp)) {
    bdnms <- dimnames(n(biolscpp[[i]][["biol"]]))
    year_range <- range(as.numeric(bdnms$year)) 
    # Need a window equivalent for year and season
    deviances[[i]] <- window(deviances[[i]], start=year_range[1],
      end=year_range[2])
    # No NAs
    deviances[[i]][is.na(deviances[[i]])] <- 1
    biolscpp[[i]][["srr_deviances"]] <- deviances[[i]]
  }
  
  # CREATE FCB, if missing and possible
  if(dim(control@FCB)[1] == 1 & all(is.na(control@FCB)))
    control@FCB <- fcb2int(guessfcb(object, fishery), object, fishery)

  # TODO CHECK FCB exists, else stop()

  # PARSE control
  trg <- target(control)

  # CHECK F targets have min/maxAge
  if(any(is.na(trg[trg$quant %in% c("f", "fbar"), c("minAge", "maxAge")])))
    stop("minAge and maxAge are needed in control for an 'f' or 'fbar' target.")

  # CONVERT NA and 0 (< sqrt(.Machine$double.eps)) targets to 1e-6
  t0s <- !is.na(iters(control)[, "value",]) &
    iters(control)[, "value",] < .Machine$double.eps
  iters(control)[,"value",][t0s] <- 1e-6

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

  # quiet R CMD check about no visible binding for global variable
  year <- relYear <- NULL
  # CONVERT 'years' and 'relYear' to position indices
  mny <- min(dib[,"minyear"]) - 1
  trg <- transform(trg, year=year - mny)
  trg <- transform(trg, relYear=relYear - mny)

  # Check relative columns make sense
  # If you have any of relFishery, relCatch or reBiol, you have must have
  # relYear in any row with at least 1 relBiol that is not NA
  relBiol <- unlist(lapply(trg$relBiol, function(x) any(!is.na(x))))
  relFishery <- unlist(lapply(trg$relFishery, function(x) any(!is.na(x)))) 
  relCatch <- unlist(lapply(trg$relCatch, function(x) any(!is.na(x)))) 
  relYear <- !is.na(trg$relYear)
  relSeason <- !is.na(trg$relSeason)

  # Check if we if have any relYears AND it is an annual model
  # If so, check relSeason is NA or 1
  annual_model <- dim(n(object[[1]]))[4] == 1
  if (any(relYear) & annual_model) {
    # If relSeason is not NA or 1 throw an error
    if (!all(trg$relSeason[relYear] %in% c(NA, 1))){
      stop("With an annual model, if you have a relative target,
        relSeason must be set to 1 or NA")
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

  # SUBSET fishery & control TODO iter(fwdControl)<-
  control@iters <- control@iters[,,idn, drop=FALSE]
  
  # SUBSET fishery, new object TODO iter(FLFishery)<-
  rfishery <- lapply(fishery, iter, idn)

  # CHANGE zero effort to small value
  rfishery <- lapply(rfishery, function(x){
    x@effort[x@effort == 0] <- sqrt(.Machine$double.eps)
    # SET effort to solve as total effort
    x@effort  <- x@effort * x@capacity
    x@capacity[]  <- 1
    return(x)
  })

    # CALCULATE max(effort) per fishery
  effscale <- unname(unlist(lapply(rfishery, function(x) max(x@effort))))
  
  # CALL operatingModelRun
  # TODO PASS to C++ only from last year of rfishery and biolscpp
  out <- operatingModelRun(rfishery, biolscpp, control,
    effort_max = c(effort_max * effscale), effort_mult_initial = 1.0,
    indep_min = .Machine$double.eps, indep_max = 1e12, nr_iters = 50)
  
  # WARN of unsolved targets
  if(any(out$solver_codes != 1)) {
 
    ids <- which(out$solver_codes != 1, arr.ind=TRUE)

    warning(paste("Unsolved targets at control rows: ",
      paste(unique(ids[,"row"]), collapse=", ")))
  }

  # STRUCTURE of out
  #
  # out
  # |- om
  # |  |- biols
  # |  |  |- B: FLBiolcpp
  # |  |  |  \- @n
  # |  |  \- [...]
  # |  |- fisheries
  # |  |  |- F: FLFisherycpp
  # |  |  |  |- @effort
  # |  |  |  |- @capacity
  # |  |  |  \- [[C]]
  # |  |  |     |- @landings.n
  # |  |  |     \- @discards.n
  # |  |  \- [...]
  # |  \- ctrl
  # \- solver_codes: data.frame (timestep x iters)


  # UPDATE object w/ new biolscpp@n
  for(i in names(object)) {
    n(object[[i]])[,ac(cyrs),,,,idn] <- out$om$biols[[i]]@n[,ac(cyrs),,,,]
    # DEBUG SET NAs as 1e-8
    n(object[[i]])[,ac(cyrs),,,,idn][is.na(n(object[[i]])[,ac(cyrs),,,,idn])] <- 1
    # SET n on not-run iters, on cyrs, as 1e-8
    if(any(!idn)) {
      n(object[[i]])[,ac(cyrs),,,,!idn] <- 1e-8
    }
  }
  
  # UPDATE fisheries
  for(i in names(fishery)) {
    
    fsh <- fishery[[i]]

    # PROPAGATE effort
    if(dim(fsh@effort)[6] != dim(effort(out$om$fisheries[[i]]))[6])
      fsh@effort <- propagate(fsh@effort, length(idn))
    
    # UPDATE effort scaled by capacity
    fsh@effort[,ac(cyrs),,,, idn] <-
      effort(out$om$fisheries[[i]])[, ac(cyrs)] /
      iter(fsh@capacity[,ac(cyrs),,,,], idn)
    
    # SET not-run iters, on cyrs, as NA
    if(any(!idn))
      fsh@effort[,ac(cyrs),,,,!idn] <- NA
    
    for(j in names(fsh)) {
      
      # UPDATE catches
      for(sl in c("landings.n", "discards.n")) {
        if(dim(slot(fsh[[j]], sl))[6] != dim(slot(out$om$fisheries[[i]][[j]], sl))[6])
        slot(fsh[[j]], sl) <- propagate(slot(fsh[[j]], sl), length(idn))
        slot(fsh[[j]], sl)[,ac(cyrs),,,,idn] <-
          slot(out$om$fisheries[[i]][[j]], sl)[,ac(cyrs),,,,]
      # SET landings.n and discards.n of not-run iters, on cyrs, as NA
        if(any(!idn))
          slot(fsh[[j]], sl)[,ac(cyrs),,,,!idn] <- NA
      }
    }
    fishery[[i]] <- fsh
  }

  # RETURN list(object, fishery, control)
  out <- list(biols=object, fisheries=fishery, control=control,
    flag=out$solver_codes)

  # WARNING for effort_max
   if(any(unlist(lapply(fishery,
    function(x) max(x@effort, na.rm=TRUE))) == effort_max))
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
  
  function(object, fishery, control, deviances="missing", ...) {
    
    # FIND biol name
    fcb <- FCB(control)
    if(!all(is.na(fcb)))
      idx <- fcb[,"C"][fcb[, "B"] == 1][1]
    else
      idx <- 1
    nms <- names(fishery[[1]])[idx]

    # NAME biols
    object <- FLBiols(B=object)
    names(object) <- nms
    
    # NAME deviances
    if(!missing(deviances)) {
      deviances <- FLQuants(D=deviances)
      names(deviances) <- nms
      res <- fwd(object=object, fishery=fishery, control=control,
        deviances=deviances, ...)
    } else {
      res <- fwd(object=object, fishery=fishery, control=control, ...)
    }

    return(list(biol=res$biols[[1]], fisheries=res$fisheries))
  }
) # }}}

# fwd(FLBiol, FLFishery, fwdControl) {{{
#' @rdname fwd-methods
#' @aliases fwd,FLBiol,FLFishery,fwdControl-method

setMethod("fwd", signature(object="FLBiol", fishery="FLFishery",
  control="fwdControl"),
  
  function(object, fishery, control, deviances=residuals,
    residuals=FLQuant(1, dimnames=dimnames(rec(object))), ...) {

    # COERCE to FLBiols and FLFisheries
    Bs <- FLBiols(B=object)
    Fs <- FLFisheries(F=fishery)
    Fs@desc <- "F"

    # SET @FCB
    control@FCB <- matrix(1, ncol=3, nrow=1, dimnames=list(1, c("F", "C", "B")))
    
    # TODO SET @target[fcb] if missing
    # control@target[c("fishery", "catch", "biol")]
    # control@target[c("fishery", "catch", "biol")] <- rep(c(NA, NA, 1),
    #  each=dim(control@target)[1])

    # RUN
    out <- fwd(Bs, Fs, control, deviances=FLQuants(B=deviances), ...)

    # PARSE output
    Fc <- out$fisheries[[1]]
    Bo <- out$biols[[1]]

    return(list(biol=Bo, fishery=Fc))
  }
) # }}}

# fwd(FLBiol, FLFishery, missing) {{{

#' @rdname fwd-methods
#' @aliases fwd,FLBiols,FLFisheries,missing-method
setMethod("fwd", signature(object="FLBiol", fishery="FLFishery",
  control="missing"),
  
  function(object, fishery, ..., effort_max=10, deviances=residuals,
    residuals=FLQuant(1, dimnames=dimnames(m(object)))) {
    
    # PARSE ...
    args <- list(...)
    
    # Does ... exist?
    if(length(args) < 1)
      stop("No fwdControl provided and no FLQuant targets given, cannot do anything!")

    # NAMES in qlevels?
    if(!names(args) %in% .qlevels)
      stop(
      paste0("Names of input FLQuant(s) do not match current allowed targets: ",
      paste(.qlevels, collapse=", ")))

    args <- FLQuants(args)

    control <- as(args, "fwdControl")

    out <- fwd(object, fishery, control=control, deviances=deviances,
      effort_max=effort_max)

    return(out)
  }
) # }}}

# fwd(FLStock, missing, fwdControl) {{{

#' @rdname fwd-methods
#' @aliases fwd,FLStock,missing,fwdControl-method
#' @examples
#' data(ple4)
#' # Hindcast with past rec and catch
#' hind <- fwd(ple4, sr=rec(ple4)[, ac(1980:2017)],
#'   control=fwdControl(year=1980:2017, value=fbar(ple4)[, ac(1980:2017)],
#'   quant="fbar"))
#' plot(FLStocks(PLE=ple4, FWD=hind))

setMethod("fwd", signature(object="FLStock", fishery="missing",
  control="fwdControl"),
  
  function(object, control, sr, maxF=4, deviances=residuals,
    residuals=FLQuant(1, dimnames=dimnames(rec(object))),
    effort_max=1e12, ...) {  
    
    # COMPUTE first year and season in control
    fy <- which(ac(control$year[1]) == dimnames(m(object))$year)
    fs <- which(ac(control$season[1]) == dimnames(m(object))$season)

    # TODO CHECK if seasons in object but not in control, stop()

    # FIND OUT starting time step
    if(fs == 1) {
      fy <- fy - 1
    } else {
      fs <- fs - 1
    }

    # CHECK for NAs in stock: m, stock.n, stock.wt in first year and season
    snas <- verify(object[,fy,,fs], rules=NULL, report=FALSE,
      m=~!is.na(m), stock.n=~!is.na(stock.n), stock.wt=~!is.na(stock.wt))

    if(!all(snas))
      stop(paste("NAs present in the 'm', 'stock.n' or 'stock.wt' slots,
        year:", fy, ", season:", fs))

    # DEAL with iters
    its <- max(dims(object)$iter, dim(iters(control))[3])
    if(its > 1 & dims(object)$iter == 1) {
      # TODO propagate only necessary slots (stock.n, catch.n, harvest)
      object <- propagate(object, its)
    }

    # PROJECTION years
    miny <- min(control@target$year)
    maxy <- max(control@target$year)
    pyrs <- as.character(seq(miny, maxy))

    # CHECK for missing discards ratio info
    dnas <- verify(object[,pyrs,,], report=FALSE,
      rules=list(discards.n=~!is.na(discards.n), discards.wt=~!is.na(discards.wt)))

    if(!all(dnas))
      stop(paste("NAs present in the 'discards.n' or 'discards.wt' slots in
        projection years. Cannot compute discards ratio. Set to 0 if none exist."))

    # COERCE to FLBiols
    B <- as(object, "FLBiol")

    # PARSE sr and ADD to B
    # predictModel
    if(is(sr, "predictModel")) {
      rec(B) <- sr
    # FLQuant
    } else if(is(sr, "FLQuant")){
      rec(B) <- predictModel(model=rec~a, params=FLPar(c(sr),
        dimnames=list(params="a", year=dimnames(sr)$year,
          unit=dimnames(sr)$unit, iter=dimnames(sr)$iter)))
    # FLSR
    } else if(is(sr, "FLSR")){
      rec(B) <- predictModel(model=model(sr), params=params(sr))
    # FLBRP
    } else if(is(sr, "FLBRP")) {
      B@rec@model <- model(sr)
      B@rec@params <- params(sr)
    # list(model, params)
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
    
    # ADD matching names
    name(F) <- "F"
    names(F) <- "B"

    # CREATE fisheries
    Fs <- FLFisheries(F=F)
    Fs@desc <- "F"

    # SET @FCB as c(1,1,1)
    control@FCB <- matrix(1, ncol=3, nrow=1, dimnames=list(1, c("F", "C", "B")))

    # SET @target[fcb] as biol 1
    brow <- !control@target$quant %in% c("effort", "revenue")
    control@target[brow, c("fishery", "catch", "biol")] <-
      rep(c(NA, NA, 1), each=sum(brow))
    
    # EXCEPT for effort and revenue, fishery 1
    if(sum(!brow) > 0) {
      control@target[!brow, c("fishery", "catch", "biol")] <-
        rep(c(1, NA, NA), each=sum(!brow))
    }
    
    # ADD maxF to control, only in years with no fbar target
    idx <- by(control@target[, c("quant", "year")],
      control@target$year, function(x) any(c("f", "fbar") %in% x$quant))

    yrs <- names(idx)[!idx]
    iseas <- length(unique(control$season)) > 1
    
    if(length(yrs) > 0 & !is.null(maxF) & !iseas) {
      
      # MERGE controls
      maxFc <- fwdControl(year=yrs, quant="fbar", min=0, max=maxF, biol=1)
     
      target <- rbind(control@target, maxFc@target)

      diters <- c(dim(target)[1], 3, dim(control@iters)[3])
      iters <- array(NA, dim=diters, dimnames=list(
        row=seq(diters[1]), val=c("min", "value", "max"),
        iter=seq(diters[3])))

      dlim <- dim(control@iters)[1]
      iters[seq(1, dlim) ,,] <- control@iters
      iters[seq(dlim + 1, diters[1]),,] <- maxFc@iters

      control@target <- target
      control@iters <- iters
    }

    # CHECK targets that require minAge and maxAge to be set
    # IF minAge and maxAge are NA and target is one of them, then range(min, max)
    
    # Fbar and F
    age_range_targets <- c("f", "fbar")

    control@target[,"minAge"] <- ifelse(
      is.na(control@target[,"minAge"]) & (control@target[,"quant"] %in% age_range_targets),
        range(object, "minfbar"), control@target[,"minAge"])

    control@target[,"maxAge"] <- ifelse(
      is.na(control@target[,"maxAge"]) & (control@target[,"quant"] %in% age_range_targets),
        range(object, "maxfbar"), control@target[,"maxAge"])

    # CHECK relBiol or relFishery if relYear
    if (any(!is.na(control@target$relYear))){
      
      # BIOL target
      control@target[!control@target$quant %in% c("effort","revenue") & 
        !is.na(control@target$relYear), "relBiol"] <- 1

      control@target[!control@target$quant %in% c("effort","revenue") & 
        !is.na(control@target$relYear), c("relMinAge", "relMaxAge")] <- 
      control@target[!control@target$quant %in% c("effort","revenue") & 
        !is.na(control@target$relYear), c("minAge", "maxAge")] 

      # FISHERY target
      control@target[control@target$quant %in% c("effort","revenue") & 
        !is.na(control@target$relYear), "relFishery"] <- 1
    }

    control <- add_target_order_fls(control)

    # RUN
    out <- fwd(Bs, Fs, control, deviances=FLQuants(B=deviances),
      effort_max=effort_max, ...)

    # PARSE output
    Fc <- out$fisheries[[1]][[1]]
    eff <- out$fisheries[[1]]@effort
    Bo <- out$biols[[1]]
    
    # RETURN one more year if ssb_flash and  *.spwn == 0
    if(any(control[control$year == maxy,]$quant == "ssb_flash") &
      sum(spwn(Bo)[,ac(maxy)]) == 0) {
      maxy <- min(maxy + 1, dims(Bo)$maxyear)
      pyrs <- as.character(seq(miny, maxy))
    }
 
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
    # object@harvest[, pyrs] <- harvest(n(Bo)[, pyrs], catch.n(Fc)[, pyrs],
    #   m(Bo)[, pyrs])
    units(object@harvest) <- "f"

    # stock.n
    object@stock.n[,pyrs] <- Bo@n[,pyrs]
    # stock
    object@stock <- quantSums(object@stock.n * object@stock.wt)

    return(object)
  }
) # }}}

# fwd(FLStock, ANY, missing, ...) {{{

#' @rdname fwd-methods
#' @aliases fwd,FLStock,missing,missing-method
#' @examples
#' #' Hindcast with past rec and catch
#' hind <- fwd(ple4, sr=rec(ple4)[, ac(1980:2017)],
#'   catch=catch(ple4)[, ac(1980:2017)])

setMethod("fwd", signature(object="FLStock", fishery="ANY", control="missing"),
  function(object, fishery=missing, sr, maxF=4, deviances=residuals,
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
      narg <- narg[!narg %in% c("", "object", "sr", "deviances",
        grep("^[f].*", .qlevels, value=TRUE, invert=TRUE))]
      args[[narg]] <- fishery
    }

    if (length(args) == 0) {
      
      dms <- dims(object)
      res <- window(object, start=dms$minyear + 1, end=dms$maxyear + 1)
    
      # ADD plusgroup
      stock.n(res)[ac(dms$max),] <-
        quantSums(stock.n(object)[ac(seq(dms$max - 1, dms$max)),] *
          exp(-z(object)[ac(seq(dms$max - 1, dms$max)),]))
    
      # MOVE other ages
      stock.n(res)[ac(seq(dms$min + 1, dms$max - 1)), ] <-
        stock.n(object)[ac(seq(dms$min, dms$max - 2)), ] *
          exp(-z(object)[ac(seq(dms$min, dms$max - 2)), ])

      # GET rec
      if(any(is(sr) %in% c("numeric", "FLPar", "FLQuant"))) {
        stock.n(res)[1, ] <- c(sr)[1]
      } else {
        fvars <- all.vars(model(sr)[[length(model(sr))]])
        funs <- fvars[!fvars %in% dimnames(params(sr))$params]
        pargs <- lapply(setNames(nm=funs), do.call, args=list(object))
      
        stock.n(res)[1, ] <- do.call("predict",
          c(list(object=as(sr, "predictModel")), pargs))
      }
      return(res)
    }
    
    # Does ... exist?
    if(length(args) < 1)
      stop("No fwdControl provided and no FLQuant targets given, cannot do anything!")

    # NAMES in qlevels?
    if(any(!names(args) %in% .qlevels))
      stop(paste0("Names of input FLQuant(s) do not match current allowed targets: ", paste(.qlevels, collapse=", ")))

    args <- FLQuants(args)

    # COERCE to fwdControl
    control <- as(args, "fwdControl")
    
    return(fwd(object=object, control=control, deviances=deviances, sr=sr, maxF=maxF))
  }
) # }}}
