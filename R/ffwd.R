# ffwd.R - Fast fwd for simple fbar targets on FLStock only.
# FLasher/R/ffwd.R

# Copyright Iago MOSQUEIRA (WMR), 2021
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>
#
# Distributed under the terms of the EUPL-1.2


# ffwd {{{ 

#' Project forward an FLStock for a fbar target
#'
#' Projection of an FLStock object for a fishing mortality target does not
#' always require the features of fwd().Fast-forward an FLStock object for a fishing mortality yearly target only.
#' 
#' @param object An *FLStock*
#' @param sr A stock-recruit relationship, *FLSR* or *predictModel*.
#' @param fbar Yearly target for average fishing mortality, *FLQuant*.
#' @param control Yearly target for average fishing mortality, *fwdControl*.
#' @param deviances Deviances for the strock-recruit relationsip, *FLQuant*.
#'
#' @return The projected *FLStock* object.
#'
#' @author Iago MOSQUEIRA (MWR), Henning WINKEL (JRC).
#' @seealso \link{fwd}
#' @keywords classes
#' @examples
#' data(ple4)
#' sr <- predictModel(model=bevholt, params=FLPar(a=140.4e4, b=1.448e5))
#' # Project for fixed Fbar=0.21
#' run <- ffwd(ple4, sr=sr, fbar=FLQuant(0.21, dimnames=list(year=1958:2017)))
#' plot(run)
#' # Same projection with fwd()
#' test <- fwd(ple4, sr=sr, fbar=FLQuant(0.21, dimnames=list(year=1958:2017)))
#' plot(run, test)

ffwd <- function(object, sr, fbar=control, control=fbar, deviances="missing") {

    # DIMS
    dm <- dim(object)
    dms <- dims(object)

    # EXTRACT slots
    naa <- stock.n(object)
    maa <- m(object)
    faa <- harvest(object)
    sel <- catch.sel(object)

    # DEVIANCES
    if(missing(deviances)) {
      deviances <- rec(object) %=% 1
    }

    # WINDOW deviances to match stock
    deviances <- window(deviances, start=dms$minyear)
    
    # HANDLE fwdControl
    if(is(fbar, "fwdControl")) {
      # CHECK single target per year & no max/min
      if(length(fbar$year) != length(unique(fbar$year)))
        stop("ffwd() can only project for yearly targets, try calling fwd().")

      # CHECK no max/min
      if(any(is.na(iters(fbar)[, "value",])))
        stop("ffwd() can only handle targets and not min/max limits, try calling fwd().")
      
      # CHECK target is fbar/f
      if(!all(fbar$quant %in% c("f", "fbar")))
        stop("ffwd() can only project for f/fbar targets, try calling fwd().")

      fbar <- faa[1, ac(fbar$year)] %=% fbar$value
    }

    # SET years
    yrs <- match(dimnames(fbar)$year, dimnames(object)$year)
    
    # COMPUTE harvest
    fages <- range(object, c("minfbar", "maxfbar"))
    faa[, yrs] <- (sel[, yrs] %/%
      quantMeans(sel[ac(seq(fages[1], fages[2])), yrs])) %*% fbar

    # COMPUTE SRP multiplier
    waa <- stock.wt(object)
    mat <- mat(object)
    msp <- m.spwn(object)
    fsp <- harvest.spwn(object)
    srp <- exp(-(faa * fsp) - (maa * msp)) * waa * mat

    # LOOP over years (i is new year)
    for (i in yrs - 1) {
      # rec * deviances
      naa[1, i + 1] <- eval(sr@model[[3]],   
        c(as(sr@params, 'list'), list(ssb=c(colSums(naa[, i] * srp[, i]))))) *
        c(deviances[, i + 1])
      # n
      naa[-1, i + 1] <- naa[-dm[1], i] * exp(-faa[-dm[1], i] - maa[-dm[1], i])
      # pg
      naa[dm[1], i + 1] <- naa[dm[1], i + 1] +
        naa[dm[1], i] * exp(-faa[dm[1], i] - maa[dm[1], i])
    }

  # UPDATE stock.n & harvest

  stock.n(object) <- naa
  harvest(object) <- faa
  
  # UPDATE stock, ...
  stock(object) <- computeStock(object)

  # catch.n
  catch.n(object)[,-1] <- (naa * faa / (maa + faa) * (1 - exp(-faa - maa)))[,-1]

  # landings.n & discards.n to 0 if NA
  landings.n(object)[is.na(landings.n(object))] <- 0
  discards.n(object)[is.na(discards.n(object))] <- 0
  
  # landings.n from catch.n and ratio
  landings.n(object) <- catch.n(object) * (landings.n(object) / 
    (discards.n(object) + landings.n(object)))

  # discards
  discards.n(object) <- catch.n(object) - landings.n(object)

  # catch.wt
  catch.wt(object) <- (landings.wt(object) * landings.n(object) + 
    discards.wt(object) * discards.n(object)) / catch.n(object)

  # catch
  catch(object) <- quantSums(catch.n(object) * catch.wt(object))

  return(object)
}
# }}}

setGeneric("cfwd", function(object, fisheries, ...) standardGeneric("cfwd"))

# cfwd (FLStock) {{{ 

#' @examples
#' data(ple4)
#' # SET srr
#' sr <- predictModel(model=bevholt, params=FLPar(a=140.4e4, b=1.448e5))
#' # RUN with cfwd
#' crun <- cfwd(ple4, catch=catch(ple4)[,-1], sr=sr)
#' # RUN with fwd
#' frun <- fwd(ple4,
#'   control=as(FLQuants(catch=catch(ple4)[,-1]), 'fwdControl'), sr=sr)
#' plot(FLStocks(cfwd=crun, fwd=frun))

setMethod("cfwd", signature(object="FLStock", fisheries="missing"),
  function(object, sr, catch=control, control=catch, deviances="missing") {
    
    # DIMS
    dm <- dim(object)
    na <- dm[1]
    dmnsr <- dimnames(sr)

    # EXTRACT slots
    naa <- stock.n(object)
    maa <- m(object)
    hr <- fbar(object)
    sel <- catch.sel(object)

    # COERCE sr
    # FLQuant -> predictModel
    if(is(sr, "FLQuant")) {
    sr <- predictModel(model=rec~a, params=
      FLPar(c(sr), dimnames=list(params="a", year=dmnsr$year,
        iter=dmnsr$iter)))
    # FLSR -> predictModel
    } else if(is(sr, "FLSR")) {
      sr <- as(sr, "predictModel")
    }

    # DEVIANCES
    if(missing(deviances)) {
      deviances <- rec(object) %=% 1
    }

    # HANDLE fwdControl
    if(is(catch, "fwdControl")) {
      # CHECK single target per year & no max/min
      if(length(catch$year) != length(unique(catch$year)))
        stop("cfwd() can only project for yearly targets, try calling fwd().")

      # CHECK no max/min
      if(any(is.na(iters(catch)[, "value",])))
        stop("cfwd() can only handle targets and not min/max limits, try calling fwd().")
      
      # CHECK target is catch
      if(!all(catch$quant %in% c("f", "catch")))
        stop("cfwd() can only project for catch targets, try calling fwd().")

      # COERCE to FLQuant
      catch <- faa[1, ac(catch$year)] %=% catch$value
    }
    
    # SET years as position
    yrs <- match(dimnames(catch)$year, dimnames(object)$year)
    y0 <- yrs[1] - 1
    naa[,yrs] <- NA

    # COMPUTE harvest rate in year 0
    hr[, y0] <- (catch[,1] / vb(object)[, y0])
    
    # COMPUTE SRP multiplier
    waa <- stock.wt(object)
    cwaa <- catch.wt(object)
    mat <- mat(object)
    msp <- m.spwn(object)
    hsp <- harvest.spwn(object)
    srp <- exp(- maa * msp) * waa * mat

    # LOOP over years (y is prev year)
    
    for (y in yrs - 1) {

      # rec  = srr(ssb) * deviances
      naa[1, y + 1] <- eval(sr@model[[3]],   
        c(as(sr@params, 'list'), list(
        # ssb = n * srp * (1 - hr * h.spwn)
        ssb=c(colSums(naa[, y] * srp[, y] *
        (1 - hr[, y] %*% hsp[, y] %*% sel[, y])))))) * c(deviances[, y + 1])

      # n_y+1 = n_y * exp(-m_y) * (1 - hr_y)
      naa[-1, y + 1] <- naa[-na, y] * exp(-maa[-na, y]) *
        (1 - hr[-na, y] %*% sel[-na, y])

      # PG
      naa[na, y + 1] <- naa[na, y + 1] +
        naa[na, y] * exp(- maa[na, y]) * (1 - hr[, y] %*% sel[na, y])

      # CALCULATE hr
      hr[, y + 1] <- catch[, y] / quantSums(naa[, y + 1] *
        waa[, y + 1] * sel[, y + 1])
      hr[, y + 1][hr[, y + 1] >=1] <- 0.999
    }

  # UPDATE stock.n & harvest

  stock.n(object) <- naa

  # TODO: RETURN F or HR
  if(units(harvest(object)) == "hr") {
    harvest(object) <- hr %*% sel
    units(harvest(object)) <- "hr"
  } else if(units(harvest(object)) == "f") {
    harvest(object) <- - log(1 - hr %*% sel)
    units(harvest(object)) <- "f"
  }
  
  # UPDATE stock, ...
  stock(object) <- computeStock(object)

  # catch.n, uses 0.5 * m
  catch.n(object)[, yrs] <- (naa * (hr %*% sel) / (0.5 * maa + (hr %*% sel)) * 
    (1 - exp(-(hr %*% sel) - 0.5 * maa)))[,yrs]

  # caa = naa * sel * hr * wt
  # catch.n(object)[, yrs] <- (naa * catch.sel(object) %*% hr)[, yrs]

  # landings.n from catch.n and ratio
  landings.n(object) <- catch.n(object) * (landings.n(object) / 
    (discards.n(object) + landings.n(object)))

  # discards
  discards.n(object) <- catch.n(object) - landings.n(object)

  # landings.n & discards.n to 0 if NA
  landings.n(object)[is.na(landings.n(object))] <- 0
  discards.n(object)[is.na(discards.n(object))] <- 0
  
  # catch.wt
  catch.wt(object) <- (landings.wt(object) * landings.n(object) + 
    discards.wt(object) * discards.n(object)) / catch.n(object)

  # catch
  catch(object) <- quantSums(catch.n(object) * catch.wt(object))

  return(object)
  }
)
# }}}

