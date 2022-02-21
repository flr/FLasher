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
