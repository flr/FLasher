# ffwd.R - DESC
# /ffwd.R

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
#' run <- ffwd(ple4, sr=sr, fbar=FLQuant(0.21, dimnames=list(year=1958:2017)))
#' plot(run)

ffwd <- function(object, sr, fbar=control, control=fbar, deviances="missing") {

    # DIMS
    dm <- dim(object)

    # EXTRACT slots
    sn <- stock.n(object)
    sm <- m(object)
    sf <- harvest(object)
    se <- catch.sel(object)

    # DEVIANCES
    if(missing(deviances)) {
      deviances <- rec(object) %=% 1
    }
    
    # HANDLE fwdControl
    if(is(fbar, "fwdControl")) {
      # TODO CHECK single target per year & no max/min
      # TODO CHECK target is fbar/f
      fbar <- sf[1, fbar$year] %=% fbar$value
    }

    # SET years
    yrs <- match(dimnames(fbar)$year, dimnames(object)$year)
    
    # COMPUTE harvest
    fages <- range(object, c("minfbar", "maxfbar"))
    sf[, yrs] <- (se[, yrs] %/%
      quantMeans(se[seq(fages[1], fages[2]), yrs])) %*% fbar

    # COMPUTE TEP
    sw <- stock.wt(object)
    ma <- mat(object)
    ms <- m.spwn(object)
    fs <- harvest.spwn(object)
    ep <- exp(-(sf * fs) - (sm * ms)) * sw * ma

    # LOOP over years
    for (i in yrs - 1) {
      # rec * deviances
      sn[1, i + 1] <- eval(sr@model[[3]],   
        c(as(sr@params, 'list'), list(ssb=c(colSums(sn[, i] * ep[, i]))))) *
        c(deviances[, i + 1])
      # n
      sn[-1, i + 1] <- sn[-dm[1], i] * exp(-sf[-dm[1], i] - sm[-dm[1], i])
      # pg
      sn[dm[1], i + 1] <- sn[dm[1], i + 1] +
        sn[dm[1], i] * exp(-sf[dm[1], i] - sm[dm[1], i])
    }

  # UPDATE stock.n & harvest

  stock.n(object) <- sn
  harvest(object) <- sf
  
  # UPDATE stock, ...
  stock(object) <- computeStock(object)

  # catch.n
  catch.n(object)[,-1] <- (sn * sf / (sm + sf) * (1 - exp(-sf - sm)))[,-1]

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
