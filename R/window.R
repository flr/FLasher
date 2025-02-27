# window.R - DESC
# /window.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# stf(FLStock) {{{

#' stf for FLStock
#' @rdname stf
#' @examples
#' data(ple4)
#' proj <- stf(ple4, 3)

setMethod('stf', signature(object='FLStock'),
  function(object, nyears=3, wts.nyears=3, fbar.nyears=wts.nyears,
    f.rescale=FALSE, arith.mean=TRUE, na.rm=TRUE,
    end=dims(object)$maxyear + nyears, disc.nyears=wts.nyears) {

    res <- fwdWindow(object, end=end,
      fun=ifelse(arith.mean, 'mean', 'geomean'),
      years=list(wt=wts.nyears, mat=wts.nyears, m=wts.nyears, spwn=wts.nyears,
      discards.ratio=wts.nyears, catch.sel=fbar.nyears))

    # f.rescale
    if(f.rescale) {
      fbar.ages <- ac(seq(range(object, 'minfbar'), range(object, 'maxfbar')))
      fbar.years <- ac(seq(end - fbar.nyears + 1, end))
      years <- ac(seq(dims(object)$maxyear + 1, end))
      # mean f over fbar ages and years
      fbar <- mean(apply(slot(res, 'harvest')[fbar.ages, fbar.years], c(2:6), mean,
        na.rm=na.rm))
      # fbar for last REAL year
      lastfbar <- apply(slot(res, 'harvest')[fbar.ages, ac(dims(object)$maxyear)],
        3:6, mean, na.rm=na.rm)

      # divide by fbar and multiply by lastfbar
      slot(res, 'harvest')[, years] <- sweep(slot(res, 'harvest')[, years], 3:6, fbar, '/')
      slot(res, 'harvest')[, years] <- sweep(slot(res, 'harvest')[, years], 3:6, lastfbar, '*')
    }
    return(res)
  }
) # }}}

# stf(FLBiol) {{{

#' @rdname stf

setMethod('stf', signature(object='FLBiol'),
  function(object, nyears=3, wts.nyears=3, arith.mean=TRUE, na.rm=TRUE,
    end=dims(object)$maxyear + nyears) {

    dims <- dims(object)
    
    # check nyears and end match
    if(missing(nyears))
      nyears <- as.numeric(end) - dims$maxyear
    else if(dims$maxyear + nyears != end)
      stop("'nyears' and 'end' do not match: ", dims$maxyear + nyears, " vs. ", end)

    # years
    years <- ac((dims$maxyear+1):end)
    wts.years <- ac(seq(dims$maxyear-wts.nyears+1, dims$maxyear))

    # arith or geometric
    if(arith.mean)
      fmean <- mean
    else  
      fmean <- function(x) exp(mean(log(x)))

    # window object
    res <- window(object, end=end)

    # average slots
    # *.wt, mat, m and *.spwn as average over wts.years
    for (i in c('wt', 'm', 'spwn'))
      slot(res, i)[,years] <- apply(slot(res, i)[,wts.years], c(1,3:6), fmean, na.rm=TRUE)
    
    return(res)
  }
) # }}}

# stf(FLStocks) {{{

#' @rdname stf

setMethod("stf", signature(object="FLStocks"),
  function(object, ... ) {
    lapply(object, stf, ...)
  }
)
# }}}
