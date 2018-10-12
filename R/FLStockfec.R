# FLStockfec.R - DESC
# /FLStockfec.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# FLStockfec CLASS

FLStockfec <- setClass("FLStockfec",
  representation("FLStock", fec="FLQuant"))

# initialize

setMethod("initialize", "FLStockfec",
  function(.Object, fec=missing, ...) {

    args <- list(...)

    # fec is FLStock
    if(is(fec, "FLStock")) {
      for(i in slotNames(fec))
        slot(.Object, i) <- slot(fec, i)

      .Object@fec <- .Object@mat %=% as.numeric(NA)
      units(.Object@fec) <- "eggs"

      return(.Object)

    # fec is FLQuant, FLStock in ...
    } else if(length(args) > 0 && is(args[[1]], "FLStock")) {
      .Object <- initialize(.Object, fec=args[[1]])
      .Object@fec <- fec

      return(.Object)

    # FLQuant(s) input
    } else {
      # TODO Stop using FLStock()
      fls <- do.call("FLStock", list(object=fec))
      if(!missing(fec))
        .Object <- initialize(.Object, fec=fec, fls)
      else
        .Object <- initialize(.Object, fec=fls)
      return(.Object)
    }
    stop("Could not create FLStockfec object")
  })

# accessors
setMethod("fec", c(object="FLStockfec"),
  function(object) {
    return(object@fec)
  })

setReplaceMethod("fec", c(object="FLStockfec", value="FLQuant"),
  function(object, value) {
    object@fec <- value
    return(object)
  })

setReplaceMethod("fec", c(object="FLStockfec", value="numeric"),
  function(object, value) {
    object@fec[] <- value
    return(object)
  })

# coerce
setAs("FLStock", "FLStockfec",
  function(from) {
    return(FLStockfec(from))
  })

setAs("FLStockfec", "FLBiol",
  function(from) {

    biol <- as(as(from, 'FLStock'), 'FLBiol')
    fec(biol) <- fec(from)

    return(biol)
  })

# stf
setMethod("stf", c(object="FLStockfec"),
  function(object, nyears=3, wts.nyears=3, ...) {

    res <- callNextMethod()

    fec(res)[, seq(dim(fec(from))[2]+1, dim(fec(res))[2])]  <- 
      yearMeans(fec(from)[, seq(dim(fec(from))[2] - wts.nyears, length=wts.nyears)])  

    return(res)
  })

# srp
setGeneric('srp', function(object, ...)
		standardGeneric('srp'))

setMethod("srp", signature(object="FLStockfec"),
	function(object, ...) {

		uns <- units(harvest(object))

		if(uns == 'f') {
			return(quantSums(stock.n(object) * exp(-(harvest(object) * harvest.spwn(object) +
				m(object) * m.spwn(object))) * stock.wt(object) * mat(object) * fec(object)))

		} else if(uns == 'hr') {
			return(quantSums(stock.n(object) * stock.wt(object) * mat(object) * fec(object) *
				(1 - harvest(object) * harvest.spwn(object)) *
				exp(-m(object) * m.spwn(object))))

  	} else {
		stop("Correct units (f or hr) not specified in the harvest slot")
		}
	}
)
