# accessors.R - DESC
# FLasher/R/accessors.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# target
#' Access target slot of fwdControl
#' @rdname target
setMethod("target", signature(object="fwdControl"),
	function(object) {
		return(object@target)
	}
)

# target<-
#' Set target slot of fwdControl
#' @rdname target
setReplaceMethod("target", signature(object="fwdControl", value="data.frame"),
	function(object, value) {
    object@target <- value
		return(object)
	}
)

# iters
#' Access the iters slot of the fwdControl
#' @rdname iters
setMethod("iters", signature(object="fwdControl"),
	function(object) {
		return(object@iters)
	}
)

# iters<-
#' Set the iters slot of the fwdControl
#' @rdname iters
setReplaceMethod("iters", signature(object="fwdControl", value="array"),
	function(object, value) {
		object@iters <- value
		return(object)
	}
)

# FCB
setMethod("FCB", signature(object="fwdControl"),
  function(object) {
    return(object@FCB)
  }
)


