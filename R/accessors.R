# accessors.R - DESC
# FLasher/R/accessors.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# target, iters {{{

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
) # }}}

# FCB {{{

#' @rdname FCB
#' @examples
#' control <- fwdControl()
#' # Access FCB slot
#' FCB(control)

setMethod("FCB", signature(object="fwdControl"),
  function(object) {
    return(object@FCB)
  }
)

#' Set the FCB slot of the fwdControl

#' @rdname FCB
#' @examples
#' # Assign to existing fwdControl
#' FCB(control) <- FCB(c(f=1, c=1, b=2), c(f=1, c=2, b=2))

setReplaceMethod("FCB", signature(object="fwdControl", value="matrix"),
	function(object, value) {
		object@FCB <- value
		return(object)
	}
) # }}}
