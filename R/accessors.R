# accessors.R - DESC
# accessors.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# target
setMethod("target", signature(object="fwdControl"),
	function(object) {
		return(object@target)
	}
)

# target<-
setReplaceMethod("target", signature(object="fwdControl", value="fwdElement"),
	function(object, value) {
		slot(object, 'target') <- value
		return(object)
	}
)

setReplaceMethod("target", signature(object="fwdControl", value="data.frame"),
	function(object, value) {
		
		# CHANGE element
		slot(slot(object, 'target'), 'element')[,names(value)] <- value
		# MODIFY iters
		idx <- names(value) %in% c('min', 'value', 'max')
		# IF
		# DROP iters
		slot(slot(object, 'target'), 'iters')[,idx,] <- slot(slot(object, 'target'), 'iters')[,idx,1, drop=FALSE]
		slot(slot(object, 'target'), 'iters')[,idx,] <- value[,idx]
		return(object)
	}
)
