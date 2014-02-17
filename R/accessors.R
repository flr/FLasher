# accessors.R - DESC
# accessors.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# target
setGeneric('target', function(object, ...) standardGeneric('target'))

setMethod("target", signature(object="fwdControl"),
	function(object) {
		return(object@target)
	}
)

setGeneric('target<-', function(object, ..., value) standardGeneric('target<-'))

setReplaceMethod("target", signature(object="fwdControl", value="fwdElement"),
	function(object, value) {
	}
)

setReplaceMethod("target", signature(object="fwdControl", value="data.frame"),
	function(object, value) {
	}
)
