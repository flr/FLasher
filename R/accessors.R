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
setReplaceMethod("target", signature(object="fwdControl", value="data.frame"),
	function(object, value) {
		
		return(object)
	}
)
