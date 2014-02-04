# fwdControl.R - DESC
# fwdControl.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# fwdControl() {{{
setGeneric('fwdControl', function(target, ...) standardGeneric("fwdControl"))

setMethod('fwdControl', signature(target='fwdElement'),
	function(target, ...) {
		return(new('fwdControl', target=target))
	}
)

setMethod('fwdControl', signature(target='data.frame'),
	function(target, ...) {
	
		# COMPLETE df
		# repeat empty as needed
		ele <- new('fwdElement')@element[rep(1, nrow(target)),]
		# HACK: drop rownames
		rownames(ele) <- NULL
		# assign
		ele[,names(target)] <- target

		# extract iters
		args <- list(...)
		if("iters" %in% names(args))
			iters <- args[['iters']]
		# or build and fill
		else {
			iters <- array(NA, dim=c(nrow(target),3,1),
				dimnames=list(row=seq(nrow(target)), val=c('min', 'value', 'max'), iter=1))
			iters[,'value',] <- target$value
		}

		#
		return(fwdControl(target=new('fwdElement', element=target, iters=iters)))
		
	}
)

setMethod('fwdControl', signature(target='list'))

setMethod('fwdControl', signature(target='missing'))

# }}}

# show {{{
setMethod("show", signature("fwdControl"),
	function(object) {
		#
		cat("An object of class \"fwdControl\"\n", sep="")
		cat("@target:\n", sep="")

		# SHOW always year, min, value, max, quantity
		nms <- names(object@target@element)
		# FIND relevant cols (!unique)
		idx <- apply(object@target@element[,-c(1:5)], 2, function(x) length(unique(x))==1)
		nms <- c(nms[1:5], nms[-c(1:5)][!idx])
		
		# SELECT cols
		df <- object@target@element[, nms]

		# WITH iters
		if(dim(object@target@iters)[3] > 1) {
			
			# SWAP median and mad from iters
			df[,c('min', 'value', 'max')] <- 
				apply(object@target@iters, 1:2, function(x)
					if(all(is.na(x)))
						sprintf("%s", "NA")
					else
						paste0(
				sprintf("%4.3f", median(x, na.rm=TRUE)), '(',
				sprintf("%4.3f", mad(x, na.rm=TRUE)), ')'))

			print(df)
			
			cat("   iters: ", dim(object@target@iters)[3],"\n\n")
		} else {
			print(object@target@element[, nms])
		}
	}
) # }}}
