# fwdControl.R - DESC
# fwdControl.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# fwdElement class

setClass('fwdElement',
	representation(
		element='data.frame',
		iters='array'),
	prototype(
		element=data.frame(year=1, min=NA, value=0, max=NA, quantity=NA,
			season='all', area='unique', unit='all',
			relYear=NA, relSeason=NA, relArea=NA, relUnit=NA),
		iters=array(NA, dimnames=list(row=1, val=c('min', 'value', 'max'), iter=1),
			dim=c(1,3,1))),
	# VALIDITY
	validity=function(object) {
		# rows in element == rows in iters
		if(nrow(object@element) != dim(object@iters)[1])
			return("")
		# if value, no min/max,
		if(all(is.na(object@element[,'value'])) &
			 any(is.na(object@element[,'max']), is.na(object@element[,'min'])))
			return("Only value or min/max")
		# and viceversa
		if(any(is.na(object@element[,'value'])) &
			 all(is.na(object@element[,'max']), is.na(object@element[,'min'])))
			return("Only value or min/max")
	}
)

# fwdControl class

setClass('fwdControl', representation(
	target='fwdElement'),
	prototype(target=new('fwdElement')))

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

target <- new('fwdElement')@element
target <- rbind(target, target, target)

fwdControl(target=target)

setMethod('fwdControl', signature(target='list'))

setMethod('fwdControl', signature(target='missing'))

