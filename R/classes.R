# classes.R - DESC
# FLasher/R/classes.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

qlevels <-  c('f', 'catch', 'ssb', 'landings', 'discards', 'tsb', 'rec')

# fwdElement class {{{
setClass('fwdElement',
	representation(
		element='data.frame',
		iters='array'),
	prototype(
		element=data.frame(year=1, quantity=factor(NA, levels=FLasher:::qlevels),
			min=as.numeric(NA), value=0, max=as.numeric(NA), season='all',
			area='unique', unit='all', relYear=as.numeric(NA), relSeason='NA', 
			relArea='NA', relUnit='NA', stringsAsFactors=FALSE),
		iters=array(NA, dimnames=list(row=1, val=c('min', 'value', 'max'), iter=1),
			dim=c(1,3,1))),
	# VALIDITY
	validity=function(object) {
		# rows in element == rows in iters
		if(nrow(object@element) != dim(object@iters)[1])
			return("Mismatch in number of iters in element and array")
		# if value, no min/max,
		if(all(is.na(object@element[,'value'])) &
			 any(is.na(object@element[,'max']), is.na(object@element[,'min'])))
			return("Only value or min/max")
		# and viceversa
		if(any(is.na(object@element[,'value'])) &
			 all(is.na(object@element[,'max']), is.na(object@element[,'min'])))
			return("Only value or min/max")
		# TODO: classes of data.frame columns
		# levels in 'quantity'
		if(!all(as.character(object@element$quantity) %in% FLasher:::qlevels))
			return("Specified 'quantity' not available in fwd")
	}
) # }}}

# fwdControl class {{{

setClass('fwdControl', representation(
	target='fwdElement'),
	prototype(target=new('fwdElement')))
# }}}
