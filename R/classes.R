# classes.R - DESC
# classes.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# fwdElement class {{{

setClass('fwdElement',
	representation(
		element='data.frame',
		iters='array'),
	prototype(
		element=data.frame(year=1, quantity=NA, min=NA, value=0, max=NA,
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
) # }}}

# fwdControl class {{{

setClass('fwdControl', representation(
	target='fwdElement'),
	prototype(target=new('fwdElement')))
# }}}

