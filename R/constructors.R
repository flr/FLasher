# constructors.R - DESC
# FLasher/R/constructors.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# fwdElement(element='data.frame', iters='array') {{{
setMethod('fwdElement', signature(element='data.frame', iters='array'),
	function(element, iters) {

		# COMPLETE df
		ele <- new('fwdElement')@element[rep(1, nrow(element)),]
		# HACK: drop rownames
		rownames(ele) <- NULL
		# CONVERT year
		if('year' %in% names(element))
			element$year <- as.integer(element$year)
		# assign
		ele[,names(element)] <- element
		# HACK: reassign quantity to keep factors
		ele[,'quantity']  <- factor(element$quantity, levels=FLasher:::qlevels)

		# COMPLETE iters
		dit <- dim(iters)
		dni <- dimnames(iters)
		# val
		if(dit[2] < 3) {
			ite <- array(NA, dim=c(dit[1], 3, dit[3]), dimnames=list(row=dni[[1]],
				val=c('min', 'value', 'max'), iter=1:dit[3]))
			ite[,dni[[2]],] <- iters
			iters <- ite
		}
		# row
		if(dim(iters)[1] < dim(ele)[1]) {
			dmn <- dimnames(iters)
			dmn[['row']] <- seq(1, dim(ele)[1])
			ite <- array(NA, dimnames=dmn, dim=unlist(lapply(dmn, length)))
			
			ite[dni[['row']],,]	<- iters
	
			# FIND missing rows (mro)
			mro <- dmn[['row']][!dmn[['row']] %in% dni[['row']]]
			ite[mro,'value',]	<- rep(ele[mro, 'value'], dit[3])
			ite[mro,'min',] <- rep(ele[mro, 'min'], dit[3])
			ite[mro,'max',] <- rep(ele[mro, 'max'], dit[3])
			iters <- ite
		}
		return(new('fwdElement', element=ele, iters=iters))
	}
) # }}}

# fwdElement(element='data.frame', iters='matrix') {{{
setMethod('fwdElement', signature(element='data.frame', iters='matrix'),
	function(element, iters) {

		dni <- dimnames(iters)

		# NO dimnames in iters, assume dims are 'row' & 'iter' for 'value'
		if(is.null(dni)) {
			dimnames(iters) <- list(row=1:dim(iters)[1], iter=1:dim(iters)[2])
			dni <- dimnames(iters)
		}

		dms <- list(row=1, val=c('min', 'value', 'max'), iter=1)
		dms[names(dni)] <- dni

		ite <- array(NA, dimnames=dms, dim=unlist(lapply(dms, length)))

		# MISSING 'val' dimension, assume val='value'
		if(!"val" %in% names(dni)) {
			ite[,'value',] <- iters
		}
		# MISSING row
		else if (!"row" %in% names(dni)) {
			ite[1,,] <- iters
		}
		# MISSING iter
		if(!"iter" %in% names(dni))
			stop("No 'iter' dimname in iters, cannot create object")

		return(fwdElement(element=element, iters=ite))
	}
) # }}}

# fwdElement(element='data.frame', iters='numeric') {{{
setMethod('fwdElement', signature(element='data.frame', iters='numeric'),
	function(element, iters) {
	
		# CREATE iters
		dti <- dim(element)
		ite <- array(NA, dim=c(dti[1], 3, iters), dimnames=list(row=1:dti[1], 
			val=c('min', 'value', 'max'), iter=seq(iters)))

		# FIND val names in element
		vns <- c('min', 'value', 'max')
		nms <- vns %in% colnames(element)
		ite[, vns[nms],] <- element[,vns[nms]]

		return(fwdElement(element=element, iters=ite))
	}
)
# }}}

# fwdElement(element='data.frame', iters='missing') {{{
setMethod('fwdElement', signature(element='data.frame', iters='missing'),
	function(element) {
		
		# CREATE iters
		dti <- dim(element)
		ite <- array(NA, dim=c(dti[1], 3, 1), dimnames=list(row=1:dti[1], 
			val=c('min', 'value', 'max'), iter=1))

		# FIND val names in element
		vns <- c('min', 'value', 'max')
		nms <- vns %in% colnames(element)
		ite[, vns[nms], 1] <- unlist(c(element[,vns[nms]]))

		return(fwdElement(element=element, iters=ite))
	}
)
# }}}

# fwdControl(target=fwdElement, iters='missing') {{{
setMethod('fwdControl', signature(target='fwdElement', iters='missing'),
	function(target) {
		return(new('fwdControl', target=target))
	}
) # }}}

# fwdControl(target='data.frame', iters='array') {{{
setMethod('fwdControl', signature(target='data.frame', iters='array'),
	function(target, iters) {

		ele <- fwdElement(element=target, iters=iters)

		return(new("fwdControl", target=ele))
	}
) # }}}

# fwdControl(target='data.frame', iters='matrix') {{{
setMethod('fwdControl', signature(target='data.frame', iters='matrix'),
	function(target, iters) {
		
		ele <- fwdElement(element=target, iters=iters)

		return(new("fwdControl", target=ele))
	}
) # }}}

# fwdControl(target='data.frame', iters='numeric') {{{
setMethod('fwdControl', signature(target='data.frame', iters='numeric'),
	function(target, iters) {
		
		ele <- fwdElement(element=target, iters=iters)

		return(new("fwdControl", target=ele))
	}
)
# }}}

# fwdControl(target='data.frame', iters='missing') {{{
setMethod('fwdControl', signature(target='data.frame', iters='missing'),
	function(target) {
		
		ele <- fwdElement(element=target)

		return(new("fwdControl", target=ele))
	}
)
# }}}
