# methods.R - DESC
# FLasher/R/methods.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# show {{{
setMethod("show", signature("fwdElement"),
	function(object) {
		
		# cat("An object of class \"fwdElement\"\n", sep="")

		# SHOW always year, min, value, max, quantity
		nms <- names(object@element)
		# FIND relevant cols (!unique)
		idx <- apply(object@element[,-c(1:5)], 2, function(x) length(unique(x))==1)
		nms <- c(nms[1:5], nms[-c(1:5)][!idx])
		
		# SELECT cols
		df <- object@element[, nms]

		# WITH iters
		if(dim(object@iters)[3] > 1) {
			
			# SWAP median and mad from iters
			df[,c('min', 'value', 'max')] <- 
				apply(object@iters, 1:2, function(x)
					if(all(is.na(x)))
						sprintf("%s", "NA")
					else if(length(unique(x)) == 1)
						sprintf("%4.3f", x[1])
					else
						paste0(
							sprintf("%4.3f", median(x, na.rm=TRUE)), '(',
							sprintf("%4.3f", mad(x, na.rm=TRUE)), ')')
				)
			print(df)
			
			cat("   iters: ", dim(object@iters)[3],"\n\n")
		} else {
			print(object@element[, nms])
		}
	}
)

setMethod("show", signature("fwdControl"),
	function(object) {
		
		cat("An object of class \"fwdControl\"\n\n", sep="")

		show(object@target)
	}
) # }}}

# [ {{{
setMethod("[", signature(x="fwdElement"),
  function(x, i, j) {

		# 'i' applies to rows in both element ands iters
		if(!missing(i)) {
			x@element <- x@element[i,,drop=FALSE]
			x@iters <- x@iters[i,,,drop=FALSE]
		}
		# 'j' applies only to 3rd dimension in iters
		if(!missing(j)) {
			x@iters <- x@iters[,,j,drop=FALSE]
		}

		return(x)
	}
)

setMethod("[", signature(x="fwdControl"),
  function(x, i, j) {

		args <- list()

		# 'i' selected?
		if(!missing(i))
			args[[1]] <- i

		# 'j' selected?
		if(!missing(j))
			args[[2]] <- j

		# apply to each slot
		x@target <- do.call('[', c(list(x@target), args))

		return(x)
	}
) # }}}

# [<- {{{ TODO
setMethod("[<-", signature(x="fwdElement", value="ANY"),
  function(x, i, j, k, value) {

		arge <- lapply(as.list(dim(x@element)), seq)
		argi <- lapply(as.list(dim(x@iters)), seq)

		if(!missing(i)) {
			arge[[1]] <- i
			argi[[1]] <- i
		}
		if(!missing(j)) {
			arge[[2]] <- j
			if(any(j %in% c('min', 'value', 'max') | j %in% c(3:5))) {
				argi[[2]] <- j[j %in% c('min', 'value', 'max') | j %in% c(3:5)]
			}
		}
		if(!missing(k)) {
			argi[[3]] <- k
		}

		# element
		ele <- do.call('[<-', c(list(x@element), arge, list(value=value)))

		# iters, if min, value or max changed
		if(any(j %in% c('min', 'value', 'max') | j %in% c(3:5))) {
			ite <- do.call('[<-', c(list(x@iters), argi, list(value=value)))
			x@iters <- ite
			# TODO update element
		}

		x@element <- ele

		return(x)
	}
)

setMethod("[<-", signature(x="fwdControl", value="ANY"),
  function(x, i, j, value) {

		args <- list()

		# 'i' selected?
		if(!missing(i))
			args[[1]] <- i

		# 'j' selected?
		if(!missing(j))
			args[[2]] <- j

		# apply to each slot
		x@target <- do.call('[', c(list(x@target), args))

		return(x)
	}
) # }}}
