# coerce.R - DESC
# /coerce.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# FLQuants -> fwdControl {{{

setAs("FLQuants", "fwdControl",
  function(from) {
		
		# CHECK length == 1
		if (length(from) != 1)
			stop("Method needs a single FLQuant")

		# NAME
		quant <- names(from)

		# CONVERT
		flq <- from[[1]]
		df <- as.data.frame(flq)[,c('year', 'iter', 'data')]

		# ITERS
		if(dim(flq)[6] == 1) {
			target <- cbind(df[,c('year', 'data')], quant=quant)
			names(target)[grep('data', names(target))] <- 'value'

			return(fwdControl(target))
		} else {

			target <- cbind(df[df$iter == df$iter[1],][,c('year', 'data')], quant=quant)
			names(target)[grep('data', names(target))] <- 'value'

			arrt <- array(NA, dim=c(dim(target)[1], 3, dim(flq)[6]),
				dimnames=list(seq(dim(target)[1]), c('min', 'value', 'max'), iter=dimnames(flq)$iter))
			arrt[,'val',] <- c(flq)
			
			return(fwdControl(target, trgtArray=arrt))
		}
	stop('Conversion unsucessful')
} ) # }}}
