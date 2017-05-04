# coerce.R - DESC
# /coerce.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# FLQuants -> fwdControl {{{

setAs("FLQuants", "fwdControl",
  function(from) {
		
		# CONVERT
    target <- as.data.frame(from)[,c('year', 'iter', 'data', 'qname')]
    names(target)[3:4] <- c('value', 'quant')

		# ITERS
		if(max(as.numeric(target$iter)) == 1) {

      target <- cbind(target[,-2], fishery=as.numeric(NA), catch=as.numeric(NA),
        biol=1)

      return(fwdControl(target))
		} else {

			target <- cbind(target[target$iter == target$iter[1],][,c('year', 'data')], quant=quant)
			names(target)[grep('data', names(target))] <- 'value'

			arrt <- array(NA, dim=c(dim(target)[1], 3, dim(flq)[6]),
				dimnames=list(seq(dim(target)[1]), c('min', 'value', 'max'), iter=dimnames(flq)$iter))
			arrt[,'val',] <- c(flq)
      
      target <- cbind(target, fishery=as.numeric(NA), catch=as.numeric(NA),
        biol=1)
			
			return(fwdControl(target, trgtArray=arrt))
		}
	stop('Conversion unsucessful')
} ) # }}}

# FLBiol -> FLBiolcpp list {{{
setAs("FLBiol", "list",
  function(from) {

    list(
      biol = as(from, "FLBiolcpp"),
      srr_residuals = FLQuant(),
      srr_residuals_mult = TRUE)
  }) # }}}
