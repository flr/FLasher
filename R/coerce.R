# coerce.R - DESC
# /coerce.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# FLQuants -> fwdControl {{{

setAs("FLQuants", "fwdControl",
  function(from) {

    # CHECKS

    # Length must be 1
    if(length(from) > 1)
        stop("Conversion to fwdControl only possible for an single FLQuant")
		
		# CONVERT
    df <- as.data.frame(from)[,c('year', 'iter', 'data', 'qname')]
    names(df)[3:4] <- c('value', 'quant')

    its <- dim(from[[1]])[6]

		# NO ITERS
		if(its == 1) {

      target <- cbind(df[,-2], fishery=as.numeric(NA), catch=as.numeric(NA),
        biol=1)

      return(fwdControl(target))

    # ITERS
		} else {

			target <- cbind(df[df$iter == df$iter[1],][,c('year', 'quant')])

			iters <- array(NA, dim=c(dim(target)[1], 3, its),
        dimnames=list(seq(dim(target)[1]), c("min", "value", "max"), iter=dimnames(from[[1]])$iter))
                   
			iters[,"value",] <- c(from[[1]])
      
      target <- cbind(target, fishery=as.numeric(NA), catch=as.numeric(NA),
        biol=1)
			
			return(fwdControl(target=target, iters=iters))
		}
} ) # }}}

# FLBiol -> FLBiolcpp list {{{
setAs("FLBiol", "list",
  function(from) {

    list(
      biol = as(from, "FLBiolcpp"),
      srr_residuals = FLQuant(),
      srr_residuals_mult = TRUE)
  }) # }}}
