# utilities.R - DESC
# /utilities.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# calc_F {{{
# F = alpha * Biomass ^ -beta * sel * effort
calc_F <- function(catch, biol, effort){
    biomass <- quantSums(biol@n * biol@wt)
    F <- (catch@catch.q['alpha',] * biomass ^ (-catch@catch.q['beta',]) * effort) %*% catch@catch.sel
    return(F)
} # }}}

# fillchar {{{
setMethod("fillchar", signature("FLFisheries"),
  function(object) {

    if(length(object@desc) == 0)
      object@desc <- character(1)

    lapply(object, function(x) {
      if(length(name(x)) == 0)
        name(x) <- character(1)
      if(length(desc(x)) == 0)
        desc(x) <- character(1)
      lapply(x, function(y) {
        if(length(name(y)) == 0)
          name(y) <- character(1)
        if(length(desc(y)) == 0)
          desc(y) <- character(1)
        return(y)
      })
      return(x)
    })
  }
)


setMethod("fillchar", signature("FLBiols"),
  function(object) {

    if(length(object@desc) == 0)
      object@desc <- character(1)

    lapply(object, function(x) {
      if(length(name(x)) == 0)
        name(x) <- character(1)
      if(length(desc(x)) == 0)
        desc(x) <- character(1)
      return(x)
    })
  }
) # }}}
