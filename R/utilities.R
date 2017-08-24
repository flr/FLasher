# utilities.R - DESC
# /utilities.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# calc_F {{{
# F = alpha * Biomass ^ -beta * sel * effort
# operating_model.cpp: * F = effort * selectivity * alpha * biomass ^ -beta
#' Calculate fishing mortality
#'
#' Calculate F in the same way as the internal C++ does
#' @param catch The FLCatch
#' @param biol The FLBiol
#' @param effort The fishing effort
calc_F <- function(catch, biol, effort){
    biomass <- quantSums(biol@n * biol@wt)
    F <- catch@catch.q['alpha',] * biomass ^ (-catch@catch.q['beta',]) %*% effort %*% catch@catch.sel
    return(F)
} # }}}

# fillchar {{{
#' fillchar for FLFisheries
#' @rdname fillchar
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

#' fillchar for FLBiols
#' @rdname fillchar
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

# G {{{

#' @rdname fwdControl
#' @examples
#' # Construct a fwdControl with some targets having multiple Biols, specified using the G() function
#' fwdControl(list(year=2000:2001, value=200, quant="catch", biol=G(1,2)),
#'   list(year=2002:2003, value=100, quant="catch", biol=c(1,2)))
G <- function(...)
  return(I(list(unlist(list(...)))))
# }}}
