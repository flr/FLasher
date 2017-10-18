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

# add_target_order {{{
#' Add the order column to the control target
#'
#' Add the order column to the control target data.frame so that targets are processed in the correct order.
#'
#' It is important that the targets in the control object are processed in the correct order.
#' Targets can happen simultaneously. For example, if there are multiple FLFishery objects in
#' operating model each will need to have a target to solve for at the same time as the others.
#' The targets are processed in a time ordered sequence (year / season).
#' However, within the same year and season it is necessary for the min and max targets to be processed
#' separatley and after the other targets.
#'
#' @param control A fwdControl object
#' @return A fwdControl object with an order column.
#' @export
add_target_order <- function(control){
    # Add temporary original order column - order gets messed about with merge
    control@target$orig_order <- 1:nrow(control@target)
    # Add temporary minmax column
    control@target$minmax <- is.na(control@iters[,"value",1])
    sim_targets <- unique(control@target[,c("year","season","minmax")])
    # Order by year / season / minmax
    sim_targets <- sim_targets[order(sim_targets$year, sim_targets$season, sim_targets$minmax),]
    sim_targets$order <- 1:nrow(sim_targets)
    # Problem - merge reorders by order column
    control@target <- merge(control@target, sim_targets) # order should be the same
    # Reorder by original order so that target and iters slots are consistent
    control@target <- control@target[order(control@target$orig_order),]
    # Reorder target and iters slots by new order
    new_order <- order(control@target$order, control@target$fishery, control@target$catch, control@target$biol)
    control@target <- control@target[new_order,]
    control@iters <- control@iters[new_order,,,drop=FALSE]
    # Remove minmax and orig_order columns
    control@target <- control@target[,colnames(control@target) != "minmax"]
    control@target <- control@target[,colnames(control@target) != "orig_order"]
    return(control)
} # }}}
