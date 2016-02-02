# classes.R - DESC
# FLasher/R/classes.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

qlevels <-  c('f', 'catch', 'ssb', 'biomass', 'landings', 'discards', 'tsb', 'rec')

# fwdControl class {{{

#' A class for the targets and limits of a fishery projection.
#'
#' The desired targets, limits and time steps used in fishery projections can be 
#' specified by creating an object of class \code{fwdControl}.
#' 
#' [...]
#' 
#' @name fwdControl
#' @rdname fwdControl
#' @aliases fwdControl fwdControl-methods fwdControl-class
#' @docType class
#'
#' @section Slots:
#'     \describe{
#'     \item{target}{The table of quantities and time steps used as target (\code{data.frame}).}
#'     \item{target}{The values and limits for each target quantity and time step (\code{array}).}
#' }
#'
#' @section Validity: \describe{
#'     \item{VALIDITY}{Neque porro quisquam est qui dolorem ipsum.}
#' }
#'
#' @section Accessors:
#' All slots in the class have accessor and replacement methods defined that
#' allow retrieving and substituting individual slots.
#'
#' The values passed for replacement need to be of the class of that slot.
#' A numeric vector can also be used when replacing FLQuant slots, and the
#' vector will be used to substitute the values in the slot, but not its other
#' attributes.
#'
#' @section Constructor:
#' 
#' A construction method exists for this class that can take named arguments for
#' any of its slots. All slots are then created to match the requirements of the
#' class validity. If an unnamed \code{FLQuant} object is provided, this is used
#' for sizing but not stored in any slot.
#'
#' @author Iago Mosqueira, Finlay Scott - EC JRC.
#' @seealso \link{data.frame}
#' @keywords classes
#' @examples
#'
#' # CREATE targets on fishing mortality ('f') by year
#'
#' target <- data.frame(year=2000:2010, value=rlnorm(11), quantity='f')
#'
#' fwc <- fwdControl(target=target)
#'
#' # INSPECT fwdControl object
#'
#' show(fwc)

setClass('fwdControl',

  # REPRESENTATION
  representation(
    target='data.frame',
    iters='array'),

  # PROTOTYPE
  # year quant season area unit relYear relSeason relFishery relCatch relBiol minAge maxAge fishery catch biol
  prototype(
    target=data.frame(year=1, quant=factor(NA, levels=FLasher:::qlevels),
      season='all', area='unique', unit='all',
      relYear=as.integer(NA), relSeason=as.integer(NA),
      relFishery=as.integer(NA), relCatch=as.integer(NA), relBiol=as.integer(NA),
      minAge=as.integer(NA), maxAge=as.integer(NA),
      fishery='NA', catch='NA', biol='NA',
      stringsAsFactors=FALSE),
    iters=array(NA, dimnames=list(row=1, val=c('min', 'value', 'max'), iter=1),
      dim=c(1,3,1))),

  # VALIDITY
  validity=function(object) {
    # rows in target == rows in iters
    if(nrow(object@target) != dim(object@iters)[1])
      return("Mismatch in number of rows in target and array")
    
    # value & min/max
    idx <- !is.na(object@iters)
    ids <- idx[,1,] + idx[,3,] + (10 * idx[,2,])
    rsu <- rowSums(ids / dim(idx)[3])

    if(any(rsu == 11 | rsu == 22))
      return("Only value OR min/max allowed by row")

    # TODO: classes of data.frame columns

    # levels in 'quant'
    if(!all(as.character(object@target$quant) %in% FLasher:::qlevels))
      return("Specified 'quant' not available in fwd")
  }
) # }}}
