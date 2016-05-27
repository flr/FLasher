# classes.R - DESC
# FLasher/R/classes.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

qlevels <-  c('f', 'catch', 'ssb', 'biomass', 'landings', 'discards', 'tsb', 'rec')

# fwdElement class {{{
#' A class for the elements to set up a fishery projection.
#'
#' The desired targets, limits and time steps used in fishery projections can be 
#' specified by creating an object of class \code{fwdElement}.
#' 
#' [...]
#' 
#' @name fwdElement
#' @rdname fwdElement
#' @aliases fwdElement fwdElement-methods fwdElement-class
#' @docType class
#'
#' @section Slots:
#'     \describe{
#'     \item{element}{The table of values, quantities and time steps (\code{data.frame}).}
#'     \item{iters}{Values for multiple iterations (\code{array}).}
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
#'
#' @section Constructor:
#' 
#' A construction method exists for this class that can take named arguments for
#' any of its slots. All slots are then created to match the requirements of the
#' class validity.
#'
#' @author Iago Mosqueira, Finlay Scott - EC JRC.
#' @seealso \link{fwdControl}
#' @keywords classes
#' @examples
#'
#' # CREATE targets on fishing mortality ('f') by year
#'
#' element <- data.frame(year=2000:2010, value=rlnorm(11), quantity='f')
#'
#' fwe <- fwdElement(element=element)
#'
#' # INSPECT fwdControl object
#'
#' show(fwe)

setClass('fwdElement',

  # REPRESENTATION
  representation(
    element='data.frame',
    iters='array'),

  # PROTOTYPE
  prototype(
    element=data.frame(year=1, quantity=factor(NA, levels=FLasher:::qlevels),
      min=as.numeric(NA), value=0, max=as.numeric(NA), season='all',
      area='unique', unit='all', relYear=as.integer(NA), relSeason=as.integer(NA), 
      #relArea='NA', relUnit='NA',
      stringsAsFactors=FALSE),
    iters=array(NA, dimnames=list(row=1, val=c('min', 'value', 'max'), iter=1),
      dim=c(1,3,1))),

  # VALIDITY
  validity=function(object) {

    # rows in element == rows in iters
    if(nrow(object@element) != dim(object@iters)[1])
      return("Mismatch in number of iters in element and array")
    
    # if value, no min/max,
    if(all(is.na(object@element[,'value'])) &
       any(is.na(object@element[,'max']), is.na(object@element[,'min'])))
      return("Only value or min/max")
    
    # and viceversa
    if(any(is.na(object@element[,'value'])) &
       all(is.na(object@element[,'max']), is.na(object@element[,'min'])))
      return("Only value or min/max")
    
    # TODO: classes of data.frame columns

    # levels in 'quantity'
    if(!all(as.character(object@element$quantity) %in% FLasher:::qlevels))
      return("Specified 'quantity' not available in fwd")
  }
) # }}}

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
#'     \item{target}{The table of values, quantities and time steps used as target (\code{fwdElement}).}
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
#' @seealso \link{fwdElement}
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

setClass('fwdControl', representation(
  target='fwdElement'),
  prototype(target=new('fwdElement')))
# }}}
