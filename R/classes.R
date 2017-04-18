# classes.R - DESC
# FLasher/R/classes.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# .qlevels - available quants for fwdControl
.biol_quants <- c('srp', 'ssb_end', 'biomass_end', 'ssb_spawn', 'biomass_spawn', 'ssb_flash', 'biomass_flash') 
.qlevels <-  c('catch', 'landings', 'discards', 'f', 'fbar', 'revenue', 'effort', .biol_quants)

# .fcb, .vfcb - Possible fishery-catch-biol combinations in @target {{{
.fcb <- list(
  list(quant=c("effort", "revenue"), fishery=TRUE, catch=FALSE, biol=FALSE),
  list(quant=c("revenue"), fishery=TRUE,catch=TRUE, biol=FALSE),
  list(quant=c("fbar", "f"), fishery=c(TRUE, FALSE), catch=c(TRUE, FALSE), biol=c(TRUE,TRUE)),
  list(quant=c("catch", "landings", "discards"),
    fishery=c(TRUE, FALSE),catch=c(TRUE, FALSE), biol=c(FALSE, TRUE)),
  list(quant=.biol_quants, catch=FALSE, fishery=FALSE, biol=TRUE))

.foo <- function(x) {
  fcb <- as.data.frame(x[2:4])
  quant <- rep(x[[1]], each=nrow(fcb))
  return(cbind(data.frame(quant=quant), fcb[rep(seq(nrow(fcb)), length(x[[1]])),]))
}

.vfcb <- do.call(rbind, c(lapply(.fcb, .foo), list(make.row.names = FALSE)))
# }}}

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
#'     \item{iters}{The values and limits for each target quantity and time step (\code{array}).}
#'     \item{FCB}{The matrix describing which FLCatch of which FLFishery catches which FLBiol. A \code{matrix} with 3 columns: F, C, and B.}
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

setClass("fwdControl",

  # REPRESENTATION
  slots=c(
    target="data.frame",
    iters="array",
    FCB="array"),

  # PROTOTYPE
  # year quant season area unit relYear relSeason relFishery relCatch relBiol minAge maxAge fishery catch biol
  prototype=list(
    target=data.frame(year=1, quant=factor(NA, levels=.qlevels),
      season="all", area="unique", unit="unique",
      relYear=as.integer(NA), relSeason=as.integer(NA),
      relFishery=as.integer(NA), relCatch=as.integer(NA), relBiol=as.integer(NA),
      relMinAge=as.integer(NA), relMaxAge=as.integer(NA),
      minAge=as.integer(NA), maxAge=as.integer(NA),
      fishery=as.integer(NA), catch=as.integer(NA), biol=as.integer(NA),
      stringsAsFactors=FALSE),
    iters=array(NA, dimnames=list(row=1, val=c("min", "value", "max"), iter=1),
      dim=c(1,3,1)),
    FCB=array(c(NA), dim=c(1,3), dimnames=list(1, c("F", "C", "B")))),

  # VALIDITY
  validity=function(object) {
    # rows in target == rows in iters
    if(nrow(object@target) != dim(object@iters)[1])
      return("Mismatch in number of rows in target and array")
    
    # value & min/max
    idx <- !is.na(object@iters)
    ids <- idx[,1,] + idx[,3,] + (10 * idx[,2,])
    ids <- idx[,1,,drop=FALSE] + idx[,3,,drop=FALSE] + (10 * idx[,2,,drop=FALSE])
    rsu <- rowSums(ids / dim(idx)[3])

    if(any(rsu == 11 | rsu == 22))
      return("Only value OR min/max allowed by row")

    # TODO: classes of data.frame columns
    # TODO: colnames in target

    # FCB
    if(!all.equal(dimnames(object@FCB)[[2]], c("F", "C", "B")))
      return("colnames of FCB slot are incorrect, must be 'F', 'C', 'B'")
    if(length(dim(object@FCB)) != 2)
      return("@FCB array must have 2 dimensions")

    # levels in "quant"
    if(!all(as.character(object@target$quant) %in% .qlevels))
      return("Specified 'quant' currently not available as target in fwd")
  }
) # }}}
