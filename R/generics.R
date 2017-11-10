# generics.R - DESC
# generics.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# fwdControl

#' Constructor for fwdControl objects
#'
#' Constructor for fwdControl objects.
#' Bare bones man pages. Better to look at the vignettes and tutorials.
#' @param target The target. Can be a data.frame, a list or missing.
#' @param iters target The iters. Can be an array, a numeric or missing.
#' @param ... Something
#' @rdname fwdControl
#' @examples
#'
#' # Construct from data.frame and array
#' fcn <- fwdControl(data.frame(year=2000:2005, quant='f', value=0.5))
setGeneric('fwdControl', function(target, iters, ...) standardGeneric("fwdControl"))

# target, target<-

#' Access and replace the target
#'
#' Access and replace the target slot of a fwdControl object
#' @param object The fwdControl object
#' @param ... Other things.
#' @rdname target
setGeneric('target', function(object, ...) standardGeneric('target'))

#' Replace the target
#'
#' Replace the target slot of a fwdControl object
#' @param value The target object to replace the existing one with.
#' @rdname target
setGeneric('target<-', function(object, ..., value) standardGeneric('target<-'))

# iters<-
#' Access and replace the iters slot of the fwdControl
#' @param object The fwdControl.
#' @param value The iters array to replace the existing one with.
#' @param ... Other things.
#' @rdname iters
setGeneric('iters<-', function(object, ..., value) standardGeneric('iters<-'))

# fillchar
#' Fill up character slots
#'
#' Fill the character slots (name etc) with something.
#' @param object The object (FLFisheries or FLBiols)
setGeneric("fillchar", function(object) standardGeneric("fillchar"))

# stf
#' Prepare object for future projection
#'
#' Similar to the old STF method in FLAssess.
#' Extends the object by a number of years and fill in the life history characteristics by taking a mean of the last few years.
#'
#' @param object The object (FLStock or FLBiol)
#' @param nyears Number of years to extend the object
#' @param wts.nyears Number of years to average over to get the future mean weights at age.
#' @param disc.nyears Number of years to average over to get the future mean proportion of discards at age.
#' @param arith.mean If TRUE the arithmetic mean is used. If FALSE the geometric mean is used. Default is TRUE.
#' @param na.rm For the mean function.
#' @param end My beautiful friend
#' @param fbar.nyears Number of years to average the F over (only used if object is an FLStock)
#' @param f.rescale Rescale F (TRUE or FALSE - default is FALSE)
#' @param ... Other things.
setGeneric("stf", function(object,...) standardGeneric("stf"))
