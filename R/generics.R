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

# FCB

#' Create and extract the FCB slot in fwdControl
#'
#' The FCB slot in fwdControl specifies the relationships between the catches
#' ('C') taken by each fishery ('F') from different biological units ('B').
#'
#' This slot is of class *matrix* and has thre columns, named 'F', 'C' and 'B',
#' and as many rows as relationships between the FLBiol(s) and FLFishery(ies)
#' objects the fwdControl refers to.
#'
#' @name FCB
#' @param object Input object to construct or extract from.
#' @param ... Extract input arguments
#' @param value Input matrix

setGeneric("FCB<-", function(object, ..., value) standardGeneric("FCB<-"))

#' Calculation of fisheries partial fishing mortalities
#'
#' Fishing mortalities at age for one of both stock (`FLBiol`) are partitioned
#' along the fisheries (`FLFisheries`) exploiting them.
#' @param object The exploited population or populations, `FLBiol` or `FLBiols`.
#' @param fisheries The fisheries exploiting the resource, `FLFisheries`.
#' @param ... Any extra argument.
#' @rdname partialF

setGeneric("partialF", function(object, fisheries, ...) standardGeneric("partialF"))
