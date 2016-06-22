# generics.R - DESC
# generics.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# fwdControl
setGeneric('fwdControl', function(target, iters, ...) standardGeneric("fwdControl"))

# fwd
setGeneric('fwd', function(biols, fisheries, control, ...) standardGeneric("fwd"))

# target, target<-
setGeneric('target', function(object, ...) standardGeneric('target'))
setGeneric('target<-', function(object, ..., value) standardGeneric('target<-'))

# iters<-
setGeneric('iters<-', function(object, ..., value) standardGeneric('iters<-'))

# fillchar
setGeneric("fillchar", function(object) standardGeneric("fillchar"))

# stf
setGeneric("stf", function(object,...) standardGeneric("stf"))
