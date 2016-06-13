# generics.R - DESC
# generics.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

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
