# generics.R - DESC
# generics.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# fwdControl
setGeneric('fwdControl', function(target, iters, ...) standardGeneric("fwdControl"))

# target, target<-
setGeneric('target', function(object, ...) standardGeneric('target'))
setGeneric('target<-', function(object, ..., value) standardGeneric('target<-'))

