# generics.R - DESC
# generics.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

setGeneric('fwdElement', function(element, iters, ...) standardGeneric("fwdElement"))

setGeneric('fwdControl', function(target, iters, ...) standardGeneric("fwdControl"))