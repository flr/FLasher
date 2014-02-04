# fwdControl.R - DESC
# fwdControl.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# XX {{{
# }}}

target <- new('fwdElement')@element
target <- rbind(target, target, target)

object <- fwdControl(target=target)

object@target@iters <- array(rnorm(90), dim=c(3,3,30), dimnames=list(row=1:3, val=c('min', 'value', 'max'), iter=1:30))
object@target@iters[,'min',] <- NA
object@target@iters[,'max',] <- NA


