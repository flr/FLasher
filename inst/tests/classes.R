# classes.R - DESC
# classes.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# context test.fwdElement.class {{{

context("test.fwdElement.class")
# }}}

# context test.fwdControl.class {{{
context("test.fwdControl.class")
# }}}


# context test.fwdControl.constructor {{{
context("test.fwdControl.constructor")

# fwdControl(targte=data.frame, iters=array)
target <- data.frame(year=2000:2010, value=rlnorm(11), quantity='f')

# missing 'min' and 'max' columns
iters <- array(rlnorm(110), dim=c(11, 1, 100), dimnames=list(row=1:11, val='value', iter=1:100))

fwdControl(target=target, iters=iters)

# less rows than target
iters <- array(rlnorm(110), dim=c(4, 1, 100), dimnames=list(row=c(1, 4, 6, 7), val='value', iter=1:100))

fwdControl(target=target, iters=iters)

# matrix missing 'val'
fwdControl(target=target, iters=iters[c(2,3,4),,])

# }}}
