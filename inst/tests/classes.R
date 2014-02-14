# classes.R - DESC
# classes.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# Class fwdElement {{{

context("test.fwdElement.class")

# new() returns the right prototype
test_that("new() returns the right prototype", {
	
	expect_is(new('fwdElement'), 'fwdElement')

})


# }}}

# Class fwdControl {{{
context("test.fwdControl.class")

# }}}

# Constructor fwdControl() {{{
context("test.fwdControl.constructor")

# fwdControl(target=data.frame, iters=array) full objects
test_that("fwdControl(target=data.frame, iters=array) full objects", {

	target <- data.frame(year=2000:2010, value=rlnorm(11), quantity='f')
	iters <- array(rlnorm(110), dim=c(11, 1, 100),
		dimnames=list(row=1:11, val='value', iter=1:100))

	fwc <- fwdControl(target=target, iters=iters)
	
	# fwdControl is of class fwdControl
	expect_is(fwc, 'fwdControl')
	
	# fwdControl() is a validObject
	expect_true(validObject(fwc))
	
	# result has right elements
	expect_equal(fwc@target@element[,'value'], target[,'value'])
	expect_equal(fwc@target@element[,'year'], target[,'year'])
	expect_equal(fwc@target@element[,'quantity'], target[,'quantity'])
	# iters
	expect_equal(fwc@target@iters[,'value',], iters[,'value',])

})

# fwdControl(target=data.frame, iters=array) short iters
test_that("fwdControl(target=data.frame, iters=array) short iters", {

	target <- data.frame(year=2000:2010, value=rlnorm(11), quantity='f')
	# iters only for rows 1, 4, 6 & 7
	iters <- array(rlnorm(110), dim=c(4, 1, 100), dimnames=list(row=c(1, 4, 6, 7), 
		val='value', iter=1:100))

	fwc <- fwdControl(target=target, iters=iters)
	
	# fwdControl is of class fwdControl
	expect_is(fwc, 'fwdControl')
	
	# fwdControl() is a validObject
	expect_true(validObject(fwc))
	
	# result has right elements
	expect_equal(fwc@target@element[,'value'], target[,'value'])
	expect_equal(fwc@target@element[,'year'], target[,'year'])
	expect_equal(fwc@target@element[,'quantity'], target[,'quantity'])
	# iters 1, 4, 6 & 7
	expect_equal(fwc@target@iters[c(1,4,6,7),'value',], iters[,'value',])

})


# matrix missing 'val'
fwdControl(target=target, iters=iters[c(2,3,4),,])

# }}}
