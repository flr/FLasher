# methods.R - DESC
# methods.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:


# [<- fwdElement {{{
context("test.fwdElement.replacement")

# fwdElement[i,j] <- vector {{{
test_that("fwdElement[i,j] <- vector", {

	element <- data.frame(year=2000:2010, value=rlnorm(11), quantity='f')
	iters <- array(rlnorm(110), dim=c(11, 1, 100),
		dimnames=list(row=1:11, val='value', iter=1:100))

	fwe <- fwdElement(element=element, iters=iters)

	# fwe[1,'value'] <- 999
	fwe[1, 'value'] <- 999
	
	# fwe[1:2,'value'] <- 999
	fwe[1:2, 'value'] <- 999
	
	# Changed element is 999, iters too
	expect_equal(fwe@element[1,'value'], 999)
	
	# NOTE: using sum to avoid mismatch in names
	expect_equal(sum(fwe@iters[1,'value',]), sum(rep(999, 100)))
	
	# fwe[,'value'] <- 999
	fwe[, 'value'] <- 999

	# fwe[,'value'] <- rnorm(1100)
	val <- rnorm(1100)
	fwe[, 'value'] <- val

	# fwe[,'year'] <- 1900:1910
	fwe[, 'year'] <- 1900:1910

	# fwe[,'quantity'] <- 1900:1910
	fwe[, 'quantity'] <- rep('z', 11)

	
}) # }}}

# }}}
