# constructors.R - DESC
# constructors.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:


fwdElement(year=1:2, quantity='f', value=list(rnorm(10), rnorm(10)))

fwdElement(year=1:2, quantity='f', value=list(rnorm(10), rnorm(10)))

fwdElement(year=1:2, quantity='f', value=list(rnorm(10)))

fwdElement(year=1:2, quantity='f', value=rnorm(10))

fwdElement(list(year=1, quantity='f', value=rnorm(10)))

fwdElement(list(
	list(year=1, quantity='f', value=rnorm(10)),
	list(year=1, quantity='f', value=rnorm(10))))
