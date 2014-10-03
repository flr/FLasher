# fwd.R - DESC
# fwd.R

# Copyright 2003-2014 FLR Team. Distributed under the GPL 2 or later
# Maintainer: Iago Mosqueira, JRC
# Soundtrack:
# Notes:

# XX {{{
# }}}


# testFwdInputs
testFwdInputs <- function(biol, fish, sr, control) {

	# INPUT: lists from dims(x)

	# CHECK:

	# 1. years
    # years are not character strings. They are integer indices of positions i.e. from 1 : 10, not 2001 : 2010. 

    # seasons must be integers, starting from 1 to N. If 'all' convert to 1L

    # if target quantity is a calculation over an age range, e.g. f or fmax, then age range must be columns in the data frame (minAge, maxAge)

    # We need include CatchNo and FisheryNo columns as some way of referring to which catch of which fishery is associated with the target
    # These will be 1 and 1 for simple projections but allow us to do multiple fleet projections

    # Other columns force to be of correct type, e.g. quantity is character, catch no is integer

	# 2. iters

	return(TRUE)

}
