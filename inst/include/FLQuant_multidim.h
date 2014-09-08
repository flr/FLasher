/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */


// Multidimensional FLQuant classes.
// These add a 7th (and in the future 8th) dimension to the already 6th dimensional of the FLQuant class.
// However, they are really just lists of FLQuant objects so the FLQuant objects do not have to be the same size.
// Consequently, the sizes of the FLQuants are stored at the FLQuant level, not the FLQuant7 level.
// Useful for storing harvest rates for multiple stocks etc.
// The extra dimensions are implemented using STL vector containers.

#ifndef _FLQuant_base_
#define _FLQuant_base_

#include "FLQuant_base.h"

#endif

#define _FLQuant_multidim_

template <typename T>
class FLQuant7_base {
	public:
        /* Constructors */
		FLQuant7_base();
		FLQuant7_base(SEXP lst_sexp); // Used as intrusive 'as' - takes a List of FLQuant objects
		FLQuant7_base(FLQuant_base<T> flq); // Constructor from an FLQuant
        operator SEXP() const; // Used as intrusive 'wrap'

		FLQuant7_base(const FLQuant7_base& FLQuant7_base_source); // copy constructor to ensure that copies (i.e. when passing to functions) are deep
		FLQuant7_base& operator = (const FLQuant7_base& FLQuant7_source); // Assignment operator for a deep copy

        /* () accessors */
        // If accessing by single element, returns the FLQuant_base<T>
        // If accessing by multiple elements, returns the T value
		FLQuant_base<T> operator () (const unsigned int element=1) const; // only gets an FLQuant so const reinforced 
		T operator () (const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area, const unsigned int iter, const unsigned int dim7=1) const; // only gets an element so const reinforced 
		FLQuant_base<T>& operator () (const unsigned int element=1); // gets and sets an FLQuant so const not reinforced
		T& operator () (const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area, const unsigned int iter, const unsigned int dim7=1); // gets and sets an element so const not reinforced
        void operator() (const FLQuant_base<T> flq); // Add another FLQuant_base<T> to the data
        unsigned int get_ndim7() const;

    private:

        std::vector<FLQuant_base<T> > data;
};

typedef FLQuant7_base<double> FLQuant7;
typedef FLQuant7_base<adouble> FLQuant7AD;

