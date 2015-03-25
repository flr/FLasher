/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

// Necessary check to avoid the redefinition of FLQuant_base in the RcppExports.cpp

#ifndef _FLCatch_base_
#define _FLCatch_base_

#include "FLCatch.h"

#endif // end of FLC define

#define _FLFishery_



/*
 * FLFishery class
 * Contains catches information (including abundances and selectivity) for making projections and some economics (effort and costs)
 * It's very similar to the FLFishery class in R
 * Notes on implementation:
 * Was going to make an FLFishery class which CONTAINED and FLCatches as a member. However, in R
 * FLFishery does not CONTAIN an FLCatches. An FLFishery IS A FLCatches, with 4 extra members (effort,
 * vcost, fcost, name). This suggests inheritance.
 * Alternatively, FLCatches is probably never used on it's own. So I could just take the current FLCatches
 * implementation, add the extra memebers, change the name to FLFishery and not have an FLCatches class.
 * Options:
 * 1. FLFishery contains an FLCatches
 * 2. FLFishery is a derived class of FLCatches
 * 3. Make an FLFishery the same as current FLCatches, add more members, then delete FLCatches
 *
 * We'll start with Option 2. Then use Option 3 if it's too hard!
 * Seems OK now...
 */

/*-------------------------------------------------------------------*/
template <typename T>
class FLFishery_base : public FLCatches_base<T> {
    public:
        /* Constructors */
		FLFishery_base();
		FLFishery_base(SEXP flf_sexp); // Used as intrusive 'as', takes an FLFishery
        operator SEXP() const; // Used as intrusive 'wrap' - returns an FLFishery
		FLFishery_base(const FLFishery_base& FLFishery_base_source); // copy constructor to ensure that copy is a deep copy - used when passing FLSs into functions
		FLFishery_base& operator = (const FLFishery_base& FLFishery_base_source); // Assignment operator for a deep copy

        // Accessor methods for the slots
        // Get only
        FLQuant_base<T> effort(std::vector<unsigned int> indices_min, std::vector<unsigned int> indices_max) const;
        FLQuant_base<T> effort() const;
        FLQuant vcost() const;
        FLQuant fcost() const;
        // Get and Set
        FLQuant_base<T>& effort();
        FLQuant& vcost();
        FLQuant& fcost();

    private:
        std::string name;
        Rcpp::NumericVector range;
        FLQuant_base<T> effort_flq;
        FLQuant vcost_flq;
        FLQuant fcost_flq;

};

typedef FLFishery_base<double> FLFishery;
typedef FLFishery_base<adouble> FLFisheryAD;


/*-------------------------------------------------------------------*/
// The plural class
template <typename T>
class FLFisheries_base {
    public:
        /* Constructors */
		FLFisheries_base();
		FLFisheries_base(SEXP flf_sexp); // Used as intrusive 'as', takes an FLFisheries
        operator SEXP() const; // Used as intrusive 'wrap' - returns an FLFisheries
		FLFisheries_base(const FLFisheries_base& FLFisheries_base_source); // copy constructor to ensure that copy is a deep copy - used when passing FLSs into functions
		FLFisheries_base& operator = (const FLFisheries_base& FLFisheries_base_source); // Assignment operator for a deep copy

        // Accessors
		FLFishery_base<T> operator () (const unsigned int  fishery) const; // Only gets an FLFishery so const reinforced. 
		FLFishery_base<T>& operator () (const unsigned int fishery); // Gets and sets an FLFishery so const not reinforced. Default is the first element
		FLCatch_base<T> operator () (const unsigned int fishery, const unsigned int catches) const; // Only gets an FLCatch so const reinforced. 
		FLCatch_base<T>& operator () (const unsigned int fishery, const unsigned int catches); // Gets and sets an FLCatch so const not reinforced. 

        unsigned int get_nfisheries() const;

    private:
        std::vector<FLFishery_base<T> > fisheries;
        Rcpp::CharacterVector names; // of the fisheries
        std::string desc;
};

typedef FLFisheries_base<double> FLFisheries;
typedef FLFisheries_base<adouble> FLFisheriesAD;
