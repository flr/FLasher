/* 
 * Copyright 2013 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

// Necessary check to avoid the redefinition of FLQuant_base in the RcppExports.cpp
#ifndef _FLQuant_base_
#define _FLQuant_base_

#include "FLQuant_base.h"

#endif

/*
 * FLStock class
 * Only uses FLQuant, not FLQuantAD
 * Needs to be templated like fwdBiol etc
 */

class FLStock {
    public:
        /* Constructors */
		FLStock();
		FLStock(SEXP fls_sexp); // Used as intrusive 'as'
        operator SEXP() const; // Used as intrusive 'wrap'
		FLStock(const FLStock& FLStock_source); // copy constructor to ensure that copy is a deep copy - used when passing FLSs into functions
		FLStock& operator = (const FLStock& FLStock_source); // Assignment operator for a deep copy


        /* These data members are public but the actual data in the FLQuant members is not.
         * It can only be accessed by the () operators */
        // The FLQuant slots
        FLQuant catches;  // catch is a reserved word
        FLQuant catch_n;  
        FLQuant catch_wt; 
        FLQuant discards;
        FLQuant discards_n;
        FLQuant discards_wt;
        FLQuant landings;
        FLQuant landings_n;
        FLQuant landings_wt;
        FLQuant stock;
        FLQuant stock_n;
        FLQuant stock_wt;
        FLQuant m;
        FLQuant mat;
        FLQuant harvest;
        FLQuant harvest_spwn;
        FLQuant m_spwn;

    private:
        std::string name;
        std::string desc;
        Rcpp::NumericVector range;

};

