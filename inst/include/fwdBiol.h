/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

// Necessary check to avoid the redefinition of FLQuant_base in the RcppExports.cpp
#ifndef _FLQuant_base_
#define _FLQuant_base_

#include "FLQuant_base.h"

#endif


#ifndef _fwdSR_
#define _fwdSR_

#include "fwdSR.h"

#endif


#define _fwdBiol_
/*
 * fwdBiol class
 * Contains biological information (incuding abundance) by age for making projections
 * It's very similar to the FLBiol class in R but also includes SRR information
 */

/*-------------------------------------------------------------------*/
// Necessary to declare this here so that operatingModel class can have access to fwdSR as a friend
template <typename T>
class operatingModel_base;
/* Making a templated equivalent */
// Only n is templated and can be ADOLC
// The other slots are fixed because they are never dependent
// T is double or adouble
template <typename T>
class fwdBiol_base {
    public:
        // /* Constructors */
		fwdBiol_base();
		fwdBiol_base(SEXP flb_sexp); // Used as intrusive 'as', takes an FLBiol but with no SRR
        operator SEXP() const; // Used as intrusive 'wrap' - returns an FLBiol
        fwdBiol_base(const SEXP flb_sexp, const fwdSR_base<T> srr_in); // Pass in FLBiol and fwdSR
        fwdBiol_base(const SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult = TRUE); // Pass in FLBiol and bits of fwdSR

		fwdBiol_base(const fwdBiol_base& fwdBiol_base_source); // copy constructor to ensure that copy is a deep copy - used when passing FLSs into functions
		fwdBiol_base& operator = (const fwdBiol_base& fwdBiol_base_source); // Assignment operator for a deep copy

        // Get accessors with const reinforced
        FLQuant_base<T> n() const;
        FLQuant wt() const;
        FLQuant m() const;
        FLQuant spwn() const;
        FLQuant fec() const;

        // SSB calculations not implemented here - need harvest.spwn information
        //FLQuant_base<T> ssb() const;
        //std::vector<T> ssb(const int timestep) const;
        //T ssb(const int timestep, const int iter) const;

        // Accessor methods for the slots
        FLQuant_base<T>& n();
        FLQuant& wt();
        FLQuant& m();
        FLQuant& spwn();
        FLQuant& fec();

        fwdSR_base<T> get_srr() const;
        std::string get_name() const;
        std::string get_desc() const;
        Rcpp::NumericVector get_range() const;

        // Added a friend so that operating model can access the SRR
        friend class operatingModel;

    private:
        std::string name;
        std::string desc;
        Rcpp::NumericVector range;
        FLQuant_base<T> n_flq;
        FLQuant wt_flq;
        FLQuant m_flq;
        FLQuant spwn_flq;
        FLQuant fec_flq;
        // Annoying init because you can't delegate constructors until C++11
        void init(const SEXP flb_sexp, const fwdSR_base<T> srr_in);
        fwdSR_base<T> srr;
};


typedef fwdBiol_base<double> fwdBiol;
typedef fwdBiol_base<adouble> fwdBiolAD;

