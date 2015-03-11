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
        FLQuant_base<T> n(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuant wt() const;
        FLQuant wt(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuant m() const;
        FLQuant m(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuant spwn() const;
        FLQuant spwn(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuant fec() const;
        FLQuant fec(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;

        // SSB calculations not implemented here - need harvest.spwn information
        //FLQuant_base<T> ssb() const;
        //std::vector<T> ssb(const int timestep) const;
        //T ssb(const int timestep, const int iter) const;

        // Accessor methods (get and set) for the slots
        FLQuant_base<T>& n();
        FLQuant& wt();
        FLQuant& m();
        FLQuant& spwn();
        FLQuant& fec();

        // Summary methods
        FLQuant_base<T> biomass() const;
        FLQuant_base<T> biomass(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const; // subsetting

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

/*----------------------------------------------*/
// FLBiols class - a vector of FLBiols objects

template <typename T>
class fwdBiols_base {
    public:
        /* Constructors */
		fwdBiols_base();
		fwdBiols_base(SEXP flbs_list_sexp); // Used as intrusive 'as', takes a list of fwdBiol objects - remove. Better to make each fwdBiol separately and add to list
        operator SEXP() const; // Used as intrusive 'wrap' - returns an FLBiols
		fwdBiols_base(fwdBiol_base<T> flb); // Constructor from an fwdBiol object
		fwdBiols_base(const fwdBiols_base& fwdBiols_base_source); // copy constructor to ensure that copy is a deep copy 
		fwdBiols_base& operator = (const fwdBiols_base& fwdBiols_base_source); // Assignment operator for a deep copy

        // Accessors
		fwdBiol_base<T> operator () (const unsigned int element = 1) const; // Only gets an fwdBiol so const reinforced. Default is the first element
		fwdBiol_base<T>& operator () (const unsigned int element = 1); // Gets and sets an fwdBiol so const not reinforced

        void operator() (const fwdBiol_base<T> flb); // Add another fwdBiol_base<T> to the data
        unsigned int get_nbiols() const;

    protected:
        std::vector<fwdBiol_base<T> > biols;
        Rcpp::CharacterVector names; // of the biols
        // std::string desc;
};

typedef fwdBiols_base<double> fwdBiols;
typedef fwdBiols_base<adouble> fwdBiolsAD;


