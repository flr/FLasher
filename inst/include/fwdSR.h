/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

// Necessary check to avoid the redefinition of FLQuant_base in the RcppExports.cpp
#ifndef _FLQuant_base_
#define _FLQuant_base_

#include "FLQuant_base.h"

#endif
#define _fwdSR_

/*
 * fwdSR class
 * Contains data and methods for stock-recruitment relationships
 */

/*-------------------------------------------------------------------*/


template <typename T>
class fwdSR_base {
    public:
        // /* Constructors */
		fwdSR_base();
		//fwdSR_base(SEXP fls_sexp); // Used as intrusive 'as', takes an fwdSR
		fwdSR_base(const std::string model_name, const FLQuant params_ip, const int timelag_ip, const FLQuant residuals_ip, const bool residuals_mult_ip = TRUE); // Main constructor method
        operator SEXP() const; // Used as intrusive 'wrap' - returns a list
		fwdSR_base(const fwdSR_base& fwdSR_base_source); // copy constructor to ensure that copy is a deep copy - used when passing FLSs into functions
		fwdSR_base& operator = (const fwdSR_base& fwdSR_base_source); // Assignment operator for a deep copy

        // Different ways of evaluating the model
        // Timing is when the recruitment happens - not when SSB happens
        // So if SRR params are time disagrregated, the timing refers to the recruitment not the SSB
        T eval_model(const T ssb, int year, int unit, int season, int area, int iter);
        //FLQuant_base<T> eval_model(const FLQuant_base<T> ssb);
        //std::vector<T> eval_model(const std::vector<T> ssb);

        // Typedef for the SRR model functions
        typedef T (*srr_model_ptr)(const T, const std::vector<double>);
        typedef std::map<std::string, srr_model_ptr> model_map_type;
        void init_model_map();

        int get_nparams() const; // No of params in a time step - the length of the first dimension
        int get_timelag() const;
        FLQuant_base<double> get_residuals() const;
        bool get_residuals_mult() const;

    private:
        int timelag; // Number of timesteps (years and seasons) between spawning and recruitment
        T (*model) (const T, const std::vector<double>); // Pointer to SRR function
        FLQuant_base<double> params;
        FLQuant_base<double> residuals;
        bool residuals_mult;
        model_map_type map_model_name_to_function; // Map for the SRR models

};


typedef fwdSR_base<double> fwdSR;
typedef fwdSR_base<adouble> fwdSRAD;

//------------------------------------------------------------------
// SRR functions

template <typename T>
T ricker(const T ssb, const std::vector<double> params);

template <typename T>
T bevholt(const T ssb, const std::vector<double> params);

// std::vec<T> ricker(const std::vec<T> ssb, const FLQuant params);
// FLQuant_base<T> ricker(const FLQuant<T> ssb, const FLQuant params);



