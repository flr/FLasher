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
		fwdSR_base(const std::string model_name_ip, const FLQuant params_ip, const FLQuant deviances_ip, const bool deviances_mult_ip);  // Construct using model name
        operator SEXP() const; // Used as intrusive 'wrap' - returns a list
		fwdSR_base(const fwdSR_base& fwdSR_base_source); // copy constructor to ensure that copy is a deep copy - used when passing into functions
		fwdSR_base& operator = (const fwdSR_base& fwdSR_base_source); // Assignment operator for a deep copy

        // Evaluate the model only 1 value at a time
        T eval_model(const T srp, int year, int unit, int season, int area, int iter) const;
        T eval_model(const T srp, const std::vector<unsigned int> params_indices) const;

        // Predict recruitment. As eval() but also applies the deviances
        FLQuant_base<T> predict_recruitment(const FLQuant_base<T> srp, const std::vector<unsigned int> initial_params_indices);
        
        // Typedef for the SRR model functions
        typedef T (*srr_model_ptr)(const T, const std::vector<double>);
        typedef std::map<std::string, srr_model_ptr> model_map_type;
        void init_model_map();

        // Accessors and setters
        FLQuant_base<double> get_params() const;
        std::string get_model_name() const;
        std::vector<double> get_params(unsigned int year, unsigned int unit, unsigned int season, unsigned int area, unsigned int iter) const;
        int get_nparams() const; // No of params in a time step - the length of the first dimension
        FLQuant_base<double> get_deviances() const;
        bool get_deviances_mult() const;
        void set_deviances(const FLQuant_base<double> new_deviances);
        void set_deviances_mult(const bool new_deviances_mult);

        bool does_recruitment_happen(unsigned int unit, unsigned int year, unsigned int season) const;
        bool has_recruitment_happened(unsigned int unit, unsigned int year, unsigned int season) const;

    private:
        T (*model) (const T, const std::vector<double>); // Pointer to SRR function
        std::string model_name;
        FLQuant_base<double> params;
        FLQuant_base<double> deviances;
        bool deviances_mult;
        model_map_type map_model_name_to_function; // Map for the SRR models
};

typedef fwdSR_base<double> fwdSR;
typedef fwdSR_base<adouble> fwdSRAD;

//------------------------------------------------------------------
// SRR functions

template <typename T>
T ricker(const T srp, const std::vector<double> params);

template <typename T>
T bevholt(const T srp, const std::vector<double> params);

template <typename T>
T constant(const T srp, const std::vector<double> params);

template <typename T>
T bevholtSS3(const T srp, const std::vector<double> params);

template <typename T>
T cushing(const T srp, const std::vector<double> params);

template <typename T>
T segreg(const T srp, const std::vector<double> params);

template <typename T>
T survsrr(const T srp, const std::vector<double> params);

template <typename T>
T bevholtsig(const T srp, const std::vector<double> params);
