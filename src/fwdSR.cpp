/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../inst/include/fwdSR.h"

/*-------------------------------------------------*/
// Templated class

template <typename T>
void fwdSR_base<T>::init_model_map(){
    //Rprintf("Initialising model map\n");
    // Fill up the map
    map_model_name_to_function["ricker"] = &ricker;
    map_model_name_to_function["Ricker"] = &ricker;
    map_model_name_to_function["bevholt"] = &bevholt;
    map_model_name_to_function["Bevholt"] = &bevholt;
    return;
}

// Default constructor
// Just an empty object
template <typename T>
fwdSR_base<T>::fwdSR_base(){
    //Rprintf("In empty fwdSR constructor\n");
}

// Main constructor method
// Assumes all dims have been checked in R before calling
template <typename T>
fwdSR_base<T>::fwdSR_base(const std::string model_name, const FLQuant params_ip, const int timelag_ip, const FLQuant residuals_ip, const bool residuals_mult_ip) {
    //Rprintf("In main fwdSR constructor\n");
    timelag = timelag_ip;
    params = params_ip;
    residuals = residuals_ip;
    residuals_mult = residuals_mult_ip;
    init_model_map();

    // Set the model pointer
    typename model_map_type::const_iterator model_pair_found = map_model_name_to_function.find(model_name);
    if (model_pair_found != map_model_name_to_function.end()){
        model = model_pair_found->second; // pulls out value
    }
    else
        Rcpp::stop("SRR model not found\n");
} 

/* Intrusive 'wrap' */
// Returns a List of stuff - used for tests
template <typename T>
fwdSR_base<T>::operator SEXP() const{
    //Rprintf("Wrapping fwdSR_base<T>.\n");
    //Rprintf("Doesn't do anything at the moment.\n");
    return Rcpp::List::create(Rcpp::Named("params", params),
                            Rcpp::Named("residuals",residuals),
                            Rcpp::Named("residuals_mult",residuals_mult));
}


// Copy constructor - else members can be pointed at by multiple instances
template <typename T>
fwdSR_base<T>::fwdSR_base(const fwdSR_base<T>& fwdSR_source){
    model = fwdSR_source.model; // Copy the pointer - we want it point to the same place so copying should be fine.
    params = fwdSR_source.params;
    timelag = fwdSR_source.timelag;
    residuals = fwdSR_source.residuals;
    residuals_mult = fwdSR_source.residuals_mult;
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
template <typename T>
fwdSR_base<T>& fwdSR_base<T>::operator = (const fwdSR_base<T>& fwdSR_source){
	if (this != &fwdSR_source){
        model = fwdSR_source.model; // Copy the pointer - we want it point to the same place so copying should be fine.
        timelag = fwdSR_source.timelag;
        params = fwdSR_source.params;
        residuals = fwdSR_source.residuals;
        residuals_mult = fwdSR_source.residuals_mult;
	}
	return *this;
}

// Model evaluation
template <typename T>
T fwdSR_base<T>::eval_model(const T ssb, int year, int unit, int season, int area, int iter) {
    const int nparams = get_nparams();
    std::vector<double> model_params(nparams);
    // Sort out params - if years > no years in the params object (i.e. params are not disaggregated by time etc.) just pick the first one
    // The real checking should be done in the R side
    if (year > params.get_nyear()){
        year = 1;
    }
    if (unit > params.get_nunit()){
        unit = 1;
    }
    if (season > params.get_nseason()){
        season = 1;
    }
    if (area > params.get_narea()){
        area = 1;
    }
    // iters already cared for in generic FLQuant_base<> accessor
    for (int i = 1; i <= nparams; ++i){
        model_params[i-1] = params(i,year,unit,season,area,iter);
    }

    T rec = model(ssb, model_params);
    return rec;
}

// No of params in a time step - the length of the first dimension
template <typename T>
int fwdSR_base<T>::get_nparams() const{
    return params.get_nquant();
}

template <typename T>
int fwdSR_base<T>::get_timelag() const{
    return timelag;
}

template <typename T>
FLQuant_base<double> fwdSR_base<T>::get_residuals() const{
    return residuals;
}

template <typename T>
bool fwdSR_base<T>::get_residuals_mult() const{
    return residuals_mult;
}


// Explicit instantiation of class
template class fwdSR_base<double>;
template class fwdSR_base<adouble>;

//--------------------------------------------------------------------
// SRR functions
// The params in these functions do not have any disaggregation (e.g. by time or area)
// They are only the params required to evaluate the SRR function
// The disaggregated parameter values are stored in the fwdSR_base class as an FLQuant_base object
// The fwdSR.eval_model method sorts out time step of params etc and passes the correct set of params to these functions
template <typename T>
T ricker(const T ssb, const std::vector<double> params){
    T rec;
    // rec = a * ssb * exp (-b * ssb)
    rec = params[0] * ssb * exp(-params[1] * ssb);
    return rec;
}

template <typename T>
T bevholt(const T ssb, const std::vector<double> params){
    T rec;
    // rec = a * ssb / (b + ssb)
    rec = params[0] * ssb / (params[1] + ssb);
    return rec;
}

// Instantiate functions
template double ricker(const double ssb, const std::vector<double> params);
template adouble ricker(const adouble ssb, const std::vector<double> params);
template double bevholt(const double ssb, const std::vector<double> params);
template adouble bevholt(const adouble ssb, const std::vector<double> params);

