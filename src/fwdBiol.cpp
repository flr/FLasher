/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../inst/include/fwdBiol.h"

/*-------------------------------------------------*/
// Templated class

// Default constructor
// Just an empty object
template <typename T>
fwdBiol_base<T>::fwdBiol_base(){
    name = std::string();
    desc = std::string(); 
    range = Rcpp::NumericVector();
    n_flq = FLQuant_base<T>();
    wt_flq = FLQuant();
    m_flq = FLQuant();
    spwn_flq = FLQuant();
    fec_flq = FLQuant();
    mat_flq = FLQuant();
    srr = fwdSR_base<T>();
}

// Constructor from a SEXP S4 FLBiol
// Does not set the SR - only the FLBiol bits
// Used as intrusive 'as'
template <typename T>
fwdBiol_base<T>::fwdBiol_base(const SEXP flb_sexp){
    //Rprintf("In FLBiol constructor\n");
    // Empty fwdSR
    fwdSR_base<T> empty_srr;
    srr = empty_srr;
    // All the FLBiol bits
    Rcpp::S4 fwdb_s4 = Rcpp::as<Rcpp::S4>(flb_sexp);
    name = Rcpp::as<std::string>(fwdb_s4.slot("name"));
    desc = Rcpp::as<std::string>(fwdb_s4.slot("desc"));
    range = fwdb_s4.slot("range");
    n_flq = fwdb_s4.slot("n");
    wt_flq = fwdb_s4.slot("wt");
    m_flq = fwdb_s4.slot("m");
    spwn_flq = fwdb_s4.slot("spwn");
    fec_flq = fwdb_s4.slot("fec");
    mat_flq = fwdb_s4.slot("mat");
}

// Constructor from FLBiol and fwdSR
// Use delegated constructor - excitement!
template <typename T>
fwdBiol_base<T>::fwdBiol_base(const SEXP flb_sexp, const fwdSR_base<T> srr_in) : fwdBiol_base(flb_sexp){
    //Rprintf("In FLBiol and fwdSR constructor\n");
    // Check size of residuals matches that of FLBiol: dims 2-5
    auto residuals = srr_in.get_residuals();
    auto resid_dim = residuals.get_dim();
    auto biol_dim = n().get_dim();
    if ((biol_dim[1] != resid_dim[1]) || (biol_dim[2] != resid_dim[2]) || (biol_dim[3] != resid_dim[3]) || (biol_dim[4] != resid_dim[4])){
        Rcpp::stop("Dimensions 2-5 of SR residuals should equal those of the fwdBiol\n");
    }
    srr = srr_in;
}

// Constructor from FLBiol and fwdSR bits
// Use delegated constructor - excitement!
template <typename T>
fwdBiol_base<T>::fwdBiol_base(const SEXP flb_sexp, const std::string model_name, const FLQuant params, const FLQuant residuals, const bool residuals_mult) : fwdBiol_base(flb_sexp, fwdSR_base<T>(model_name, params, residuals, residuals_mult)){
    //Rprintf("In FLBiol and FLSR bits constructor.\n");
}

// Copy constructor - else members can be pointed at by multiple instances
template <typename T>
fwdBiol_base<T>::fwdBiol_base(const fwdBiol_base<T>& fwdBiol_source){
    name = fwdBiol_source.name;
    desc = fwdBiol_source.desc;
    range = Rcpp::clone<Rcpp::NumericVector>(fwdBiol_source.range);
    n_flq = fwdBiol_source.n_flq;
    wt_flq = fwdBiol_source.wt_flq;
    m_flq = fwdBiol_source.m_flq;
    spwn_flq = fwdBiol_source.spwn_flq;
    fec_flq = fwdBiol_source.fec_flq;
    mat_flq = fwdBiol_source.mat_flq;
    srr = fwdBiol_source.srr;
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
template <typename T>
fwdBiol_base<T>& fwdBiol_base<T>::operator = (const fwdBiol_base<T>& fwdBiol_source){
	if (this != &fwdBiol_source){
        name = fwdBiol_source.name;
        desc = fwdBiol_source.desc;
        range = Rcpp::clone<Rcpp::NumericVector>(fwdBiol_source.range);
        n_flq = fwdBiol_source.n_flq;
        wt_flq = fwdBiol_source.wt_flq;
        m_flq = fwdBiol_source.m_flq;
        spwn_flq = fwdBiol_source.spwn_flq;
        fec_flq = fwdBiol_source.fec_flq;
        mat_flq = fwdBiol_source.mat_flq;
        srr = fwdBiol_source.srr;
	}
	return *this;
}

/* Intrusive 'wrap' */
// Returns an FLBiol and ignores the SRR
template <typename T>
fwdBiol_base<T>::operator SEXP() const{
    //Rprintf("Wrapping fwdBiol_base<T>.\n");
    Rcpp::S4 flb_s4("FLBiol");
    flb_s4.slot("name") = name;
    flb_s4.slot("desc") = desc;
    flb_s4.slot("range") = range;
    flb_s4.slot("n") = n_flq;
    flb_s4.slot("wt") = wt_flq;
    flb_s4.slot("m") = m_flq;
    flb_s4.slot("spwn") = spwn_flq;
    flb_s4.slot("fec") = fec_flq;
    flb_s4.slot("mat") = mat_flq;
    return Rcpp::wrap(flb_s4);
}

// Get const accessors
template <typename T>
FLQuant_base<T> fwdBiol_base<T>::n() const {
    return n_flq;
}

template <typename T>
FLQuant_base<T> fwdBiol_base<T>::n(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return n_flq(indices_min, indices_max);
}

template <typename T>
FLQuant fwdBiol_base<T>::wt() const {
    return wt_flq;
}

template <typename T>
FLQuant fwdBiol_base<T>::wt(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return wt_flq(indices_min, indices_max);
}

template <typename T>
FLQuant fwdBiol_base<T>::m() const {
    return m_flq;
}

template <typename T>
FLQuant fwdBiol_base<T>::m(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return m_flq(indices_min, indices_max);
}

template <typename T>
FLQuant fwdBiol_base<T>::spwn() const {
    return spwn_flq;
}

template <typename T>
FLQuant fwdBiol_base<T>::spwn(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return spwn_flq(indices_min, indices_max);
}

template <typename T>
FLQuant fwdBiol_base<T>::fec() const {
    return fec_flq;
}

template <typename T>
FLQuant fwdBiol_base<T>::fec(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return fec_flq(indices_min, indices_max);
}

template <typename T>
FLQuant fwdBiol_base<T>::mat() const {
    return mat_flq;
}

template <typename T>
FLQuant fwdBiol_base<T>::mat(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return mat_flq(indices_min, indices_max);
}

// Get and set accessors
template <typename T>
FLQuant_base<T>& fwdBiol_base<T>::n() {
    return n_flq;
}

template <typename T>
FLQuant& fwdBiol_base<T>::wt() {
    return wt_flq;
}

template <typename T>
FLQuant& fwdBiol_base<T>::m() {
    return m_flq;
}

template <typename T>
FLQuant& fwdBiol_base<T>::spwn() {
    return spwn_flq;
}

template <typename T>
FLQuant& fwdBiol_base<T>::fec() {
    return fec_flq;
}

template <typename T>
FLQuant& fwdBiol_base<T>::mat() {
    return mat_flq;
}

template <typename T>
fwdSR_base<T> fwdBiol_base<T>::get_srr() const{
    return srr;
}

// Total biomass at the beginning of the timestep
template <typename T>
FLQuant_base<T> fwdBiol_base<T>::biomass() const {
    auto biomass = quant_sum(n() * wt());
    return biomass;
}

// Subset biomass
template <typename T>
FLQuant_base<T> fwdBiol_base<T>::biomass(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const { 
    if(indices_min.size() != 5 | indices_max.size() != 5){
        Rcpp::stop("In fwdBiol biomass subset. indices not of length 5\n");
    }
    std::vector<unsigned int> dim = n().get_dim();
    // Add age range to indices
    std::vector<unsigned int> new_indices_min = indices_min;
    std::vector<unsigned int> new_indices_max = indices_max;
    new_indices_min.insert(new_indices_min.begin(), 1);
    new_indices_max.insert(new_indices_max.begin(), dim[0]);
    FLQuant_base<T> biomass = quant_sum(n_flq(new_indices_min, new_indices_max) * wt_flq(new_indices_min, new_indices_max));
    return biomass;
}

/*------------------------------------------------------------*/
// fwdBiols class

// Default constructor
// Does nothing
template <typename T>
fwdBiols_base<T>::fwdBiols_base(){
}

// Each element of flbs_list is a list containing the fwdBiol components:
// FLBiol, params, residuals, residuals_mult 
template <typename T>
fwdBiols_base<T>::fwdBiols_base(Rcpp::List flbs_list){
    auto no_biols = flbs_list.size();
    biols.reserve(no_biols);
    // Go through the biols list and make the fwdBiol elements
    for (Rcpp::List flb_list: flbs_list){
        fwdBiol_base<T> flb(flb_list["biol"], flb_list["srr_model_name"], flb_list["srr_params"], flb_list["srr_residuals"], flb_list["srr_residuals_mult"]);
        biols.push_back(flb);
    }
    names = flbs_list.names();
}

// Intrusive wrap - Just the FLBiol bits - no SRR bits
template<typename T>
fwdBiols_base<T>::operator SEXP() const{
    Rcpp::S4 flbs_s4("FLBiols");
    //Rprintf("Wrapping FLBiols_base<T>.\n");
    Rcpp::List list_out;
    // wrap of each fwdBiol is just the FLBiol part
    for (auto biol : biols){
        list_out.push_back(biol);
    }
    flbs_s4.slot(".Data") = list_out;
    flbs_s4.slot("names") = names;
    return flbs_s4;
}

// Constructor from an fwdBiol
template <typename T> 
fwdBiols_base<T>::fwdBiols_base(fwdBiol_base<T> flb){
    biols.push_back(flb);
}

// Copy constructor - else 'data' can be pointed at by multiple instances
template<typename T>
fwdBiols_base<T>::fwdBiols_base(const fwdBiols_base<T>& fwdBiols_source){
    //Rprintf("In fwdBiols_base<T> copy constructor\n");
	biols = fwdBiols_source.biols; // std::vector always does deep copy
    names = Rcpp::clone<Rcpp::CharacterVector>(fwdBiols_source.names);
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
template<typename T>
fwdBiols_base<T>& fwdBiols_base<T>::operator = (const fwdBiols_base<T>& fwdBiols_source){
    //Rprintf("In fwdBiols_base<T> assignment operator\n");
	if (this != &fwdBiols_source){
        biols  = fwdBiols_source.biols; // std::vector always does deep copy
        names = Rcpp::clone<Rcpp::CharacterVector>(fwdBiols_source.names);
	}
	return *this;
}

template<typename T>
unsigned int fwdBiols_base<T>::get_nbiols() const {
    return biols.size();
}

// Get only data accessor - single element - starts at 1
template <typename T>
fwdBiol_base<T> fwdBiols_base<T>::operator () (const unsigned int element) const{
    if (element > get_nbiols()){
        Rcpp::stop("fwdBiols_base: Trying to access element larger than data size.");
    }
    return biols[element-1];
}

// Data accessor - single element - starts at 1
template <typename T>
fwdBiol_base<T>& fwdBiols_base<T>::operator () (const unsigned int element){
    if (element > get_nbiols()){
        Rcpp::stop("fwdBiols_base: Trying to access element larger than data size.");
    }
	return biols[element-1];
}

// Add another fwdBiol_base<T> to the data
template <typename T>
void fwdBiols_base<T>::operator() (const fwdBiol_base<T> fwb){
    biols.push_back(fwb);
}



// Explicit instantiation of classes
template class fwdBiol_base<double>;
template class fwdBiol_base<adouble>;
template class fwdBiols_base<double>;
template class fwdBiols_base<adouble>;
