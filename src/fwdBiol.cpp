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
    srr = fwdSR_base<T>();
}

// Annoying init because you can't delegate constructors until C++11
template <typename T>
void fwdBiol_base<T>::init(const SEXP flb_sexp, const fwdSR_base<T> srr_in){
    //Rprintf("In fwdBiol init\n");
    Rcpp::S4 fwdb_s4 = Rcpp::as<Rcpp::S4>(flb_sexp);
    name = Rcpp::as<std::string>(fwdb_s4.slot("name"));
    desc = Rcpp::as<std::string>(fwdb_s4.slot("desc"));
    range = fwdb_s4.slot("range");
    n_flq = fwdb_s4.slot("n");
    wt_flq = fwdb_s4.slot("wt");
    m_flq = fwdb_s4.slot("m");
    spwn_flq = fwdb_s4.slot("spwn");
    fec_flq = fwdb_s4.slot("fec");
    srr = srr_in;
}

// Constructor from a SEXP S4 FLBiol
// But this does not set the SRR!
// Need another constructor for that
// Used as intrusive 'as'
template <typename T>
fwdBiol_base<T>::fwdBiol_base(SEXP flb_sexp){
    //Rprintf("In fwdBiol as\n");
    // Empty fwdSR
    fwdSR_base<T> srr;
    init(flb_sexp, srr);
}

// Constructor from FLBiol and fwdSR
template <typename T>
fwdBiol_base<T>::fwdBiol_base(const SEXP flb_sexp, const fwdSR_base<T> srr_in){
    init(flb_sexp, srr_in);
} 

// Constructor from FLBiol and fwdSR bits
template <typename T>
fwdBiol_base<T>::fwdBiol_base(const SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult){
    //Rprintf("In FLBiol and FLSR bits constructor.\n");
    fwdSR_base<T> srr(model_name, params, timelag, residuals, residuals_mult);
    init(flb_sexp, srr);
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
    return Rcpp::wrap(flb_s4);
}

// Get const accessors
template <typename T>
FLQuant_base<T> fwdBiol_base<T>::n() const {
    return n_flq;
}

template <typename T>
FLQuant fwdBiol_base<T>::wt() const {
    return wt_flq;
}

template <typename T>
FLQuant fwdBiol_base<T>::m() const {
    return m_flq;
}
template <typename T>
FLQuant fwdBiol_base<T>::spwn() const {
    return spwn_flq;
}
template <typename T>
FLQuant fwdBiol_base<T>::fec() const {
    return fec_flq;
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
fwdSR_base<T> fwdBiol_base<T>::get_srr() const{
    return srr;
}

// Total biomass at the beginning of the timestep
template <typename T>
FLQuant_base<T> fwdBiol_base<T>::biomass() const {
    FLQuant_base<T> biomass = quant_sum(n() * wt());
    return biomass;
}

// Subset biomass
template <typename T>
FLQuant_base<T> fwdBiol_base<T>::biomass(const int quant_min, const int quant_max, const int year_min, const int year_max, const int unit_min, const int unit_max, const int season_min, const int season_max, const int area_min, const int area_max, const int iter_min, const int iter_max) const { 
    FLQuant_base<T> biomass = quant_sum(n_flq(quant_min, quant_max, year_min, year_max, unit_min, unit_max, season_min, season_max, area_min, area_max, iter_min, iter_max) * wt_flq(quant_min, quant_max, year_min, year_max, unit_min, unit_max, season_min, season_max, area_min, area_max, iter_min, iter_max));
    return biomass;
}




/*------------------------------------------------------------*/
// fwdBiols class

// Default constructor
// Does nothing
template <typename T>
fwdBiols_base<T>::fwdBiols_base(){
}

// Would like to speed up by not using push_back but cannot define size in advance as we don't what it is!
// Constructor from a list: list_fwdBiol
// Each element of list_fwdBiol is a list containing the fwdBiol components:
// FLBiol, params, residuals, timelag, residuals_mult 
// Used as intrusive 'as'
template <typename T>
fwdBiols_base<T>::fwdBiols_base(SEXP flbs_list_sexp){
    //Rprintf("In FLBiols SEXP constructor\n");
    Rcpp::List flbs_list = Rcpp::as<Rcpp::List>(flbs_list_sexp);
    //fwdBiol_base<T> flb; // empty biol to fill up and put into the list
    for (unsigned int biol_counter=0; biol_counter < flbs_list.size(); ++biol_counter){
        Rcpp::List flb_list = Rcpp::as<Rcpp::List>(flbs_list[biol_counter]);
        // Make a new fwdBiol by pulling out each element by name
        fwdBiol_base<T> flb(flb_list["biol"],
            Rcpp::as<std::string>(flb_list["srr_model_name"]),
            Rcpp::as<FLQuant>(flb_list["srr_params"]),
            Rcpp::as<int>(flb_list["srr_timelag"]),
            Rcpp::as<FLQuant>(flb_list["srr_residuals"]),
            Rcpp::as<bool>(flb_list["srr_residuals_mult"]));
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
    for (unsigned int i = 0; i < get_nbiols(); i++){
        list_out.push_back(biols[i]);
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



// Explicit instantiation of class
template class fwdBiol_base<double>;
template class fwdBiol_base<adouble>;
template class fwdBiols_base<double>;
template class fwdBiols_base<adouble>;
