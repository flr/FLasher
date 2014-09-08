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


// Explicit instantiation of class
template class fwdBiol_base<double>;
template class fwdBiol_base<adouble>;

