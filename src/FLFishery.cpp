/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../inst/include/FLFishery.h"

/*-------------------------------------------------*/
// Default constructor
// Just an empty object
template <typename T>
FLFishery_base<T>::FLFishery_base(){
    name = std::string();
    effort_flq = FLQuant();
    vcost_flq = FLQuant();
    fcost_flq = FLQuant();
}

// Constructor from a SEXP S4 FLFishery
// Used as intrusive 'as'
template <typename T>
FLFishery_base<T>::FLFishery_base(SEXP flf_sexp) : FLCatches_base<T>(flf_sexp){ // Force the FLCatches SEXP constructor to be called first
    Rcpp::S4 flf_s4 = Rcpp::as<Rcpp::S4>(flf_sexp);
    name = Rcpp::as<std::string>(flf_s4.slot("name"));
    effort_flq = flf_s4.slot("effort");
    vcost_flq = flf_s4.slot("vcost");
    fcost_flq = flf_s4.slot("fcost");
    range = flf_s4.slot("range");
}

/* Intrusive 'wrap' */
// Note that we have to use this manky FLCatches_base<T>::member_of_FLCatches syntax with templated inheritance
// Returns an FLFishery
template <typename T>
FLFishery_base<T>::operator SEXP() const{
    //Rprintf("Wrapping FLFishery_base<T>.\n");
    Rcpp::S4 flf_s4("FLFishery");
    flf_s4.slot("name") = name;
    flf_s4.slot("effort") = effort_flq;
    flf_s4.slot("vcost") = vcost_flq;
    flf_s4.slot("fcost") = fcost_flq;
    flf_s4.slot("range") = range;
    // FLCatches bits - can't call wrap on these bits
    Rcpp::List list_out;
    for (unsigned int i = 0; i < FLCatches_base<T>::get_ncatches(); i++){
        list_out.push_back(FLCatches_base<T>::catches[i]);
    }
    flf_s4.slot(".Data") = list_out;
    flf_s4.slot("desc") = FLCatches_base<T>::desc;
    flf_s4.slot("names") = FLCatches_base<T>::names;
    return Rcpp::wrap(flf_s4);
}

// Copy constructor - else members can be pointed at by multiple instances
template <typename T>
FLFishery_base<T>::FLFishery_base(const FLFishery_base<T>& FLFishery_source) : FLCatches_base<T>(FLFishery_source){
    //Rprintf("In FLFishery_base copy constructor\n");
    name = FLFishery_source.name;
    effort_flq = FLFishery_source.effort_flq;
    vcost_flq = FLFishery_source.vcost_flq;
    fcost_flq = FLFishery_source.fcost_flq;
    range = Rcpp::clone<Rcpp::NumericVector>(FLFishery_source.range);
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
template <typename T>
FLFishery_base<T>& FLFishery_base<T>::operator = (const FLFishery_base<T>& FLFishery_source){
    //Rprintf("In FLFishery_base assignment operator\n");
	if (this != &FLFishery_source){
        // Need to add all the bits from a FLCatches too
        FLCatches_base<T>::operator=(FLFishery_source);
        name = FLFishery_source.name;
        effort_flq = FLFishery_source.effort_flq;
        vcost_flq = FLFishery_source.vcost_flq;
        fcost_flq = FLFishery_source.fcost_flq;
        range = Rcpp::clone<Rcpp::NumericVector>(FLFishery_source.range);
	}
	return *this;
}

// Accessors of economic slots
template <typename T>
FLQuant FLFishery_base<T>::effort() const {
    return effort_flq;
}

template <typename T>
FLQuant FLFishery_base<T>::vcost() const {
    return vcost_flq;
}

template <typename T>
FLQuant FLFishery_base<T>::fcost() const {
    return fcost_flq;
}

template <typename T>
FLQuant& FLFishery_base<T>::effort() {
    return effort_flq;
}

template <typename T>
FLQuant& FLFishery_base<T>::vcost() {
    return vcost_flq;
}

template <typename T>
FLQuant& FLFishery_base<T>::fcost() {
    return fcost_flq;
}

/*-------------------------------------------------*/
// FLFisheries

// Default constructor
// Just an empty object
template <typename T>
FLFisheries_base<T>::FLFisheries_base(){
    //Rprintf("In FLFisheries empty constructor\n");
}

// Constructor from a SEXP S4 FLFisheries
// Used as intrusive 'as'
template <typename T>
FLFisheries_base<T>::FLFisheries_base(SEXP flfs_sexp) { 
    Rcpp::S4 flfs_s4 = Rcpp::as<Rcpp::S4>(flfs_sexp);
    desc = Rcpp::as<std::string>(flfs_s4.slot("desc"));
    names = flfs_s4.slot("names");
    Rcpp::List fishery_list = Rcpp::as<Rcpp::List>(flfs_s4.slot(".Data"));
    Rcpp::List::iterator lst_iterator;
    for (lst_iterator = fishery_list.begin(); lst_iterator != fishery_list.end(); ++ lst_iterator){
        fisheries.push_back(*lst_iterator);
    }
}

/* Intrusive 'wrap' */
// Returns an FLFisheries
template <typename T>
FLFisheries_base<T>::operator SEXP() const{
    Rcpp::S4 flfs_s4("FLFisheries");
    Rcpp::List list_out;
    for (unsigned int i = 0; i < get_nfisheries(); i++){
        list_out.push_back(fisheries[i]);
    }
    flfs_s4.slot(".Data") = list_out;
    flfs_s4.slot("desc") = desc;
    flfs_s4.slot("names") = names;
    return Rcpp::wrap(flfs_s4);
}

// Copy constructor - else 'data' can be pointed at by multiple instances
template<typename T>
FLFisheries_base<T>::FLFisheries_base(const FLFisheries_base<T>& FLFisheries_source){
	fisheries  = FLFisheries_source.fisheries; // std::vector always does deep copy
    desc = FLFisheries_source.desc;
    names = Rcpp::clone<Rcpp::CharacterVector>(FLFisheries_source.names);
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
template<typename T>
FLFisheries_base<T>& FLFisheries_base<T>::operator = (const FLFisheries_base<T>& FLFisheries_source){
	if (this != &FLFisheries_source){
        fisheries  = FLFisheries_source.fisheries; // std::vector always does deep copy
        desc = FLFisheries_source.desc;
        names = Rcpp::clone<Rcpp::CharacterVector>(FLFisheries_source.names);
	}
	return *this;
}

// Accessors
template<typename T>
unsigned int FLFisheries_base<T>::get_nfisheries() const {
    return fisheries.size();
}

// Get only data accessor - single element - starts at 1
template <typename T>
FLFishery_base<T> FLFisheries_base<T>::operator () (const unsigned int fishery) const{
    if (fishery > get_nfisheries()){
        Rcpp::stop("FLFisheries_base: Trying to access fishery larger than data size.");
    }
    return fisheries[fishery-1];
}

// Data accessor - single element - starts at 1
template <typename T>
FLFishery_base<T>& FLFisheries_base<T>::operator () (const unsigned int fishery){
    if (fishery > get_nfisheries()){
        Rcpp::stop("FLFisheries_base: Trying to access fishery larger than data size.");
    }
	return fisheries[fishery-1];
}

// Get only data accessor - two elements - both start at 1
template <typename T>
FLCatch_base<T> FLFisheries_base<T>::operator () (const unsigned int fishery, const unsigned int catches) const{
    if (fishery > get_nfisheries()){
        Rcpp::stop("FLFisheries_base: Trying to access fishery larger than data size.");
    }
    if (catches > (*this)(fishery).get_ncatches()){
        Rcpp::stop("FLFisheries_base: Trying to access catches larger than data size.");
    }
    return (*this)(fishery)(catches);
}

// Data accessor - two elements - both start at 1
template <typename T>
FLCatch_base<T>& FLFisheries_base<T>::operator () (const unsigned int fishery, const unsigned int catches) {
    if (fishery > get_nfisheries()){
        Rcpp::stop("FLFisheries_base: Trying to access fishery larger than data size.");
    }
    if (catches > (*this)(fishery).get_ncatches()){
        Rcpp::stop("FLFisheries_base: Trying to access catches larger than data size.");
    }
    return (*this)(fishery)(catches);
}

// FLCatch slot accessors
//T landings_n(std::vector<int>) const;

// Explicit instantiation of classes
template class FLFishery_base<double>;
template class FLFishery_base<adouble>;
template class FLFisheries_base<double>;
template class FLFisheries_base<adouble>;

