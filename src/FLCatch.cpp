/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../inst/include/FLCatch.h"

/*-------------------------------------------------*/
// Templated class

// Default constructor
// Just an empty object
template <typename T>
FLCatch_base<T>::FLCatch_base(){
    name = std::string();
    desc = std::string(); 
    range = Rcpp::NumericVector();
    landings_n_flq = FLQuant_base<T>();
    discards_n_flq = FLQuant_base<T>();
    landings_wt_flq = FLQuant();
    discards_wt_flq = FLQuant();
    catch_sel_flq = FLQuant();
    price_flq = FLQuant();
    //catch_q_flq = FLQuant();
}

// Constructor from a SEXP S4 FLCatch
// Used as intrusive 'as'
template <typename T>
FLCatch_base<T>::FLCatch_base(SEXP flc_sexp){
    Rcpp::S4 flc_s4 = Rcpp::as<Rcpp::S4>(flc_sexp);
    name = Rcpp::as<std::string>(flc_s4.slot("name"));
    desc = Rcpp::as<std::string>(flc_s4.slot("desc"));
    range = flc_s4.slot("range");
    landings_n_flq = flc_s4.slot("landings.n");
    discards_n_flq = flc_s4.slot("discards.n");
    landings_wt_flq = flc_s4.slot("landings.wt");
    discards_wt_flq = flc_s4.slot("discards.wt");
    catch_sel_flq = flc_s4.slot("catch.sel");
    price_flq = flc_s4.slot("price");
    //catch_q_flq = flc_s4.slot("catch.q");
}

// Copy constructor - else members can be pointed at by multiple instances
template <typename T>
FLCatch_base<T>::FLCatch_base(const FLCatch_base<T>& FLCatch_source){
    name = FLCatch_source.name;
    desc = FLCatch_source.desc;
    range = Rcpp::clone<Rcpp::NumericVector>(FLCatch_source.range);
    landings_n_flq = FLCatch_source.landings_n_flq;
    discards_n_flq = FLCatch_source.discards_n_flq;
    landings_wt_flq = FLCatch_source.landings_wt_flq;
    discards_wt_flq = FLCatch_source.discards_wt_flq;
    catch_sel_flq = FLCatch_source.catch_sel_flq;
    price_flq = FLCatch_source.price_flq;
    //catch_q_flq = FLCatch_source.catch_q_flq;
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
template <typename T>
FLCatch_base<T>& FLCatch_base<T>::operator = (const FLCatch_base<T>& FLCatch_source){
	if (this != &FLCatch_source){
        name = FLCatch_source.name;
        desc = FLCatch_source.desc;
        range = Rcpp::clone<Rcpp::NumericVector>(FLCatch_source.range);
        landings_n_flq = FLCatch_source.landings_n_flq;
        discards_n_flq = FLCatch_source.discards_n_flq;
        landings_wt_flq = FLCatch_source.landings_wt_flq;
        discards_wt_flq = FLCatch_source.discards_wt_flq;
        catch_sel_flq = FLCatch_source.catch_sel_flq;
        price_flq = FLCatch_source.price_flq;
        //catch_q_flq = FLCatch_source.catch_q_flq;
	}
	return *this;
}


/* Intrusive 'wrap' */
// Returns an FLBiol and ignores the SRR
template <typename T>
FLCatch_base<T>::operator SEXP() const{
    //Rprintf("Wrapping FLCatch_base<T>.\n");
    Rcpp::S4 flc_s4("FLCatch");
    flc_s4.slot("name") = name;
    flc_s4.slot("desc") = desc;
    flc_s4.slot("range") = range;
    flc_s4.slot("landings.n") = landings_n_flq;
    flc_s4.slot("discards.n") = discards_n_flq;
    flc_s4.slot("landings.wt") = landings_wt_flq;
    flc_s4.slot("discards.wt") = discards_wt_flq;
    flc_s4.slot("catch.sel") = catch_sel_flq;
    flc_s4.slot("price") = price_flq;
    //flc_s4.slot("catch.q") = catch_q_flq;
    return Rcpp::wrap(flc_s4);
}

// Accessors
// Get only
template <typename T>
FLQuant_base<T> FLCatch_base<T>::landings_n() const {
    return landings_n_flq;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::discards_n() const {
    return discards_n_flq;
}

template <typename T>
FLQuant FLCatch_base<T>::landings_wt() const {
    return landings_wt_flq;
}

template <typename T>
FLQuant FLCatch_base<T>::discards_wt() const {
    return discards_wt_flq;
}

template <typename T>
FLQuant FLCatch_base<T>::catch_sel() const {
    return catch_sel_flq;
}

template <typename T>
FLQuant FLCatch_base<T>::price() const {
    return price_flq;
}

// Get and Set
template <typename T>
FLQuant_base<T>& FLCatch_base<T>::landings_n() {
    return landings_n_flq;
}

template <typename T>
FLQuant_base<T>& FLCatch_base<T>::discards_n() {
    return discards_n_flq;
}

template <typename T>
FLQuant& FLCatch_base<T>::landings_wt() {
    return landings_wt_flq;
}

template <typename T>
FLQuant& FLCatch_base<T>::discards_wt() {
    return discards_wt_flq;
}

template <typename T>
FLQuant& FLCatch_base<T>::catch_sel() {
    return catch_sel_flq;
}

template <typename T>
FLQuant& FLCatch_base<T>::price() {
    return price_flq;
}
/*
template <typename T>
FLQuant& FLCatch_base<T>::catch_q() {
    return catch_q_flq;
}
*/

// methods
template <typename T>
FLQuant_base<T> FLCatch_base<T>::landings() const {
    FLQuant_base<T> landings = quant_sum(landings_n() * landings_wt());
    return landings;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::discards() const {
    FLQuant_base<T> discards = quant_sum(discards_n() * discards_wt());
    return discards;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::catch_n() const {
    FLQuant_base<T> catch_n = discards_n() + landings_n();
    return catch_n;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::catches() const {
    FLQuant_base<T> catches = quant_sum((discards_n() * discards_wt())  + (landings_n() * landings_wt()));
    return catches;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::catch_wt() const {
    FLQuant_base<T> catch_wt = ((landings_wt() * landings_n()) + (discards_wt() * discards_n())) / (landings_n() * discards_n());
    return catch_wt;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::discards_ratio() const {
    FLQuant_base<T> discards_ratio = discards_n() / catch_n();
    return discards_ratio;
}


template <typename T>
FLQuant_base<T> FLCatch_base<T>::landings_sel() const {
    FLQuant_base<T> landings_sel = catch_sel() * (1.0 - discards_ratio());
    return scale_by_max_quant(landings_sel);
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::discards_sel() const {
    FLQuant_base<T> discards_sel = catch_sel() * discards_ratio();
    return scale_by_max_quant(discards_sel);
}

template <typename T>
Rcpp::NumericVector FLCatch_base<T>::get_range() const {
    return range;
}


/*------------------------------------------------------------*/
// FLCatches class


// Default constructor
// Just an empty object
template <typename T>
FLCatches_base<T>::FLCatches_base(){
    //Rprintf("In FLCatches empty constructor\n");
    //catches
}

// Constructor from a list of SEXP S4 FLCatch objects
// Used as intrusive 'as'
template <typename T>
FLCatches_base<T>::FLCatches_base(SEXP flcs_sexp){
    //Rprintf("In FLCatches SEXP constructor\n");
    Rcpp::S4 flcs_s4 = Rcpp::as<Rcpp::S4>(flcs_sexp);
    desc = Rcpp::as<std::string>(flcs_s4.slot("desc"));
    names = flcs_s4.slot("names");
    Rcpp::List catch_list = Rcpp::as<Rcpp::List>(flcs_s4.slot(".Data"));
    Rcpp::List::iterator lst_iterator;
    for (lst_iterator = catch_list.begin(); lst_iterator != catch_list.end(); ++ lst_iterator){
        catches.push_back(*lst_iterator);
    }
}

// Intrusive wrap
// List is unamed
template<typename T>
FLCatches_base<T>::operator SEXP() const{
    Rcpp::S4 flcs_s4("FLCatches");
    //Rprintf("Wrapping FLCatches_base<T>.\n");
    Rcpp::List list_out;
    for (unsigned int i = 0; i < get_ncatches(); i++){
        list_out.push_back(catches[i]);
    }
    flcs_s4.slot(".Data") = list_out;
    flcs_s4.slot("desc") = desc;
    flcs_s4.slot("names") = names;
    return flcs_s4;
}

// Constructor from an FLCatch
template <typename T> 
FLCatches_base<T>::FLCatches_base(FLCatch_base<T> flc){
    catches.push_back(flc);
}

// Copy constructor - else 'data' can be pointed at by multiple instances
template<typename T>
FLCatches_base<T>::FLCatches_base(const FLCatches_base<T>& FLCatches_source){
    //Rprintf("In FLCatches_base<T> copy constructor\n");
	catches  = FLCatches_source.catches; // std::vector always does deep copy
    desc = FLCatches_source.desc;
    names = Rcpp::clone<Rcpp::CharacterVector>(FLCatches_source.names);
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
template<typename T>
FLCatches_base<T>& FLCatches_base<T>::operator = (const FLCatches_base<T>& FLCatches_source){
    //Rprintf("In FLCatches_base<T> assignment operator\n");
	if (this != &FLCatches_source){
        catches  = FLCatches_source.catches; // std::vector always does deep copy
        desc = FLCatches_source.desc;
        names = Rcpp::clone<Rcpp::CharacterVector>(FLCatches_source.names);
	}
	return *this;
}

template<typename T>
unsigned int FLCatches_base<T>::get_ncatches() const {
    return catches.size();
}

// Add another FLCatch_base<T> to the data
template <typename T>
void FLCatches_base<T>::operator() (const FLCatch_base<T> flc){
    catches.push_back(flc);
}

// Get only data accessor - single element - starts at 1
template <typename T>
FLCatch_base<T> FLCatches_base<T>::operator () (const unsigned int element) const{
    if (element > get_ncatches()){
        Rcpp::stop("FLCatches_base: Trying to access element larger than data size.");
    }
    return catches[element-1];
}

// Data accessor - single element - starts at 1
template <typename T>
FLCatch_base<T>& FLCatches_base<T>::operator () (const unsigned int element){
    //Rprintf("In single element accessor\n");
    if (element > get_ncatches()){
        Rcpp::stop("FLCatches_base: Trying to access element larger than data size.");
    }
	return catches[element-1];
}


// Explicit instantiation of classes
template class FLCatch_base<double>;
template class FLCatch_base<adouble>;
template class FLCatches_base<double>;
template class FLCatches_base<adouble>;




