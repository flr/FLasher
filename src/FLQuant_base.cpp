/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include <time.h> // included for some basic profiling
#include "../inst/include/FLQuant_base.h"

// Default constructor
// No dimnames set as the array is null
template <typename T>
FLQuant_base<T>::FLQuant_base(){
    //Rprintf("In FLQuant_base<T> basic constructor\n");
	units = std::string(); // Empty string - just ""
    dim = Rcpp::IntegerVector();
    dimnames = Rcpp::List();
}

// Generic SEXP constructor
// Used as intrusive 'as'
// Need to add check that the SEXP is an FLQuant
template<typename T>
FLQuant_base<T>::FLQuant_base(SEXP flq_sexp){
    //Rprintf("In FLQuant_base SEXP constructor\n");
	Rcpp::S4 flq_s4 = Rcpp::as<Rcpp::S4>(flq_sexp);
    Rcpp::NumericVector data_nv = flq_s4.slot(".Data");
    // Initialise data to the correct size?
    // Fill up data
    // Add reserve() in here first to make data the correct size?
    // data(nquant * nyear * nunit * nseason * narea * niter)?
    data.insert(data.begin(), data_nv.begin(), data_nv.end());
	units = Rcpp::as<std::string>(flq_s4.slot("units"));
	dim = data_nv.attr("dim");
	dimnames = data_nv.attr("dimnames");
    //Rprintf("Finishing FLQ_base constructor\n");
}

// Make an FLQuant of a certain size filled with 0
// Note that units and dimnames have not been set
template <typename T>
FLQuant_base<T>::FLQuant_base(const int nquant, const int nyear, const int nunit, const int nseason, const int narea, const int niter){
    //Rprintf("Making a new FLQuant_base<T> with user defined dims\n");
	units = std::string(); // Empty string - just ""
    dim = Rcpp::IntegerVector::create(nquant, nyear, nunit, nseason, narea, niter);
    data = std::vector<T>(nquant * nyear * nunit * nseason * narea * niter,0.0);
    // How to fill dimnames up appropriately?
    // Just of the right size at the moment.
    // Could use ::create to pass in actual characters, but then do we want just 1:nage, 1:nyear etc?
    dimnames = Rcpp::List::create(
            Rcpp::Named("quant", Rcpp::CharacterVector(nquant)),
            Rcpp::Named("year", Rcpp::CharacterVector(nyear)),
            Rcpp::Named("unit", Rcpp::CharacterVector(nunit)),
            Rcpp::Named("season", Rcpp::CharacterVector(nseason)),
            Rcpp::Named("area", Rcpp::CharacterVector(narea)),
            Rcpp::Named("iter", Rcpp::CharacterVector(niter)));
}

/* Used as intrusive 'wrap'
 * This needs to be specialised for T
 * i.e. wrapping a double is different to wrapping adouble
 * So this is a generic wrap that just returns a 0
 */
template<typename T>
FLQuant_base<T>::operator SEXP() const{
    Rprintf("Wrapping generic FLQuant_base. Probably not what you wanted.\n");
    int x = 0;
    return Rcpp::wrap(x);
}

// Specialise the wrap for an FLQuant_base<double>
template<>
FLQuant_base<double>::operator SEXP() const{
    //Rprintf("Specialised wrapping FLQuant_base<double>\n");
    Rcpp::S4 flq_s4("FLQuant");
    //// Make and fill the NumericVector that will be the 'data' slot 
    Rcpp::NumericVector data_nv(data.size(),0.0);
    for (int i = 0; i < data.size(); ++i){
        data_nv[i] = data[i];
    }
    // Apply dims and dimnames
	data_nv.attr("dim") = dim;
	data_nv.attr("dimnames") = dimnames;
    // Fill the slots
    flq_s4.slot(".Data") = data_nv;
    flq_s4.slot("units") = units;
    return Rcpp::wrap(flq_s4);
}

// Specialise the wrap for an FLQuant_base<adouble>
// Necessary because we have to pull .value() out
template<>
FLQuant_base<adouble>::operator SEXP() const{
    //Rprintf("Specialised wrapping FLQuant_base<adouble>\n");
    Rcpp::S4 flq_s4("FLQuant");
    // Make and fill the NumericVector that will be the 'data' slot 
    Rcpp::NumericVector data_nv(data.size(), 0.0);
    for (int i = 0; i < data.size(); ++i){
        data_nv[i] = Value(data[i]);
    }
    // Apply dims and dimnames
	data_nv.attr("dim") = dim;
	data_nv.attr("dimnames") = dimnames;
    // Fill the slots
    flq_s4.slot(".Data") = data_nv;
    flq_s4.slot("units") = units;
    return Rcpp::wrap(flq_s4);
}

// Copy constructor - else 'data' can be pointed at by multiple instances
template<typename T>
FLQuant_base<T>::FLQuant_base(const FLQuant_base<T>& FLQuant_source){
    //Rprintf("In FLQuant_base<T> copy constructor\n");
	data  = FLQuant_source.data; // std::vector always does deep copy
	units = FLQuant_source.units; // std::string always does deep copy
    dim = Rcpp::clone<Rcpp::IntegerVector>(FLQuant_source.dim);
    dimnames = Rcpp::clone<Rcpp::List>(FLQuant_source.dimnames);
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
template<typename T>
FLQuant_base<T>& FLQuant_base<T>::operator = (const FLQuant_base<T>& FLQuant_source){
    //Rprintf("In FLQuant_base<T> assignment operator\n");
	if (this != &FLQuant_source){
        data  = FLQuant_source.data; // std::vector always does deep copy
        units = FLQuant_source.units; // std::string always does deep copy
        dim = Rcpp::clone<Rcpp::IntegerVector>(FLQuant_source.dim);
        dimnames = Rcpp::clone<Rcpp::List>(FLQuant_source.dimnames);
	}
	return *this;
}

// Construct FLQuant_base<T> from an FLQuant_base<T2>
// Need specialisation
template <typename T>
template <typename T2>
FLQuant_base<T>::FLQuant_base(const FLQuant_base<T2>& FLQuant_source){
    Rprintf("In constructor for FLQuant_base<T>(FLQuant_base<T2>)\n");
    Rcpp::stop("I have no specific instructions for these types. Please add a specialisation\n");
}

// Specialise the FLQuant_base<T>(FLQuant_base<T2>) constructor 
// Make an FLQuantAdolc / CppAD from an FLQuant
template <>
template <>
FLQuant_base<adouble>::FLQuant_base(const FLQuant_base<double>& FLQuant_source){
    //Rprintf("Making an FLQuantAdolc / CppAD from an FLQuant\n");
    units = FLQuant_source.get_units(); // std::string always does deep copy
    dim = FLQuant_source.get_dim();
    dimnames = FLQuant_source.get_dimnames(); 
    std::vector<double> source_data = FLQuant_source.get_data();
    data.insert(data.begin(), source_data.begin(), source_data.end());
}


//------------------ Accessors ---------------------------------

template <typename T>
std::vector<T> FLQuant_base<T>::get_data() const{
	return data;
}

template <typename T>
std::string FLQuant_base<T>::get_units() const{
	return units;
}

template <typename T>
Rcpp::IntegerVector FLQuant_base<T>::get_dim() const{
	return Rcpp::clone<Rcpp::IntegerVector>(dim);
}

template <typename T>
Rcpp::List FLQuant_base<T>::get_dimnames() const{
	return Rcpp::clone<Rcpp::List>(dimnames);
}

template <typename T>
unsigned int FLQuant_base<T>::get_size() const{
	return data.size();
}

template <typename T>
int FLQuant_base<T>::get_nquant() const{
	Rcpp::IntegerVector dim = get_dim();
	return dim(0);
}

template <typename T>
int FLQuant_base<T>::get_nyear() const{
	Rcpp::IntegerVector dim = get_dim();
	return dim(1);
}

template <typename T>
int FLQuant_base<T>::get_nunit() const{
	Rcpp::IntegerVector dim = get_dim();
	return dim(2);
}

template <typename T>
int FLQuant_base<T>::get_nseason() const{
	Rcpp::IntegerVector dim = get_dim();
	return dim(3);
}

template <typename T>
int FLQuant_base<T>::get_narea() const{
	Rcpp::IntegerVector dim = get_dim();
	return dim(4);
}

template <typename T>
int FLQuant_base<T>::get_niter() const{
	Rcpp::IntegerVector dim = get_dim();
	return dim(5);
}

// Note that elements start at 1 NOT 0!
template <typename T>
int FLQuant_base<T>::get_data_element(const int quant, const int year, const int unit, const int season, const int area, int iter) const{
    Rcpp::IntegerVector dim = get_dim();
    if ((quant > dim(0)) || (year > dim(1)) || (unit > dim(2)) || (season > dim(3)) || (area > dim(4))){
            Rcpp::stop("Trying to access element outside of quant, year, unit, season or area dim range.");
    }
    // If only 1 iter and trying to get n iter, set iter to 1
    if ((iter > 1) && (dim(5) == 1)){
        //get_data_element(quant, year, unit, season, area, 1);
        iter = 1;
    }
    if ((iter > dim(5)) && (dim(5) > 1)){
        Rcpp::stop("In get_data_element: trying to access iter > niter\n");
    } 
	unsigned int element = (get_narea() * get_nseason() * get_nunit() * get_nyear() * get_nquant() * (iter - 1)) +
			(get_nseason() * get_nunit() * get_nyear() * get_nquant() * (area - 1)) +
			(get_nunit() * get_nyear() * get_nquant() * (season - 1)) +
			(get_nyear() * get_nquant() * (unit - 1)) +
			(get_nquant() * (year - 1)) +
			(quant - 1); 
	return element;
}

// Remove all the calls to get_nxxxx() to speed up?
template <typename T>
int FLQuant_base<T>::get_data_element2(const int quant, const int year, const int unit, const int season, const int area, int iter) const{
    Rcpp::IntegerVector dim = get_dim();
    if ((quant > dim(0)) || (year > dim(1)) || (unit > dim(2)) || (season > dim(3)) || (area > dim(4))){
            Rcpp::stop("Trying to access element outside of quant, year, unit, season or area dim range.");
    }
    // If only 1 iter and trying to get n iter, set iter to 1
    if ((iter > 1) && (dim(5) == 1)){
        //get_data_element(quant, year, unit, season, area, 1);
        iter = 1;
    }
    if ((iter > dim(5)) && (dim(5) > 1)){
        Rcpp::stop("In get_data_element: trying to access iter > niter\n");
    } 
	unsigned int element = (dim(4) * dim(3) * dim(2) * dim(1) * dim(0) * (iter - 1)) +
			(dim(3) * dim(2) * dim(1) * dim(0) * (area - 1)) +
			(dim(2) * dim(1) * dim(0) * (season - 1)) +
			(dim(1) * dim(0) * (unit - 1)) +
			(dim(0) * (year - 1)) +
			(quant - 1); 
	return element;
}


// Get only data accessor - single element
template <typename T>
T FLQuant_base<T>::operator () (const unsigned int element) const{
    //Rprintf("In const single element accessor\n");
    if (element > get_size()){
        Rcpp::stop("Trying to access element larger than data size.");
    }
    return data[element-1];
}

// Data accessor - single element
template <typename T>
T& FLQuant_base<T>::operator () (const unsigned int element){
    //Rprintf("In single element accessor\n");
    if (element > get_size()){
        Rcpp::stop("Trying to access element larger than data size.");
    }
	return data[element-1];
}

// Get only data accessor - all dims
template <typename T>
T FLQuant_base<T>::operator () (const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area, const unsigned int iter) const{
    //Rprintf("In const multiple element accessor\n");
	unsigned int element = get_data_element(quant, year, unit, season, area, iter);
	return data[element];
}

// Data accessor - all dims
template <typename T>
T& FLQuant_base<T>::operator () (const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area, const unsigned int iter){
    //Rprintf("In multiple element accessor\n");
	unsigned int element = get_data_element(quant, year, unit, season, area, iter);
	return data[element];
}

// Get data accessor - all dims with an integer vector
template <typename T>
T FLQuant_base<T>::operator () (const std::vector<unsigned int> indices) const {
    if (indices.size() > 6){
        Rcpp::stop("FLQuant indices accessor - indices longer than 6.");
    }
    if (indices.size() < 6){
        Rcpp::stop("FLQuant indices accessor - indices shorter than 6.");
    }
	unsigned int element = get_data_element(indices[0],indices[1],indices[2],indices[3],indices[4],indices[5]);
	return data[element];
}

// Data accessor - all dims with an integer vector
template <typename T>
T& FLQuant_base<T>::operator () (const std::vector<unsigned int> indices) {
    if (indices.size() > 6){
        Rcpp::stop("FLQuant indices accessor - indices longer than 6.");
    }
    if (indices.size() < 6){
        Rcpp::stop("FLQuant indices accessor - indices shorter than 6.");
    }
	unsigned int element = get_data_element(indices[0],indices[1],indices[2],indices[3],indices[4],indices[5]);
	return data[element];
}

// Subset
template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator () (const int quant_min, const int quant_max, const int year_min, const int year_max, const int unit_min, const int unit_max, const int season_min, const int season_max, const int area_min, const int area_max, const int iter_min, const int iter_max) const {
    // Check ranges
    if ((quant_min < 1) || (year_min < 1)|| (unit_min < 1)|| (season_min < 1)|| (area_min < 1)|| (iter_min < 1)) {
        Rcpp::stop("In FLQuant subsetter: requested min dimensions are less than 1.\n");
    }
    if ((quant_max > get_nquant()) || (year_max > get_nyear()) || (unit_max > get_nunit()) || (season_max > get_nseason()) || (area_max > get_narea()) || (iter_max > get_niter())){
        Rcpp::stop("In FLQuant subsetter: requested subset dimensions are outside of FLQuant bounds.\n");
    }
    // Check max >= min
    if ((quant_max < quant_min) || (year_max < year_min) || (unit_max < unit_min) || (season_max < season_min) || (area_max < area_min) || (iter_max < iter_min)){
        Rcpp::stop("In FLQuant subsetter: min dim > max\n");
    }

    std::vector<int> new_dim(6);
    new_dim[0] = quant_max - quant_min + 1;
    new_dim[1] = year_max - year_min + 1;
    new_dim[2] = unit_max - unit_min + 1;
    new_dim[3] = season_max - season_min + 1;
    new_dim[4] = area_max - area_min + 1;
    new_dim[5] = iter_max - iter_min + 1;
    // Having to do this is unfortunate - with C++11 it could be cleaner
    std::vector<int> min_dim(6);
    min_dim[0] = quant_min;
    min_dim[1] = year_min;
    min_dim[2] = unit_min;
    min_dim[3] = season_min;
    min_dim[4] = area_min;
    min_dim[5] = iter_min;

    FLQuant_base<T> out(new_dim[0], new_dim[1], new_dim[2], new_dim[3], new_dim[4], new_dim[5]);
    out.set_units(get_units());
    for (int quant_count = 1; quant_count <= new_dim[0]; ++quant_count){
        for (int year_count = 1; year_count <= new_dim[1]; ++year_count){
            for (int unit_count = 1; unit_count <= new_dim[2]; ++unit_count){
                for (int season_count = 1; season_count <= new_dim[3]; ++season_count){
                    for (int area_count = 1; area_count <= new_dim[4]; ++area_count){
                        for (int iter_count = 1; iter_count <= new_dim[5]; ++iter_count){
                            unsigned int element = get_data_element(quant_count + quant_min - 1, year_count + year_min - 1, unit_count + unit_min - 1, season_count + season_min - 1, area_count + area_min - 1, iter_count + iter_min - 1);
                            out(quant_count, year_count, unit_count, season_count, area_count, iter_count) = data[element];
    }}}}}}

    Rcpp::List old_dimnames = get_dimnames();
    Rcpp::List new_dimnames = get_dimnames();
    std::vector<std::string> temp_old_dimname;
    std::vector<std::string> temp_new_dimname;

    for (int dim_counter = 0; dim_counter < 6; ++dim_counter){ // counts over dims
        temp_old_dimname = Rcpp::as<std::vector<std::string> >(old_dimnames[dim_counter]);
        temp_new_dimname.resize(new_dim[dim_counter]); 
        for (int counter = 0; counter < new_dim[dim_counter]; ++counter){ // counts along dims
            temp_new_dimname[counter] = temp_old_dimname[counter + min_dim[dim_counter] - 1];
        }
        new_dimnames[dim_counter] = temp_new_dimname;
    }
    out.set_dimnames(new_dimnames);
    return out;
}


template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator () (const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area) const {
    FLQuant_base<T> out = (*this)(quant, quant, year, year, unit, unit, season, season, area, area, 1, get_niter());
    return out;
}


//template <typename T>
//FLQuant_base<T>& FLQuant_base<T>::operator () (const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area) {
//    FLQuant_base<T> out = (*this)(quant, quant, year, year, unit, unit, season, season, area, area, 1, get_niter());
//    return out;
//}

//------------- Setting methods ----------------

template <typename T>
void FLQuant_base<T>::set_data(const std::vector<T>& data_in){
    Rcpp::IntegerVector dim = get_dim();
    unsigned int dim_prod = (dim[0] * dim[1] * dim[2]* dim[3]* dim[4]* dim[5]);
    if(dim_prod != data_in.size()){
        Rcpp::stop("Cannot set data. Data size does not match dims.\n");
    }
    data = data_in;
}

// Checks if dimnames dimensions fit current dim
template <typename T>
void FLQuant_base<T>::set_dimnames(const Rcpp::List& dimnames_in){
    Rcpp::CharacterVector name;
    int dim_length = 0;
    for (int i = 0; i <= 5; ++i){
        name = dimnames_in[i];
        dim_length = name.size();
        if (dim_length != dim[i]){
            Rcpp::stop("Cannot set dimnames as new dimnames are different size to current dimensions\n");
        }
    }
    dimnames = dimnames_in;
}

template <typename T>
void FLQuant_base<T>::set_units(const std::string& units_in){
    units = units_in;
}

template <typename T>
FLQuant_base<T> FLQuant_base<T>::propagate_iters(const int iters) const{
    if (get_niter() > 1){
        Rcpp::stop("In FLQuant_base.extend_iters: only works if original data has 1 iter.\n");
    }
    const int new_size = data.size() * iters; 
    std::vector<T> new_data(new_size);
    std::vector<std::string> iter_dimnames(iters);
    // Copy data
    for (int iter_counter = 0; iter_counter < iters; ++iter_counter){
        iter_dimnames[iter_counter] = number_to_string(iter_counter+1);
        for (unsigned int size_counter = 0; size_counter < data.size(); ++size_counter){
            new_data[size_counter+(iter_counter*data.size())] = data[size_counter];
        }
    }
    // Make new object and return
    FLQuant_base<T> out = *this;
    out.data = new_data;
    out.dim[5] = iters;
    out.dimnames[5] = iter_dimnames;
    return out;
}

//------------------ Multiplication operators -------------------
/*  * Need to consider what happens with the combinations FLQuant<T1> * / + - FLQuant<T2>, i.e. what is the output type?
 *  adouble *  double = adouble
 *  adouble * adouble = adouble
 *  double  *  double = double
 *  Definition of friend function for arithmetic operation
 */

// Multiplication self assignment
template<typename T>
FLQuant_base<T>& FLQuant_base<T>::operator *= (const FLQuant_base<T>& rhs){
    if (dim5_matcher(get_dim(), rhs.get_dim()) != 1){
        Rcpp::stop("You cannot multiply FLQuants as dimensions 1-5 do not match.");
    }
    FLQuant_base<T> new_rhs = rhs;
    if (get_niter() > rhs.get_niter()){
        // Blow up rhs 
        new_rhs = rhs.propagate_iters(get_niter() - rhs.get_niter() + 1);
    }
    if (rhs.get_niter() > get_niter()){
        // Blow up this
        *this = (*this).propagate_iters(rhs.get_niter() - get_niter() + 1);
    }
    std::transform((*this).data.begin(), (*this).data.end(), new_rhs.data.begin(), (*this).data.begin(), std::multiplies<T>());
    return *this;
}
// Special case of multiplication assignment 
// Instantiation below ensures that it will only compile for FLQuantAdolc / CppAD *= FLQuant
// FLQuant *= FLQuantAdolc / CppAD will not compile as cannot have double = double * adouble
// Needs to be instanitated due to extra template class, T2
template <typename T>
template <typename T2>
FLQuant_base<T>& FLQuant_base<T>::operator *= (const FLQuant_base<T2>& rhs){

    if (dim5_matcher(get_dim(), rhs.get_dim()) != 1){
        Rcpp::stop("You cannot multiply FLQuants as dimensions 1-5 do not match.");
    }
    FLQuant_base<T2> new_rhs = rhs;
    if (get_niter() > rhs.get_niter()){
        // Blow up rhs 
        new_rhs = rhs.propagate_iters(get_niter() - rhs.get_niter() + 1);
    }
    if (rhs.get_niter() > get_niter()){
        // Blow up this
        *this = (*this).propagate_iters(rhs.get_niter() - get_niter() + 1);
    }
    std::vector<T2> new_rhs_data = new_rhs.get_data();

    std::transform((*this).data.begin(), (*this).data.end(), new_rhs_data.begin(), (*this).data.begin(), std::multiplies<T>());

    //std::vector<T2> rhs_data = rhs.get_data();
    //for (unsigned int i = 0; i < rhs.get_size(); ++i){
    //    data[i] = data[i] * rhs_data[i];
    //}
    return *this;
}

// FLQuant *= double
// FLQuantAdolc / CppAD *= adouble
template <typename T>
FLQuant_base<T>& FLQuant_base<T>::operator *= (const T& rhs){
    //Rprintf("In scalar T=*T multiplication assignment\n");
    std::transform((*this).data.begin(), (*this).data.end(), (*this).data.begin(), std::bind1st(std::multiplies<T>(),rhs)); 
    return *this;
}

// Special case of multiplication assignment 
// Used for FLQuantAdolc / CppAD *= double
// Needs to be instanitated due to extra template class, T2
template <typename T>
template <typename T2>
FLQuant_base<T>& FLQuant_base<T>::operator *= (const T2& rhs){
    //Rprintf("In scalar T=*T2 multiplication assignment\n");
    std::transform((*this).data.begin(), (*this).data.end(), (*this).data.begin(), std::bind1st(std::multiplies<T>(),rhs)); 
    return *this;
}

// General multiplication
// FLQuant_base<T> * FLQuant_base<T>
template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator * (const FLQuant_base<T>& rhs) const{
    //Rprintf("In self multiplication\n");
    //if (match_dims(rhs) != 1){
    //    Rcpp::stop("You cannot multiply FLQuants as your dimensions do not match.");
    //}
    FLQuant_base<T> out = *this; // Copy myself
    out *= rhs;
    return out;
}

// FLQuant_base<T> * T
template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator * (const T& rhs) const{
    FLQuant_base<T> out = *this; // Copy myself
    out *= rhs;
    return out;
}

// Outside of class
template <typename T>
FLQuant_base<T> operator * (const FLQuant_base<double>& lhs, const FLQuant_base<T>& rhs){
    //Rprintf("FLQuant_base<double> * FLQuant_base<T>\n");
    //if (lhs.match_dims(rhs) != 1){
    //    Rcpp::stop("You cannot multiply FLQuants as your dimensions do not match.");
    //}
    FLQuant_base<T> out = rhs;
    out *= lhs;
    return out;

}

template <typename T>
FLQuant_base<T> operator * (const FLQuant_base<T>& lhs, const FLQuant_base<double>& rhs){
    //Rprintf("FLQuant_base<T> * FLQuant_base<double>\n");
    //if (lhs.match_dims(rhs) != 1){
    //    Rcpp::stop("You cannot multiply FLQuants as your dimensions do not match.");
    //}
    FLQuant_base<T> out = lhs;
    out *= rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator * (const T& lhs, const FLQuant_base<T>& rhs){
    FLQuant_base<T> out = rhs;
    out *= lhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator * (const double& lhs, const FLQuant_base<T>& rhs){
    FLQuant_base<T> out = rhs;
    out *= lhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator * (const FLQuant_base<T>& lhs, const double& rhs){
    FLQuant_base<T> out = lhs;
    out *= rhs;
    return out;
}

FLQuant_base<double> operator * (const double& lhs, const FLQuant_base<double>& rhs){
    FLQuant_base<double> out = rhs;
    out *= lhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator * (const FLQuant_base<double>& lhs, const T& rhs){
    FLQuant_base<T> out(lhs);
    out *= rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator * (const T& lhs, const FLQuant_base<double>& rhs){
    FLQuant_base<T> out(rhs);
    out *= lhs;
    return out;
}

//------------------ Division operators -------------------

// Division self assignment
template<typename T>
FLQuant_base<T>& FLQuant_base<T>::operator /= (const FLQuant_base<T>& rhs){
    if (dim5_matcher(get_dim(), rhs.get_dim()) != 1){
        Rcpp::stop("You cannot divide FLQuants as dimensions 1-5 do not match.");
    }
    FLQuant_base<T> new_rhs = rhs;
    if (get_niter() > rhs.get_niter()){
        // Blow up rhs 
        new_rhs = rhs.propagate_iters(get_niter() - rhs.get_niter() + 1);
    }
    if (rhs.get_niter() > get_niter()){
        // Blow up this
        *this = (*this).propagate_iters(rhs.get_niter() - get_niter() + 1);
    }
    std::transform((*this).data.begin(), (*this).data.end(), new_rhs.data.begin(), (*this).data.begin(), std::divides<T>());
    return *this;
}
// Special case of division assignment 
// Instantiation below ensures that it will only compile for FLQuantAdolc / CppAD /= FLQuant
template <typename T>
template <typename T2>
FLQuant_base<T>& FLQuant_base<T>::operator /= (const FLQuant_base<T2>& rhs){
    if (dim5_matcher(get_dim(), rhs.get_dim()) != 1){
        Rcpp::stop("You cannot divide FLQuants as dimensions 1-5 do not match.");
    }
    FLQuant_base<T2> new_rhs = rhs;
    if (get_niter() > rhs.get_niter()){
        // Blow up rhs 
        new_rhs = rhs.propagate_iters(get_niter() - rhs.get_niter() + 1);
    }
    if (rhs.get_niter() > get_niter()){
        // Blow up this
        *this = (*this).propagate_iters(rhs.get_niter() - get_niter() + 1);
    }
    std::vector<T2> new_rhs_data = new_rhs.get_data();
    std::transform((*this).data.begin(), (*this).data.end(), new_rhs_data.begin(), (*this).data.begin(), std::divides<T>());
    return *this;
}

// FLQuant /= double
// FLQuantAdolc / CppAD /= adouble
template <typename T>
FLQuant_base<T>& FLQuant_base<T>::operator /= (const T& rhs){
    std::transform((*this).data.begin(), (*this).data.end(), (*this).data.begin(), std::bind2nd(std::divides<T>(),rhs)); 
    return *this;
}

// Special case of division assignment 
// Used for FLQuantAdolc / CppAD /= double
// Needs to be instanitated due to extra template class, T2
template <typename T>
template <typename T2>
FLQuant_base<T>& FLQuant_base<T>::operator /= (const T2& rhs){
    std::transform((*this).data.begin(), (*this).data.end(), (*this).data.begin(), std::bind2nd(std::divides<T>(),rhs)); 
    return *this;
}

// General division 
// FLQuant_base<T> / FLQuant_base<T>
template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator / (const FLQuant_base<T>& rhs) const{
    FLQuant_base<T> out = *this; // Copy myself
    out /= rhs;
    return out;
}

// FLQuant_base<T> / T
template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator / (const T& rhs) const{
    FLQuant_base<T> out = *this; // Copy myself
    out /= rhs;
    return out;
}

// Declared outside of class
template <typename T>
FLQuant_base<T> operator / (const FLQuant_base<double>& lhs, const FLQuant_base<T>& rhs){
    FLQuant_base<T> out(lhs);
    out /= rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator / (const FLQuant_base<T>& lhs, const FLQuant_base<double>& rhs){
    FLQuant_base<T> out = lhs;
    out /= rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator / (const T& lhs, const FLQuant_base<T>& rhs){
    FLQuant_base<T> out = rhs;
    std::vector<T> out_data = out.get_data();
    std::transform(out_data.begin(), out_data.end(), out_data.begin(), std::bind1st(std::divides<T>(),lhs)); 
    out.set_data(out_data);
    return out;
}

template <typename T>
FLQuant_base<T> operator / (const double& lhs, const FLQuant_base<T>& rhs){
    FLQuant_base<T> out = rhs;
    std::vector<T> out_data = out.get_data();
    std::transform(out_data.begin(), out_data.end(), out_data.begin(), std::bind1st(std::divides<T>(),lhs)); 
    out.set_data(out_data);
    return out;
}

FLQuant_base<double> operator / (const double& lhs, const FLQuant_base<double>& rhs){
    FLQuant_base<double> out = rhs;
    std::vector<double> out_data = out.get_data();
    std::transform(out_data.begin(), out_data.end(), out_data.begin(), std::bind1st(std::divides<double>(),lhs)); 
    out.set_data(out_data);
    return out;
}

template <typename T>
FLQuant_base<T> operator / (const FLQuant_base<T>& lhs, const double& rhs){
    FLQuant_base<T> out = lhs;
    out /= rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator / (const FLQuant_base<double>& lhs, const T& rhs){
    FLQuant_base<T> out(lhs);
    out /= rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator / (const T& lhs, const FLQuant_base<double>& rhs){
    FLQuant_base<T> out(rhs);
    std::vector<T> out_data = out.get_data();
    std::transform(out_data.begin(), out_data.end(), out_data.begin(), std::bind1st(std::divides<T>(),lhs)); 
    out.set_data(out_data);
    return out;
}

//------------------ Subtraction operators -------------------

// Subtraction self assignment
template<typename T>
FLQuant_base<T>& FLQuant_base<T>::operator -= (const FLQuant_base<T>& rhs){
    if (dim5_matcher(get_dim(), rhs.get_dim()) != 1){
        Rcpp::stop("You cannot subtract FLQuants as dimensions 1-5 do not match.");
    }
    FLQuant_base<T> new_rhs = rhs;
    if (get_niter() > rhs.get_niter()){
        // Blow up rhs 
        new_rhs = rhs.propagate_iters(get_niter() - rhs.get_niter() + 1);
    }
    if (rhs.get_niter() > get_niter()){
        // Blow up this
        *this = (*this).propagate_iters(rhs.get_niter() - get_niter() + 1);
    }
    std::transform((*this).data.begin(), (*this).data.end(), new_rhs.data.begin(), (*this).data.begin(), std::minus<T>());
    return *this;
}
// Special case of subtraction assignment 
// Instantiation below ensures that it will only compile for FLQuantAdolc / CppAD -= FLQuant
template <typename T>
template <typename T2>
FLQuant_base<T>& FLQuant_base<T>::operator -= (const FLQuant_base<T2>& rhs){
    if (dim5_matcher(get_dim(), rhs.get_dim()) != 1){
        Rcpp::stop("You cannot subtract FLQuants as dimensions 1-5 do not match.");
    }
    FLQuant_base<T2> new_rhs = rhs;
    if (get_niter() > rhs.get_niter()){
        // Blow up rhs 
        new_rhs = rhs.propagate_iters(get_niter() - rhs.get_niter() + 1);
    }
    if (rhs.get_niter() > get_niter()){
        // Blow up this
        *this = (*this).propagate_iters(rhs.get_niter() - get_niter() + 1);
    }
    std::vector<T2> new_rhs_data = new_rhs.get_data();
    std::transform((*this).data.begin(), (*this).data.end(), new_rhs_data.begin(), (*this).data.begin(), std::minus<T>());
    return *this;
}

// FLQuant -= double
// FLQuantAdolc / CppAD -= adouble
template <typename T>
FLQuant_base<T>& FLQuant_base<T>::operator -= (const T& rhs){
    std::transform((*this).data.begin(), (*this).data.end(), (*this).data.begin(), std::bind2nd(std::minus<T>(),rhs)); 
    return *this;
}

// Special case
// Used for FLQuantAdolc / CppAD -= double
// Needs to be instanitated due to extra template class, T2
template <typename T>
template <typename T2>
FLQuant_base<T>& FLQuant_base<T>::operator -= (const T2& rhs){
    std::transform((*this).data.begin(), (*this).data.end(), (*this).data.begin(), std::bind2nd(std::minus<T>(),rhs)); 
    return *this;
}

// General 
// FLQuant_base<T> - FLQuant_base<T>
template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator - (const FLQuant_base<T>& rhs) const{
    FLQuant_base<T> out = *this; // Copy myself
    out -= rhs;
    return out;
}

// FLQuant_base<T> - T
template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator - (const T& rhs) const{
    FLQuant_base<T> out = *this; // Copy myself
    out -= rhs;
    return out;
}

// Declared outside of class
template <typename T>
FLQuant_base<T> operator - (const FLQuant_base<double>& lhs, const FLQuant_base<T>& rhs){
    FLQuant_base<T> out(lhs);
    out -= rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator - (const FLQuant_base<T>& lhs, const FLQuant_base<double>& rhs){
    FLQuant_base<T> out = lhs;
    out -= rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator - (const T& lhs, const FLQuant_base<T>& rhs){
    FLQuant_base<T> out = rhs;
    std::vector<T> out_data = out.get_data();
    std::transform(out_data.begin(), out_data.end(), out_data.begin(), std::bind1st(std::minus<T>(),lhs)); 
    out.set_data(out_data);
    return out;
}

template <typename T>
FLQuant_base<T> operator - (const double& lhs, const FLQuant_base<T>& rhs){
    FLQuant_base<T> out = rhs;
    std::vector<T> out_data = out.get_data();
    std::transform(out_data.begin(), out_data.end(), out_data.begin(), std::bind1st(std::minus<T>(),lhs)); 
    out.set_data(out_data);
    return out;
}

FLQuant_base<double> operator - (const double& lhs, const FLQuant_base<double>& rhs){
    FLQuant_base<double> out = rhs;
    std::vector<double> out_data = out.get_data();
    std::transform(out_data.begin(), out_data.end(), out_data.begin(), std::bind1st(std::minus<double>(),lhs)); 
    out.set_data(out_data);
    return out;
}

template <typename T>
FLQuant_base<T> operator - (const FLQuant_base<T>& lhs, const double& rhs){
    FLQuant_base<T> out = lhs;
    out -= rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator - (const FLQuant_base<double>& lhs, const T& rhs){
    FLQuant_base<T> out(lhs);
    out -= rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator - (const T& lhs, const FLQuant_base<double>& rhs){
    FLQuant_base<T> out(rhs);
    std::vector<T> out_data = out.get_data();
    std::transform(out_data.begin(), out_data.end(), out_data.begin(), std::bind1st(std::minus<T>(),lhs)); 
    out.set_data(out_data);
    return out;
}

//------------------ Addition operators -------------------

// Addition self assignment
template<typename T>
FLQuant_base<T>& FLQuant_base<T>::operator += (const FLQuant_base<T>& rhs){
    if (dim5_matcher(get_dim(), rhs.get_dim()) != 1){
        Rcpp::stop("You cannot add FLQuants as dimensions 1-5 do not match.");
    }
    FLQuant_base<T> new_rhs = rhs;
    if (get_niter() > rhs.get_niter()){
        // Blow up rhs 
        new_rhs = rhs.propagate_iters(get_niter() - rhs.get_niter() + 1);
    }
    if (rhs.get_niter() > get_niter()){
        // Blow up this
        *this = (*this).propagate_iters(rhs.get_niter() - get_niter() + 1);
    }
    std::transform((*this).data.begin(), (*this).data.end(), new_rhs.data.begin(), (*this).data.begin(), std::plus<T>());
    return *this;
}
// Special case of addition assignment 
// Instantiation below ensures that it will only compile for FLQuantAdolc / CppAD += FLQuant
// FLQuant += FLQuantAdolc / CppAD will not compile as cannot have double = double + adouble
// Needs to be instanitated due to extra template class, T2
template <typename T>
template <typename T2>
FLQuant_base<T>& FLQuant_base<T>::operator += (const FLQuant_base<T2>& rhs){
    if (dim5_matcher(get_dim(), rhs.get_dim()) != 1){
        Rcpp::stop("You cannot add FLQuants as dimensions 1-5 do not match.");
    }
    FLQuant_base<T2> new_rhs = rhs;
    if (get_niter() > rhs.get_niter()){
        // Blow up rhs 
        new_rhs = rhs.propagate_iters(get_niter() - rhs.get_niter() + 1);
    }
    if (rhs.get_niter() > get_niter()){
        // Blow up this
        *this = (*this).propagate_iters(rhs.get_niter() - get_niter() + 1);
    }
    std::vector<T2> new_rhs_data = new_rhs.get_data();
    std::transform((*this).data.begin(), (*this).data.end(), new_rhs_data.begin(), (*this).data.begin(), std::plus<T>());
    return *this;
}

// FLQuant += double
// FLQuantAdolc / CppAD += adouble
template <typename T>
FLQuant_base<T>& FLQuant_base<T>::operator += (const T& rhs){
    std::transform((*this).data.begin(), (*this).data.end(), (*this).data.begin(), std::bind1st(std::plus<T>(),rhs)); 
    return *this;
}

// Special case of addition assignment 
// Used for FLQuantAdolc / CppAD += double
// Needs to be instanitated due to extra template class, T2
template <typename T>
template <typename T2>
FLQuant_base<T>& FLQuant_base<T>::operator += (const T2& rhs){
    std::transform((*this).data.begin(), (*this).data.end(), (*this).data.begin(), std::bind1st(std::plus<T>(),rhs)); 
    return *this;
}

// General multiplication
// FLQuant_base<T> + FLQuant_base<T>
template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator + (const FLQuant_base<T>& rhs) const{
    FLQuant_base<T> out = *this; // Copy myself
    out += rhs;
    return out;
}

// FLQuant_base<T> + T
template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator + (const T& rhs) const{
    FLQuant_base<T> out = *this; // Copy myself
    out += rhs;
    return out;
}

// Declared outside of class
template <typename T>
FLQuant_base<T> operator + (const FLQuant_base<double>& lhs, const FLQuant_base<T>& rhs){
    FLQuant_base<T> out = rhs;
    out += lhs;
    return out;

}

template <typename T>
FLQuant_base<T> operator + (const FLQuant_base<T>& lhs, const FLQuant_base<double>& rhs){
    FLQuant_base<T> out = lhs;
    out += rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator + (const T& lhs, const FLQuant_base<T>& rhs){
    FLQuant_base<T> out = rhs;
    out += lhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator + (const double& lhs, const FLQuant_base<T>& rhs){
    FLQuant_base<T> out = rhs;
    out += lhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator + (const FLQuant_base<T>& lhs, const double& rhs){
    FLQuant_base<T> out = lhs;
    out += rhs;
    return out;
}

FLQuant_base<double> operator + (const double& lhs, const FLQuant_base<double>& rhs){
    FLQuant_base<double> out = rhs;
    out += lhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator + (const FLQuant_base<double>& lhs, const T& rhs){
    FLQuant_base<T> out(lhs);
    out += rhs;
    return out;
}

template <typename T>
FLQuant_base<T> operator + (const T& lhs, const FLQuant_base<double>& rhs){
    FLQuant_base<T> out(rhs);
    out += lhs;
    return out;
}

/* Other methods */
template <typename T>
int FLQuant_base<T>::match_dims(const FLQuant_base<T>& b) const{
    Rcpp::IntegerVector dims_a =  get_dim();
    Rcpp::IntegerVector dims_b =  b.get_dim();
    return dim_matcher(dims_a, dims_b);
}

template <typename T>
template <typename T2>
int FLQuant_base<T>::match_dims(const FLQuant_base<T2>& b) const{
    Rcpp::IntegerVector dims_a =  get_dim();
    Rcpp::IntegerVector dims_b =  b.get_dim();
    return dim_matcher(dims_a, dims_b);
}



/* Other functions */
//FLQuant_base<T> log(const FLQuant_base<T>& flq);
//FLQuant_base<T> exp(const FLQuant_base<T>& flq);

template <typename T>
FLQuant_base<T> log(const FLQuant_base<T>& flq){
    FLQuant_base<T> flq_out = flq;
    std::vector<T> data = flq_out.get_data();
    for (typename std::vector<T>::iterator data_iterator = data.begin(); data_iterator != data.end(); ++data_iterator){
        (*data_iterator) = log(*data_iterator);
    }
    flq_out.set_data(data);
    return flq_out;
}

template <typename T>
FLQuant_base<T> exp(const FLQuant_base<T>& flq){
    FLQuant_base<T> flq_out = flq;
    std::vector<T> data = flq_out.get_data();
    for (typename std::vector<T>::iterator data_iterator = data.begin(); data_iterator != data.end(); ++data_iterator){
        (*data_iterator) = exp(*data_iterator);
    }
    flq_out.set_data(data);
    return flq_out;
}

int dim_matcher(const Rcpp::IntegerVector dims_a, const Rcpp::IntegerVector dims_b){
    for (int i=0; i<6; ++i){
        if (dims_a(i) != dims_b(i)){
            return -1 * (i+1); // Return negative of what dim does not match
        }
    }
    return 1; // Else all is good
}

// Only checks dim 1 - 5 - not iter
int dim5_matcher(const Rcpp::IntegerVector dims_a, const Rcpp::IntegerVector dims_b){
    for (int i=0; i<5; ++i){
        if (dims_a(i) != dims_b(i)){
            return -1 * (i+1); // Return negative of what dim does not match
        }
    }
    return 1; // Else all is good
}

/*------------- Shortcut methods ----------------*/
template <typename T>
FLQuant_base<T> year_sum(const FLQuant_base<T>& flq){
    Rprintf("In year_sum\n");
    Rcpp::IntegerVector dim = flq.get_dim();
    // Need to make an empty FLQ with the right dim
    // New constructor?
    FLQuant_base<T> sum_flq(dim[0], 1, dim[2], dim[3], dim[4], dim[5]);

    Rcpp::List dimnames = flq.get_dimnames();
    dimnames["year"] = Rcpp::CharacterVector::create("1");
    // Set dimnames
    sum_flq.set_dimnames(dimnames);
    return sum_flq;
}

template <typename T>
FLQuant_base<T> quant_sum(const FLQuant_base<T>& flq){
    Rcpp::IntegerVector dim = flq.get_dim();
    // Make an empty FLQ with the right dim
    FLQuant_base<T> sum_flq(1, dim[1], dim[2], dim[3], dim[4], dim[5]);
    //// Set dimnames and units
    Rcpp::List dimnames = flq.get_dimnames();
    dimnames[0] = Rcpp::CharacterVector::create("all");
    sum_flq.set_dimnames(dimnames);
    sum_flq.set_units(flq.get_units());
    // Old school summing - looks ugly
    // Cannot use accumulate() as not defined for adouble
    T sum = 0;
    for (int iters=1; iters <= flq.get_niter(); ++iters){
        for (int areas=1; areas <= flq.get_narea(); ++areas){
            for (int seasons=1; seasons <= flq.get_nseason(); ++seasons){
                for (int units=1; units <= flq.get_nunit(); ++units){
                    for (int years=1; years <= flq.get_nyear(); ++years){
                        sum = 0;
                        for (int quants=1; quants <= flq.get_nquant(); ++quants){
                            sum += flq(quants, years, units, seasons, areas, iters);
                        }
                        sum_flq(1, years, units, seasons, areas, iters) = sum;
    }}}}}
    // This also works - not so nested but maybe harder to decipher
    //int nquant = flq.get_nquant();
    //for (int chunk_counter=1; chunk_counter <= (flq.get_size()) / nquant; ++chunk_counter){
    //    T sum = 0;
    //    for (int quant_counter = (chunk_counter * nquant - nquant + 1); quant_counter <= (chunk_counter * nquant); ++quant_counter){
    //        sum = sum + flq(quant_counter);
    //    }
    //    sum_flq(chunk_counter) = sum;
    //}
    return sum_flq;
}

template <typename T>
FLQuant_base<T> quant_mean(const FLQuant_base<T>& flq){
    FLQuant_base<T> flq_mean = quant_sum(flq);
    // Divide by dim
    for (int iters=1; iters <= flq.get_niter(); ++iters){
        for (int areas=1; areas <= flq.get_narea(); ++areas){
            for (int seasons=1; seasons <= flq.get_nseason(); ++seasons){
                for (int units=1; units <= flq.get_nunit(); ++units){
                    for (int years=1; years <= flq.get_nyear(); ++years){
                        flq_mean(1, years, units, seasons, areas, iters) = flq_mean(1, years, units, seasons, areas, iters) / flq.get_nquant();
    }}}}}
    return flq_mean;
}


// max_quant - returns an FLQuant with size 1 in first dimension containing the maximum value of the quant dimension
// Would like to be able to use template functions max and max_element
// But we have to be careful when using conditionals and Adolc / CppAD adouble
// So we do it using the fmax() function from Adolc
// Might be possible to use template functions if we moved to CppAD
template <typename T>
FLQuant_base<T> max_quant(const FLQuant_base<T>& flq){
    Rcpp::IntegerVector dim = flq.get_dim();
    // Make an empty FLQ with the right dim
    FLQuant_base<T> max_flq(1, dim[1], dim[2], dim[3], dim[4], dim[5]);
    // Set dimnames and units
    Rcpp::List dimnames = flq.get_dimnames();
    dimnames[0] = Rcpp::CharacterVector::create("all");
    max_flq.set_dimnames(dimnames);
    max_flq.set_units(flq.get_units());
    // Old school summing - looks ugly
    T max = 0;
    for (int iters=1; iters <= flq.get_niter(); ++iters){
        for (int areas=1; areas <= flq.get_narea(); ++areas){
            for (int seasons=1; seasons <= flq.get_nseason(); ++seasons){
                for (int units=1; units <= flq.get_nunit(); ++units){
                    for (int years=1; years <= flq.get_nyear(); ++years){
                        max = flq(1, years, units, seasons, areas, iters);
                        for (int quants=1; quants <= flq.get_nquant(); ++quants){
                            //max = fmax(max, flq(quants, years, units, seasons, areas, iters));
                            max = CppAD::CondExpGe(max,flq(quants, years, units, seasons, areas, iters),max, flq(quants, years, units, seasons, areas, iters));
                        }
                        max_flq(1, years, units, seasons, areas, iters) = max;
    }}}}}
    return max_flq;
}


// This only makes sense if all the values are positive
template <typename T>
FLQuant_base<T> scale_by_max_quant(const FLQuant_base<T>& flq){
    FLQuant_base<T> max_quant_flq = max_quant(flq);
    // Copy the original FLQ to get the right dim
    FLQuant_base<T> scaled_flq = flq;
    // max_flq.set_units(flq.get_units()); // units should be set to ""
    for (int iters=1; iters <= flq.get_niter(); ++iters){
        for (int areas=1; areas <= flq.get_narea(); ++areas){
            for (int seasons=1; seasons <= flq.get_nseason(); ++seasons){
                for (int units=1; units <= flq.get_nunit(); ++units){
                    for (int years=1; years <= flq.get_nyear(); ++years){
                        for (int quants=1; quants <= flq.get_nquant(); ++quants){
                            scaled_flq(quants, years, units, seasons, areas, iters)  = flq(quants, years, units, seasons, areas, iters) / max_quant_flq(1, years, units, seasons, areas, iters);
                        }
    }}}}}
    return scaled_flq;
}

template <typename T>
std::string number_to_string (T number) {
    std::ostringstream ss;
    ss << number;
    return ss.str();
}

// Not templated - these are parameters so no need to AD them
// Caution! Assumes that order of present dimnames is the same as an FLQuant
// Dims can be missing (except the first and the iter) but the order of the
// present dimnames is: quant, year, unit, season, area, iter
// Also assumes that ONLY FLQuant dimnames are present - any other dimnames will break the code as data will be the wrong size
FLQuant FLPar_to_FLQuant(SEXP flp) {
	Rcpp::S4 flp_s4 = Rcpp::as<Rcpp::S4>(flp);
    Rcpp::NumericVector data_nv = flp_s4.slot(".Data");
    Rcpp::IntegerVector flp_dim = data_nv.attr("dim");
    if (flp_dim.size() > 6){
        Rcpp::stop("Cannot convert FLPar to FLQuant as FLPar has more than 6 dimensions\n");
    }
    Rcpp::List flp_dimnames = data_nv.attr("dimnames");
    std::vector<unsigned int> flq_dim (6,1);
    // The number of parameters is always the first dimension in the FLPar
    flq_dim[0] = flp_dim[0];
    flq_dim[5] = flp_dim[flp_dim.size()-1];
    unsigned int element = 0;
    std::vector<std::string> flp_dimnames_names = flp_dimnames.names();

    // Use initialiser lists in C++11 rather than this
    std::vector<std::string> other_flq_dimnames(4);
    other_flq_dimnames[0] = "year";
    other_flq_dimnames[1] = "unit";
    other_flq_dimnames[2] = "season";
    other_flq_dimnames[3] = "area";

    // Find the other 4 dimensions: year, unit, season, area
    // Use find() to match names - precheck in R that they exist - if not find, returns the last
    std::vector<std::string>::iterator flq_dimnames_iterator;
    for (unsigned int dimname_counter=0; dimname_counter < flp_dimnames_names.size(); ++dimname_counter){
        flq_dimnames_iterator = find(other_flq_dimnames.begin(), other_flq_dimnames.end(), flp_dimnames_names[dimname_counter]);
        if(flq_dimnames_iterator != other_flq_dimnames.end()){
            element = std::distance(other_flq_dimnames.begin(), flq_dimnames_iterator);
            flq_dim[element+1] = flp_dim[dimname_counter];
        }
        else {
            Rcpp::stop("dimname of FLPar not found in dimnames of FLQuant\n");
        }
    }


    // Rprintf("%i %i %i %i %i %i\n", flq_dim[0], flq_dim[1], flq_dim[2], flq_dim[3], flq_dim[4], flq_dim[5]);
    // Make the new FLQuant of the required size
    FLQuant flq_out(flq_dim[0], flq_dim[1], flq_dim[2], flq_dim[3], flq_dim[4], flq_dim[5]);
    // Set the data - this assumes that the order of the data in the FLPar is correct
    // i.e. that the dimnames in the FLPar are the same order as an FLQuant
    flq_out.set_data(flp_s4.slot(".Data"));
    // Don't set units (doesn't make sense with different parameters having different units)
    // or dimnames (too complicated)
    return flq_out;
}

/*----------------------------------------------------*/
/* Explicit instantiations - alternatively put all the definitions into the header file
 * This way we have more control over what types the functions work with
 */
// Explicit instantiation of class
template class FLQuant_base<double>;
template class FLQuant_base<adouble>;

// Instantiate arithmetic class methods with mixed types 
template FLQuant_base<adouble>& FLQuant_base<adouble>::operator *= (const FLQuant_base<double>& rhs);
template FLQuant_base<adouble>& FLQuant_base<adouble>::operator *= (const double& rhs);
template FLQuant_base<adouble>& FLQuant_base<adouble>::operator /= (const FLQuant_base<double>& rhs);
template FLQuant_base<adouble>& FLQuant_base<adouble>::operator /= (const double& rhs);
template FLQuant_base<adouble>& FLQuant_base<adouble>::operator -= (const FLQuant_base<double>& rhs);
template FLQuant_base<adouble>& FLQuant_base<adouble>::operator -= (const double& rhs);
template FLQuant_base<adouble>& FLQuant_base<adouble>::operator += (const FLQuant_base<double>& rhs);
template FLQuant_base<adouble>& FLQuant_base<adouble>::operator += (const double& rhs);

// Instantiate other class methods with mixed types 
template int FLQuant_base<adouble>::match_dims(const FLQuant_base<double>& b) const;
template int FLQuant_base<double>::match_dims(const FLQuant_base<adouble>& b) const;

// Explicit instantiation of extra templated artithmetic functions
// Multiplication
template FLQuant_base<adouble> operator * (const FLQuant_base<double>& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator * (const FLQuant_base<adouble>& lhs, const FLQuant_base<double>& rhs);
template FLQuant_base<adouble> operator * (const adouble& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator * (const double& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator * (const FLQuant_base<adouble>& lhs, const double& rhs);
template FLQuant_base<adouble> operator * (const FLQuant_base<double>& lhs, const adouble& rhs);
template FLQuant_base<adouble> operator * (const adouble& lhs, const FLQuant_base<double>& rhs);
// Division
template FLQuant_base<adouble> operator / (const FLQuant_base<double>& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator / (const FLQuant_base<adouble>& lhs, const FLQuant_base<double>& rhs);
template FLQuant_base<adouble> operator / (const adouble& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator / (const double& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator / (const FLQuant_base<adouble>& lhs, const double& rhs);
template FLQuant_base<adouble> operator / (const FLQuant_base<double>& lhs, const adouble& rhs);
template FLQuant_base<adouble> operator / (const adouble& lhs, const FLQuant_base<double>& rhs);
// Subtraction
template FLQuant_base<adouble> operator - (const FLQuant_base<double>& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator - (const FLQuant_base<adouble>& lhs, const FLQuant_base<double>& rhs);
template FLQuant_base<adouble> operator - (const adouble& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator - (const double& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator - (const FLQuant_base<adouble>& lhs, const double& rhs);
template FLQuant_base<adouble> operator - (const FLQuant_base<double>& lhs, const adouble& rhs);
template FLQuant_base<adouble> operator - (const adouble& lhs, const FLQuant_base<double>& rhs);
// Addition
template FLQuant_base<adouble> operator + (const FLQuant_base<double>& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator + (const FLQuant_base<adouble>& lhs, const FLQuant_base<double>& rhs);
template FLQuant_base<adouble> operator + (const adouble& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator + (const double& lhs, const FLQuant_base<adouble>& rhs);
template FLQuant_base<adouble> operator + (const FLQuant_base<adouble>& lhs, const double& rhs);
template FLQuant_base<adouble> operator + (const FLQuant_base<double>& lhs, const adouble& rhs);
template FLQuant_base<adouble> operator + (const adouble& lhs, const FLQuant_base<double>& rhs);

// Explicit instantiation of other functions
template FLQuant_base<double> log(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> log(const FLQuant_base<adouble>& flq);
template FLQuant_base<double> exp(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> exp(const FLQuant_base<adouble>& flq);
template FLQuant_base<double> year_sum(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> year_sum(const FLQuant_base<adouble>& flq);

template FLQuant_base<double> quant_sum(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> quant_sum(const FLQuant_base<adouble>& flq);
template FLQuant_base<double> quant_mean(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> quant_mean(const FLQuant_base<adouble>& flq);

template FLQuant_base<double> max_quant(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> max_quant(const FLQuant_base<adouble>& flq);

template FLQuant_base<double> scale_by_max_quant(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> scale_by_max_quant(const FLQuant_base<adouble>& flq);

