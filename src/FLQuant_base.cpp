/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include <time.h> // included for some basic profiling
#include "../inst/include/FLQuant_base.h"

/*! \brief Default constructor 
 *
 * Creates an empty FLQuant with no dims, dimnames or units
 */
template <typename T>
FLQuant_base<T>::FLQuant_base(){
    data = std::vector<T>();
	  units = std::string(); 
    dim = std::vector<unsigned int>();
    dimnames = Rcpp::List();
}

// Need to add check that the SEXP is an FLQuant
/*! \brief Generic SEXP constructor used as intrusive as
 *
 * Creates an FLQuant from one passed in directly from R.
 * \param flq_sexp An SEXP object that is an R based FLQuant
 */
template<typename T>
FLQuant_base<T>::FLQuant_base(SEXP flq_sexp){
	Rcpp::S4 flq_s4(flq_sexp);
    // Pulling out the .Data slot.
    // Takes time but cannot get round this extraction 
    // We need it to be a NumericVector (so we can get dim and dimnames)
    Rcpp::NumericVector data_nv = flq_s4.slot(".Data"); 
    // Even doing it the old school way takes same time
    // SEXP Quant = R_do_slot(flq_sexp, Rf_install(".Data"));
    // Other FLQuant bits
    dim = Rcpp::as<std::vector<unsigned int>>(data_nv.attr("dim"));
	dimnames = data_nv.attr("dimnames");
	units = Rcpp::as<std::string>(flq_s4.slot("units"));
    // Sort out data - need to copy across as maybe AD
    // Initialise data to the correct size
    data.reserve(std::accumulate(dim.begin(), dim.end(), 1, std::multiplies<unsigned int>()));
    data.insert(data.begin(), data_nv.begin(), data_nv.end());
}

/*! \brief Creates an FLQuant of a certain size filled with 0
 *
 * Note that units and dimnames have not been set.
 * \param nquant The size of the first dimension.
 * \param nyear The number of years.
 * \param nunit The number of units.
 * \param nseason The number of seasons.
 * \param narea The number of areas.
 * \param niter The number of iterations.
 * \param value Value to fill the new FLQuant with.
 */
template <typename T>
//FLQuant_base<T>::FLQuant_base(const unsigned int nquant, const unsigned int nyear, const unsigned int nunit, const unsigned int nseason, const unsigned int narea, const unsigned int niter, const T value){
FLQuant_base<T>::FLQuant_base(const unsigned int nquant, const unsigned int nyear, const unsigned int nunit, const unsigned int nseason, const unsigned int narea, const unsigned int niter, const T value) : FLQuant_base<T>({nquant, nyear, nunit, nseason, narea, niter}, value) {
    // Nothing to do
}

/*! \brief Creates an FLQuant of a certain size filled with 0
 *
 * Note that units and dimnames have not been set.
 * \param nquant The size of the first dimension.
 * \param value Value to fill the new FLQuant with.
 */
template <typename T>
//FLQuant_base<T>::FLQuant_base(const std::vector<unsigned int> dims, const T value) : FLQuant_base<T>(dims[0], dims[1], dims[2], dims[3], dims[4], dims[5], value) { // Call other constructor
FLQuant_base<T>::FLQuant_base(const std::vector<unsigned int> dims, const T value) { // Call other constructor
    if (dims.size() != 6){
        Rcpp::stop("In FLQuant integer vector constructor. Vector not of length 6.\n");
    }
	units = std::string(); // Empty string - just ""
    dim = dims;
    data = std::vector<T>(dims[0] * dims[1] * dims[2] * dims[3] * dims[4] * dims[5], value);
    // How to fill dimnames up appropriately?
    // Just of the right size at the moment.
    // Could use ::create to pass in actual characters, but then do we want just 1:nage, 1:nyear etc?
    dimnames = Rcpp::List::create(
      Rcpp::Named("quant", Rcpp::CharacterVector(dims[0])),
      Rcpp::Named("year", Rcpp::CharacterVector(dims[1])),
      Rcpp::Named("unit", Rcpp::CharacterVector(dims[2])),
      Rcpp::Named("season", Rcpp::CharacterVector(dims[3])),
      Rcpp::Named("area", Rcpp::CharacterVector(dims[4])),
      Rcpp::Named("iter", Rcpp::CharacterVector(dims[5])));
}

/*! \brief Used as generic intrusive wrap to return FLQuant to R
 *
 * This method is specialised for double and adouble.
 * This particular implementation should not actually be called.
 * If it is, it returns an integer and gives a warning
 */
template<typename T>
FLQuant_base<T>::operator SEXP() const{
    Rprintf("Wrapping generic FLQuant_base. Probably not what you wanted.\n");
    int x = 0;
    return Rcpp::wrap(x);
}

// Specialise the wrap for an FLQuant_base<double>
/*! \brief Specialised intrusive wrap for to return a double FLQuant to R
 *
 * It is necessary to specialise for the double FLQuant as when we have an adouble FLQuant we have pull out the real value. 
 */
template<>
FLQuant_base<double>::operator SEXP() const{
    Rcpp::S4 flq_s4("FLQuant");
    // Make and fill the NumericVector that will be the 'data' slot 
    Rcpp::NumericVector data_nv(data.size());
    // Filling this up takes a long time
    // No NV.insert but can use transform and a lambda (sort of)
    std::transform(data.begin(), data.end(), data_nv.begin(),
        [](double x) { return x; } );
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
/*! \brief Specialised intrusive wrap for to return an adouble FLQuant to R
 *
 * It is necessary to specialise as we have to pull out the real value from the adouble value.
 */
template<>
FLQuant_base<adouble>::operator SEXP() const{
    //Rprintf("Specialised wrapping FLQuant_base<adouble>\n");
    Rcpp::S4 flq_s4("FLQuant");
    // Make and fill the NumericVector that will be the 'data' slot 
    Rcpp::NumericVector data_nv(data.size());
    // Lambda to get Value - nice
    std::transform(data.begin(), data.end(), data_nv.begin(),
        [](adouble x) { return Value(x); } );
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
	data  = FLQuant_source.data; // std::vector always does deep copy
	units = FLQuant_source.units; // std::string always does deep copy
	dim  = FLQuant_source.dim; // std::vector always does deep copy
    dimnames = Rcpp::clone<Rcpp::List>(FLQuant_source.dimnames);
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
template<typename T>
FLQuant_base<T>& FLQuant_base<T>::operator = (const FLQuant_base<T>& FLQuant_source){
    //Rprintf("In FLQuant_base<T> assignment operator\n");
	if (this != &FLQuant_source){
        data  = FLQuant_source.data; // std::vector always does deep copy
        units = FLQuant_source.units; // std::string always does deep copy
        dim = FLQuant_source.dim; // std::string always does deep copy
        //dim = Rcpp::clone<Rcpp::IntegerVector>(FLQuant_source.dim);
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
    units = FLQuant_source.get_units(); // std::string always does deep copy
    dim = FLQuant_source.get_dim();
    dimnames = FLQuant_source.get_dimnames(); 
    std::vector<double> source_data = FLQuant_source.get_data();
    data.insert(data.begin(), source_data.begin(), source_data.end());
}

// Specialise the FLQuant_base<T>(FLQuant_base<T2>) constructor 
// Make an FLQuant from an FLQuantAD
template <>
template <>
FLQuant_base<double>::FLQuant_base(const FLQuant_base<adouble>& FLQuant_source){
    units = FLQuant_source.get_units(); // std::string always does deep copy
    dim = FLQuant_source.get_dim();
    dimnames = FLQuant_source.get_dimnames(); 
    std::vector<adouble> source_data = FLQuant_source.get_data();
    std::vector<double> new_data(FLQuant_source.get_size());
    std::transform(FLQuant_source.begin(), FLQuant_source.end(), new_data.begin(), 
            [] (adouble x) {return Value(x);});
    data = new_data;
}

//------------------ begin and end ---------------------------------
// These are used for for_range and iterators

template <typename T>
typename FLQuant_base<T>::iterator FLQuant_base<T>::begin(){
    return data.begin();
}

template <typename T>
typename FLQuant_base<T>::iterator FLQuant_base<T>::end(){
    return data.end();
}

template <typename T>
typename FLQuant_base<T>::const_iterator FLQuant_base<T>::begin() const {
    return data.begin();
}

template <typename T>
typename FLQuant_base<T>::const_iterator FLQuant_base<T>::end() const {
    return data.end();
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
std::vector<unsigned int> FLQuant_base<T>::get_dim() const{
    return dim;
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
unsigned int FLQuant_base<T>::get_nquant() const{
	return dim[0];
}

template <typename T>
unsigned int FLQuant_base<T>::get_nyear() const{
	return dim[1];
}

template <typename T>
unsigned int FLQuant_base<T>::get_nunit() const{
	return dim[2];
}

template <typename T>
unsigned int FLQuant_base<T>::get_nseason() const{
	return dim[3];
}

template <typename T>
unsigned int FLQuant_base<T>::get_narea() const{
	return dim[4];
}

template <typename T>
unsigned int FLQuant_base<T>::get_niter() const{
	return dim[5];
}

// Note that elements start at 1 NOT 0!
template <typename T>
unsigned int FLQuant_base<T>::get_data_element(const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area, unsigned int iter) const{
    // Check that quant etc > 0
    if ((quant <= 0) || (year <= 0) || (unit <= 0) || (season <= 0) || (area <= 0) || (iter <= 0)){
            Rcpp::stop("In FLQuant accessor. quant etc must be > 0\n");
    }
    std::vector<unsigned int> dim = get_dim();
    if ((quant > dim[0]) || (year > dim[1]) || (unit > dim[2]) || (season > dim[3]) || (area > dim[4])){
            Rcpp::stop("Trying to access element outside of quant, year, unit, season or area dim range.");
    }
    // If only 1 iter and trying to get n iter, set iter to 1
    if ((iter > 1) && (dim[5] == 1)){
        //get_data_element(quant, year, unit, season, area, 1);
        iter = 1;
    }
    if ((iter > dim[5]) && (dim[5] > 1)){
        Rcpp::stop("In get_data_element: trying to access iter > niter\n");
    } 
	unsigned int element = (dim[4] * dim[3] * dim[2] * dim[1] * dim[0] * (iter - 1)) +
			(dim[3] * dim[2] * dim[1] * dim[0] * (area - 1)) +
			(dim[2] * dim[1] * dim[0] * (season - 1)) +
			(dim[1] * dim[0] * (unit - 1)) +
			(dim[0] * (year - 1)) +
			(quant - 1); 
	return element;
}

/*! \brief Get the first age in the dimnames as an integer
 *
 */
template <typename T>
int FLQuant_base<T>::get_first_age() const{
    //std::vector<std::string> age_names = Rcpp::as<std::vector<std::string> >(n_flq.get_dimnames()[0]);
    //std::string first_age_str = age_names[0];
    std::string test = (Rcpp::as<std::vector<std::string> >(get_dimnames()[0]))[0];
    int first_age = std::stoi(test);
    return first_age;
}

// Get only data accessor - single element - starts at 1
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

// Subset - max_iter is not const as can be changed due to 1 or N
template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator () (const unsigned int quant_min, const unsigned int quant_max, const unsigned int year_min, const unsigned int year_max, const unsigned int unit_min, const unsigned int unit_max, const unsigned int season_min, const unsigned int season_max, const unsigned int area_min, const unsigned int area_max, const unsigned int iter_min, unsigned int iter_max) const {
    // Check ranges
    if ((quant_min < 1) || (year_min < 1)|| (unit_min < 1)|| (season_min < 1)|| (area_min < 1)|| (iter_min < 1)) {
        Rcpp::stop("In FLQuant subsetter: requested min dimensions are less than 1.\n");
    }
    // Check max >= min
    if ((quant_max < quant_min) || (year_max < year_min) || (unit_max < unit_min) || (season_max < season_min) || (area_max < area_min) || (iter_max < iter_min)){
        Rcpp::stop("In FLQuant subsetter: min dim > max\n");
    }

    // Check asked for indices are not greater than allowed - iter is a special case
    if ((quant_max > get_nquant()) || (year_max > get_nyear()) || (unit_max > get_nunit()) || (season_max > get_nseason()) || (area_max > get_narea())){
        Rcpp::stop("In FLQuant subsetter: requested subset dimensions are outside of FLQuant bounds.\n");
    }
    // Iterations are a special case: Allowed 1 or N. If FLQ has 1 iter and you ask for more, you get the 1. As R.
    if (iter_max > get_niter()){
        // If only 1 iter in FLQuant, and asking for subset 1 to N, return 1 to 1
        if ((iter_min==1) & (get_niter()==1)){
            iter_max = 1;
        }
        else {
            Rcpp::stop("In FLQuant subsetter: Max iter > Niters. Only allowed if FLQuant has 1 iter. Even then subset can only be 1:1 or 1:N iters.\n");
        }
    }

    // Using brace initialiser
    std::vector<unsigned int> new_dim{quant_max - quant_min + 1, year_max - year_min + 1, unit_max - unit_min + 1, season_max - season_min + 1, area_max - area_min + 1, iter_max - iter_min + 1};

    FLQuant_base<T> out(new_dim[0], new_dim[1], new_dim[2], new_dim[3], new_dim[4], new_dim[5]);
    out.set_units(get_units());
    for (unsigned int quant_count = 1; quant_count <= new_dim[0]; ++quant_count){
        for (unsigned int year_count = 1; year_count <= new_dim[1]; ++year_count){
            for (unsigned int unit_count = 1; unit_count <= new_dim[2]; ++unit_count){
                for (unsigned int season_count = 1; season_count <= new_dim[3]; ++season_count){
                    for (unsigned int area_count = 1; area_count <= new_dim[4]; ++area_count){
                        for (unsigned int iter_count = 1; iter_count <= new_dim[5]; ++iter_count){
                            unsigned int element_orig = get_data_element(quant_count + quant_min - 1, year_count + year_min - 1, unit_count + unit_min - 1, season_count + season_min - 1, area_count + area_min - 1, iter_count + iter_min - 1);
                            out(quant_count, year_count, unit_count, season_count, area_count, iter_count) = data[element_orig];
    }}}}}}

    // Sorting out dimnames - not a speed issue
    std::vector<unsigned int> min_dim{quant_min, year_min, unit_min, season_min, area_min, iter_min};
    Rcpp::List old_dimnames = get_dimnames();
    Rcpp::List new_dimnames = get_dimnames();
    std::vector<std::string> temp_old_dimname;
    std::vector<std::string> temp_new_dimname;

    for (int dim_counter = 0; dim_counter < 6; ++dim_counter){ // counts over dims
        temp_old_dimname = Rcpp::as<std::vector<std::string> >(old_dimnames[dim_counter]);
        temp_new_dimname.resize(new_dim[dim_counter]); 
        for (unsigned int counter = 0; counter < new_dim[dim_counter]; ++counter){ // counts along dims
            temp_new_dimname[counter] = temp_old_dimname[counter + min_dim[dim_counter] - 1];
        }
        new_dimnames[dim_counter] = temp_new_dimname;
    }
    out.set_dimnames(new_dimnames);
    return out;
}

/*! \brief Subsets an FLQuant
 * Just calls the bigger subsetter but has a neater interface
 *
 * \param indices_min A vector of length 6 with the minimum indices of the 6 dimensions
 * \param indices_max A vector of length 6 with the maximum indices of the 6 dimensions
 */
template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator () (const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if ((indices_min.size() != 6) | (indices_max.size() != 6)){
        Rcpp::stop("In neat FLQuant subsetter. Size of indices_min or max not equal to 6\n");
    }
    return (*this)(indices_min[0], indices_max[0], indices_min[1], indices_max[1], indices_min[2], indices_max[2], indices_min[3], indices_max[3], indices_min[4], indices_max[4], indices_min[5], indices_max[5]);
}

template <typename T>
FLQuant_base<T> FLQuant_base<T>::operator () (const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area) const {
    FLQuant_base<T> out = (*this)(quant, quant, year, year, unit, unit, season, season, area, area, 1, get_niter());
    return out;
}

//------------- Setting methods ----------------

template <typename T>
void FLQuant_base<T>::set_data(const std::vector<T>& data_in){
    //Rcpp::IntegerVector dim = get_dim();
    std::vector<unsigned int> dim = get_dim();
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
    unsigned int dim_length = 0;
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
        iter_dimnames[iter_counter] = std::to_string(iter_counter+1);
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

template <typename T>
void FLQuant_base<T>::fill(const T value){
    std::fill(data.begin(), data.end(), value);
    return;
}

template <>
template <>
void FLQuant_base<adouble>::fill(const double value){
    adouble value_ad = value;
    fill(value_ad);
}

// Can pass an AD into a D and vice versa even though the Ts are different
// It implicitly calls the constructors from FLQuantD to FLQuantAD and vice versa
/*! \brief Insert a smaller FLQuant into a bigger one
 *
 * It implicitly calls the constructors from FLQuantD to FLQuantAD and vice versa so can pass AD into D and vice versa
 * \param flq The FLQuant to be inserted.
 * \param indices_min Vector of length 6 determining where to start inserting.
 * \param indices_max Vector of length 6 determining where to finish inserting.
 */
template<typename T>
void FLQuant_base<T>::insert(const FLQuant_base<T> flq, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    if ((indices_min.size() != 6) | (indices_max.size() != 6)){
        Rcpp::stop("In neat FLQuant subsetter. Size of indices_min or max not equal to 6\n");
    }
    auto dim = get_dim();
    auto flq_dim = flq.get_dim();
    std::vector<unsigned int> dim_in(6);
    std::transform(indices_max.begin(), indices_max.end(), indices_min.begin(), dim_in.begin(), [] (unsigned int x, unsigned int y) {return x - y + 1;});
    for (int i=0; i<6; ++i){
        if (dim[i] < flq_dim[i]){
            Rcpp::stop("In FLQuant insert(). Inserted FLQuant is larger than destination FLQuant\n");
        }
        if (flq_dim[i] != dim_in[i]){
            Rcpp::stop("In FLQuant insert(). Dim of inserted FLQuant does not match indices_min and indices_max arguments\n");
        }
    }
    // Insert
    for (unsigned int qcount=indices_min[0]; qcount <= indices_max[0]; ++qcount){
        for (unsigned int ycount=indices_min[1]; ycount <= indices_max[1]; ++ycount){
            for (unsigned int ucount=indices_min[2]; ucount <= indices_max[2]; ++ucount){
                for (unsigned int scount=indices_min[3]; scount <= indices_max[3]; ++scount){
                    for (unsigned int acount=indices_min[4]; acount <= indices_max[4]; ++acount){
                        for (unsigned int icount=indices_min[5]; icount <= indices_max[5]; ++icount){
                            (*this)(qcount, ycount, ucount, scount, acount, icount) = flq(qcount-indices_min[0]+1, ycount-indices_min[1]+1, ucount-indices_min[2]+1, scount-indices_min[3]+1, acount-indices_min[4]+1, icount-indices_min[5]+1);
    }}}}}}
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
//    Rcpp::IntegerVector dims_a =  get_dim();
//    Rcpp::IntegerVector dims_b =  b.get_dim();
    std::vector<unsigned int> dims_a =  get_dim();
    std::vector<unsigned int> dims_b =  b.get_dim();
    return dim_matcher(dims_a, dims_b);
}

template <typename T>
template <typename T2>
int FLQuant_base<T>::match_dims(const FLQuant_base<T2>& b) const{
    //Rcpp::IntegerVector dims_a =  get_dim();
    //Rcpp::IntegerVector dims_b =  b.get_dim();
    std::vector<unsigned int> dims_a =  get_dim();
    std::vector<unsigned int> dims_b =  b.get_dim();
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

//int dim_matcher(const Rcpp::IntegerVector dims_a, const Rcpp::IntegerVector dims_b){
int dim_matcher(const std::vector<unsigned int> dims_a, const std::vector<unsigned int> dims_b){
    for (int i=0; i<6; ++i){
        if (dims_a[i] != dims_b[i]){
            return -1 * (i+1); // Return negative of what dim does not match
        }
    }
    return 1; // Else all is good
}

// Only checks dim 1 - 5 - not iter
//int dim5_matcher(const Rcpp::IntegerVector dims_a, const Rcpp::IntegerVector dims_b){
int dim5_matcher(const std::vector<unsigned int> dims_a, const std::vector<unsigned int> dims_b){
    for (int i=0; i<5; ++i){
        if (dims_a[i] != dims_b[i]){
            return -1 * (i+1); // Return negative of what dim does not match
        }
    }
    return 1; // Else all is good
}

/*------------- pow method ----------------*/
template <typename T>
FLQuant_base<T> pow(const FLQuant_base<T>& flq, const T power){
    FLQuant_base<T> out(flq);
    std::transform(flq.begin(), flq.end(), out.begin(),
            [power](T x) {return pow(x, power);});
    return  out;
}

FLQuant_base<adouble> pow(const FLQuant_base<adouble>& flq, const double power){
    FLQuant_base<adouble> out(flq);
    std::transform(flq.begin(), flq.end(), out.begin(),
            [power](adouble x) {return pow(x, power);});
    return  out;
}

FLQuant_base<adouble> pow(const FLQuant_base<double>& flq, const adouble power){
    FLQuant_base<adouble> out(flq);
    std::transform(flq.begin(), flq.end(), out.begin(),
            [power](double x) {return pow(x, power);});
    return  out;
}

/*------------- Sweep methods ----------------*/

/*! \brief Performs a binary function operation on 2 FLQuants of possibly different size
 *
 * The dims of the 2 FLQuant objects must be 1 or n (n can be different for each dim).
 * For the dim of 1, the values are recycled.
 * The output FLQuant has dims that are the maximum of the input FLQuants.
 * For example, FLQ1 (5,6,1,1,1,1) * FLQ2 (1,6,1,1,1,10)
 * will output an FLQ (5,6,1,1,1,10).
 * It's similar to using the %*%, %+% etc. operators in FLR (which is like sweep). 
 * The type of the output FLQuant depends on the types of the input FLQuants.
 * adoubles propagate. For example, AD * D = D.
 * \param flq1 FLQuant of type double or adouble
 * \param flq2 FLQuant of type double or adouble
 */
template <typename T1, typename T2, typename T3>
FLQuant_base<typename T1::result_type> sweep_flq(const FLQuant_base<T2>& flq1, const FLQuant_base<T3>& flq2, T1 func){
    auto dim1 = flq1.get_dim();
    auto dim2 = flq2.get_dim();
    std::vector<unsigned int> dim_out(6);
    // Go over each dim vector, check if 1 or n, make dim of output Q
    for (int i = 0; i <= 5; ++i){
        if((dim1[i] == dim2[i]) | (dim1[i] == 1) | (dim2[i] == 1)){
            dim_out[i] = std::max(dim1[i], dim2[i]);
        }
        else {
            Rcpp::stop("In FLQuant Sweep. Size of each dim must be 1 or n.\n");
        }
    }
    // Make the return FLQ of type that is output from the operator
    FLQuant_base<typename T1::result_type> out(dim_out);
    // Need to do this element by element - nasty
    for (unsigned int qcount=1; qcount <= dim_out[0]; ++qcount){
        for (unsigned int ycount=1; ycount <= dim_out[1]; ++ycount){
            for (unsigned int ucount=1; ucount <= dim_out[2]; ++ucount){
                for (unsigned int scount=1; scount <= dim_out[3]; ++scount){
                    for (unsigned int acount=1; acount <= dim_out[4]; ++acount){
                        for (unsigned int icount=1; icount <= dim_out[5]; ++icount){
                            // Apply the func
                            out(qcount, ycount, ucount, scount, acount, icount) = func(
                                flq1(std::min(qcount, dim1[0]), std::min(ycount, dim1[1]),
                                        std::min(ucount, dim1[2]), std::min(scount, dim1[3]),
                                        std::min(acount, dim1[4]), std::min(icount, dim1[5])),
                                flq2(std::min(qcount, dim2[0]), std::min(ycount, dim2[1]),
                                        std::min(ucount, dim2[2]), std::min(scount, dim2[3]),
                                        std::min(acount, dim2[4]), std::min(icount, dim2[5])));
    }}}}}}
    return out;
}

// Multiplication
// Templated for D * D, AD * AD
template <typename T>
FLQuant_base<T> sweep_mult(const FLQuant_base<T>& flq1,const FLQuant_base<T>& flq2){
    return sweep_flq(flq1, flq2, std::multiplies<T>());
}

FLQuant_base<adouble> sweep_mult(const FLQuant_base<adouble>& flq1,const FLQuant_base<double>& flq2){
    return sweep_flq(flq1, flq2, std::multiplies<adouble>());
}

FLQuant_base<adouble> sweep_mult(const FLQuant_base<double>& flq1,const FLQuant_base<adouble>& flq2){
    return sweep_flq(flq1, flq2, std::multiplies<adouble>());
}

// Division
template <typename T>
FLQuant_base<T> sweep_div(const FLQuant_base<T>& flq1, const FLQuant_base<T>& flq2){
    return sweep_flq(flq1, flq2, std::divides<T>());
}

FLQuant_base<adouble> sweep_div(const FLQuant_base<adouble>& flq1, const FLQuant_base<double>& flq2){
    return sweep_flq(flq1, flq2, std::divides<adouble>());
}

FLQuant_base<adouble> sweep_div(const FLQuant_base<double>& flq1, const FLQuant_base<adouble>& flq2){
    return sweep_flq(flq1, flq2, std::divides<adouble>());
}

// Addition
template <typename T>
FLQuant_base<T> sweep_plus(const FLQuant_base<T>& flq1, const FLQuant_base<T>& flq2){
    return sweep_flq(flq1, flq2, std::plus<T>());
}

FLQuant_base<adouble> sweep_plus(const FLQuant_base<adouble>& flq1, const FLQuant_base<double>& flq2){
    return sweep_flq(flq1, flq2, std::plus<adouble>());
}

FLQuant_base<adouble> sweep_plus(const FLQuant_base<double>& flq1, const FLQuant_base<adouble>& flq2){
    return sweep_flq(flq1, flq2, std::plus<adouble>());
}

// Subtraction
template <typename T>
FLQuant_base<T> sweep_minus(const FLQuant_base<T>& flq1, const FLQuant_base<T>& flq2){
    return sweep_flq(flq1, flq2, std::minus<T>());
}

FLQuant_base<adouble> sweep_minus(const FLQuant_base<adouble>& flq1, const FLQuant_base<double>& flq2){
    return sweep_flq(flq1, flq2, std::minus<adouble>());
}

FLQuant_base<adouble> sweep_minus(const FLQuant_base<double>& flq1, const FLQuant_base<adouble>& flq2){
    return sweep_flq(flq1, flq2, std::minus<adouble>());
}

/*------------- Shortcut methods ----------------*/
template <typename T>
FLQuant_base<T> year_sum(const FLQuant_base<T>& flq){
    //Rcpp::IntegerVector dim = flq.get_dim();
    std::vector<unsigned int> dim = flq.get_dim();
    // Need to make an empty FLQ with the right dim
    FLQuant_base<T> sum_flq(dim[0], 1, dim[2], dim[3], dim[4], dim[5]);
    Rcpp::List dimnames = flq.get_dimnames();
    dimnames["year"] = Rcpp::CharacterVector::create("1");
    // Set dimnames
    sum_flq.set_dimnames(dimnames);
    // Old school summing - looks ugly
    T sum = 0;
    for (unsigned int iters=1; iters <= flq.get_niter(); ++iters){
        for (unsigned int areas=1; areas <= flq.get_narea(); ++areas){
            for (unsigned int seasons=1; seasons <= flq.get_nseason(); ++seasons){
                for (unsigned int units=1; units <= flq.get_nunit(); ++units){
                    for (unsigned int quants=1; quants <= flq.get_nquant(); ++quants){
                        sum = 0;
                        for (unsigned int years=1; years <= flq.get_nyear(); ++years){
                            sum += flq(quants, years, units, seasons, areas, iters);
                        }
                        sum_flq(quants, 1, units, seasons, areas, iters) = sum;
    }}}}}
    return sum_flq;
}

template <typename T>
FLQuant_base<T> quant_sum(const FLQuant_base<T>& flq){
    //Rcpp::IntegerVector dim = flq.get_dim();
    std::vector<unsigned int> dim = flq.get_dim();
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
    for (unsigned int iters=1; iters <= flq.get_niter(); ++iters){
        for (unsigned int areas=1; areas <= flq.get_narea(); ++areas){
            for (unsigned int seasons=1; seasons <= flq.get_nseason(); ++seasons){
                for (unsigned int units=1; units <= flq.get_nunit(); ++units){
                    for (unsigned int years=1; years <= flq.get_nyear(); ++years){
                        sum = 0;
                        for (unsigned int quants=1; quants <= flq.get_nquant(); ++quants){
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
FLQuant_base<T> unit_sum(const FLQuant_base<T>& flq){
    std::vector<unsigned int> dim = flq.get_dim();
    // Make an empty FLQ with the right dim
    FLQuant_base<T> sum_flq(dim[0], dim[1], 1, dim[3], dim[4], dim[5]);
    //// Set dimnames and units
    Rcpp::List dimnames = flq.get_dimnames();
    dimnames["unit"] = Rcpp::CharacterVector::create("unique");
    sum_flq.set_dimnames(dimnames);
    sum_flq.set_units(flq.get_units());
    // Old school summing - looks ugly
    // Cannot use accumulate() as not defined for adouble
    T sum = 0;
    for (unsigned int iters=1; iters <= flq.get_niter(); ++iters){
        for (unsigned int areas=1; areas <= flq.get_narea(); ++areas){
            for (unsigned int seasons=1; seasons <= flq.get_nseason(); ++seasons){
                for (unsigned int years=1; years <= flq.get_nyear(); ++years){
                    for (unsigned int quants=1; quants <= flq.get_nquant(); ++quants){
                        sum = 0;
                        for (unsigned int units=1; units <= flq.get_nunit(); ++units){
                            sum += flq(quants, years, units, seasons, areas, iters);
                        }
                        sum_flq(quants, years, 1, seasons, areas, iters) = sum;
    }}}}}
    return sum_flq;
}

template <typename T>
FLQuant_base<T> quant_mean(const FLQuant_base<T>& flq){
    FLQuant_base<T> flq_mean = quant_sum(flq);
    // Divide by dim
    for (unsigned int iters=1; iters <= flq.get_niter(); ++iters){
        for (unsigned int areas=1; areas <= flq.get_narea(); ++areas){
            for (unsigned int seasons=1; seasons <= flq.get_nseason(); ++seasons){
                for (unsigned int units=1; units <= flq.get_nunit(); ++units){
                    for (unsigned int years=1; years <= flq.get_nyear(); ++years){
                        flq_mean(1, years, units, seasons, areas, iters) = flq_mean(1, years, units, seasons, areas, iters) / flq.get_nquant();
    }}}}}
    return flq_mean;
}

template <typename T>
FLQuant_base<T> year_mean(const FLQuant_base<T>& flq){
    FLQuant_base<T> flq_mean = year_sum(flq);
    // Divide by dim
    for (unsigned int iters=1; iters <= flq.get_niter(); ++iters){
        for (unsigned int areas=1; areas <= flq.get_narea(); ++areas){
            for (unsigned int seasons=1; seasons <= flq.get_nseason(); ++seasons){
                for (unsigned int units=1; units <= flq.get_nunit(); ++units){
                    for (unsigned int quants=1; quants <= flq.get_nquant(); ++quants){
                        flq_mean(quants, 1, units, seasons, areas, iters) = flq_mean(quants, 1, units, seasons, areas, iters) / flq.get_nyear();
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
    //Rcpp::IntegerVector dim = flq.get_dim();
    std::vector<unsigned int> dim = flq.get_dim();
    // Make an empty FLQ with the right dim
    FLQuant_base<T> max_flq(1, dim[1], dim[2], dim[3], dim[4], dim[5]);
    // Set dimnames and units
    Rcpp::List dimnames = flq.get_dimnames();
    dimnames[0] = Rcpp::CharacterVector::create("all");
    max_flq.set_dimnames(dimnames);
    max_flq.set_units(flq.get_units());
    // Old school summing - looks ugly
    T max = 0;
    for (unsigned int iters=1; iters <= flq.get_niter(); ++iters){
        for (unsigned int areas=1; areas <= flq.get_narea(); ++areas){
            for (unsigned int seasons=1; seasons <= flq.get_nseason(); ++seasons){
                for (unsigned int units=1; units <= flq.get_nunit(); ++units){
                    for (unsigned int years=1; years <= flq.get_nyear(); ++years){
                        max = flq(1, years, units, seasons, areas, iters);
                        for (unsigned int quants=1; quants <= flq.get_nquant(); ++quants){
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
    for (unsigned int iters=1; iters <= flq.get_niter(); ++iters){
        for (unsigned int areas=1; areas <= flq.get_narea(); ++areas){
            for (unsigned int seasons=1; seasons <= flq.get_nseason(); ++seasons){
                for (unsigned int units=1; units <= flq.get_nunit(); ++units){
                    for (unsigned int years=1; years <= flq.get_nyear(); ++years){
                        for (unsigned int quants=1; quants <= flq.get_nquant(); ++quants){
                            scaled_flq(quants, years, units, seasons, areas, iters)  = flq(quants, years, units, seasons, areas, iters) / max_quant_flq(1, years, units, seasons, areas, iters);
                        }
    }}}}}
    return scaled_flq;
}

// Not templated - these are parameters so no need to AD them
// Caution! Assumes that order of present dimnames is the same as an FLQuant
// Dims can be missing (except the first and the iter) but the order of the
// present dimnames is: quant, year, unit, season, area, iter
// Also assumes that ONLY FLQuant dimnames are present - any other dimnames will break the code as data will be the wrong size
FLQuant FLPar_to_FLQuant(SEXP flp) {
	Rcpp::S4 flp_s4 = Rcpp::as<Rcpp::S4>(flp);
    Rcpp::NumericVector data_nv = flp_s4.slot(".Data");
    //Rcpp::IntegerVector flp_dim = data_nv.attr("dim");
    std::vector<unsigned int> flp_dim = data_nv.attr("dim");
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
    for (unsigned int dimname_counter=1; dimname_counter < (flp_dimnames_names.size() - 1); ++dimname_counter){
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

// Converting timestep to year and season and vice versa
// These are based on INDICES, not characters
template <typename T>
void year_season_to_timestep(const unsigned int year, const unsigned int season, const FLQuant_base<T>& flq, unsigned int& timestep){
    year_season_to_timestep(year, season, flq.get_nseason(), timestep);
}

template <typename T>
void timestep_to_year_season(const unsigned int timestep, const FLQuant_base<T>& flq, unsigned int& year, unsigned int& season){
    timestep_to_year_season(timestep, flq.get_nseason(), year, season);
}

void year_season_to_timestep(const unsigned int year, const unsigned int season, const unsigned int nseason, unsigned int& timestep){
    timestep = (year-1) * nseason + season;
}

void timestep_to_year_season(const unsigned int timestep, const unsigned int nseason, unsigned int& year, unsigned int& season){
    year =  (timestep-1) / nseason + 1; // integer divide - takes the floor
    season = (timestep-1) % nseason + 1;
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

// Sweep operations
template FLQuant_base<adouble> sweep_mult(const FLQuant_base<adouble>& flq1, const FLQuant_base<adouble>& flq2);
template FLQuant_base<double> sweep_mult(const FLQuant_base<double>& flq1, const FLQuant_base<double>& flq2);
template FLQuant_base<adouble> sweep_div(const FLQuant_base<adouble>& flq1, const FLQuant_base<adouble>& flq2);
template FLQuant_base<double> sweep_div(const FLQuant_base<double>& flq1, const FLQuant_base<double>& flq2);
template FLQuant_base<adouble> sweep_plus(const FLQuant_base<adouble>& flq1, const FLQuant_base<adouble>& flq2);
template FLQuant_base<double> sweep_plus(const FLQuant_base<double>& flq1, const FLQuant_base<double>& flq2);
template FLQuant_base<adouble> sweep_minus(const FLQuant_base<adouble>& flq1, const FLQuant_base<adouble>& flq2);
template FLQuant_base<double> sweep_minus(const FLQuant_base<double>& flq1, const FLQuant_base<double>& flq2);

// Pow
template FLQuant_base<double> pow(const FLQuant_base<double>& flq, const double power);
template FLQuant_base<adouble> pow(const FLQuant_base<adouble>& flq, const adouble power);

// Explicit instantiation of other functions
template FLQuant_base<double> log(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> log(const FLQuant_base<adouble>& flq);
template FLQuant_base<double> exp(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> exp(const FLQuant_base<adouble>& flq);

template FLQuant_base<double> year_sum(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> year_sum(const FLQuant_base<adouble>& flq);
template FLQuant_base<double> year_mean(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> year_mean(const FLQuant_base<adouble>& flq);

template FLQuant_base<double> quant_sum(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> quant_sum(const FLQuant_base<adouble>& flq);
template FLQuant_base<double> quant_mean(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> quant_mean(const FLQuant_base<adouble>& flq);

template FLQuant_base<double> unit_sum(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> unit_sum(const FLQuant_base<adouble>& flq);

template FLQuant_base<double> max_quant(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> max_quant(const FLQuant_base<adouble>& flq);

template FLQuant_base<double> scale_by_max_quant(const FLQuant_base<double>& flq);
template FLQuant_base<adouble> scale_by_max_quant(const FLQuant_base<adouble>& flq);

template void year_season_to_timestep(const unsigned int year, const unsigned int season, const FLQuant_base<double>& flq, unsigned int& timestep);
template void year_season_to_timestep(const unsigned int year, const unsigned int season, const FLQuant_base<adouble>& flq, unsigned int& timestep);

template void timestep_to_year_season(const unsigned int timestep, const FLQuant_base<double>& flq, unsigned int& year, unsigned int& season);
template void timestep_to_year_season(const unsigned int timestep, const FLQuant_base<adouble>& flq, unsigned int& year, unsigned int& season);

