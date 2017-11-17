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
    discards_ratio_flq = FLQuant_base<T>();
    landings_wt_flq = FLQuant();
    discards_wt_flq = FLQuant();
    catch_sel_flq = FLQuant();
    price_flq = FLQuant();
    catch_q_flq = FLQuant();
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
    discards_ratio_flq = discards_n_flq / (discards_n_flq + landings_n_flq + 1e-16);
    landings_wt_flq = flc_s4.slot("landings.wt");
    discards_wt_flq = flc_s4.slot("discards.wt");
    catch_sel_flq = flc_s4.slot("catch.sel");
    price_flq = flc_s4.slot("price");
    catch_q_flq = FLPar_to_FLQuant(flc_s4.slot("catch.q"));
    // Store the original
    catch_q_orig = flc_s4.slot("catch.q");
}

// Copy constructor - else members can be pointed at by multiple instances
template <typename T>
FLCatch_base<T>::FLCatch_base(const FLCatch_base<T>& FLCatch_source){
    name = FLCatch_source.name;
    desc = FLCatch_source.desc;
    range = Rcpp::clone<Rcpp::NumericVector>(FLCatch_source.range);
    landings_n_flq = FLCatch_source.landings_n_flq;
    discards_n_flq = FLCatch_source.discards_n_flq;
    discards_ratio_flq = FLCatch_source.discards_ratio_flq;
    landings_wt_flq = FLCatch_source.landings_wt_flq;
    discards_wt_flq = FLCatch_source.discards_wt_flq;
    catch_sel_flq = FLCatch_source.catch_sel_flq;
    price_flq = FLCatch_source.price_flq;
    catch_q_flq = FLCatch_source.catch_q_flq;
    catch_q_orig = FLCatch_source.catch_q_orig;
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
        discards_ratio_flq = FLCatch_source.discards_ratio_flq;
        landings_wt_flq = FLCatch_source.landings_wt_flq;
        discards_wt_flq = FLCatch_source.discards_wt_flq;
        catch_sel_flq = FLCatch_source.catch_sel_flq;
        price_flq = FLCatch_source.price_flq;
        catch_q_flq = FLCatch_source.catch_q_flq;
        catch_q_orig = FLCatch_source.catch_q_orig;
	}
	return *this;
}

/* Intrusive 'wrap' */
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
    flc_s4.slot("catch.q") = catch_q_orig;
    return Rcpp::wrap(flc_s4);
}

// Accessors
// Get only
template <typename T>
FLQuant_base<T> FLCatch_base<T>::landings_n() const {
    return landings_n_flq;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::landings_n(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return landings_n_flq(indices_min, indices_max);
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::discards_n() const {
    return discards_n_flq;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::discards_n(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return discards_n_flq(indices_min, indices_max);
}

template <typename T>
FLQuant FLCatch_base<T>::landings_wt() const {
    return landings_wt_flq;
}

template <typename T>
FLQuant FLCatch_base<T>::landings_wt(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return landings_wt_flq(indices_min, indices_max);
}

template <typename T>
FLQuant FLCatch_base<T>::discards_wt() const {
    return discards_wt_flq;
}

template <typename T>
FLQuant FLCatch_base<T>::discards_wt(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return discards_wt_flq(indices_min, indices_max);
}

template <typename T>
FLQuant FLCatch_base<T>::catch_sel() const {
    return catch_sel_flq;
}

template <typename T>
FLQuant FLCatch_base<T>::catch_sel(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return catch_sel_flq(indices_min, indices_max);
}

template <typename T>
FLQuant FLCatch_base<T>::price() const {
    return price_flq;
}

template <typename T>
FLQuant FLCatch_base<T>::price(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return price_flq(indices_min, indices_max);
}


template <typename T>
FLQuant_base<T> FLCatch_base<T>::discards_ratio() const {
    return discards_ratio_flq;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::discards_ratio(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    return discards_ratio_flq(indices_min, indices_max);
}

template <typename T>
FLQuant FLCatch_base<T>::catch_q_params() const {
    return catch_q_flq;
}

/*! \brief Get the catchability parameters over a subset of dims
 *
 * Subset the catchability parameters over quant - iteration dimensions.
 * The parameters are stored in the first dimension.
 * 
 * \param indices_min minimum indices for dimensions quant - iter (length 6)
 * \param indices_max maximum indices for dimensions quant - iter (length 6)
 */
template <typename T>
FLQuant FLCatch_base<T>::catch_q_params(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    // Check dims are length 6
    if ((indices_min.size() != 6) | (indices_max.size() != 6)){
        Rcpp::stop("In FLCatch catch_q_params subsetter. Indices not of length 6\n");
    }
    // Checking first dimension (parameter) range
    std::vector<unsigned int> qdims = catch_q_flq.get_dim();
    if ((indices_min[0] < 1) | (indices_max[0] > qdims[0])){
        Rcpp::stop("In FLCatch catch_q_params subsetter. Outside first dimension range. Possibly asking for too many catchability parameters.\n");
    }
    // Indices_min must be between 1 and max for dims 2:6
    std::vector<unsigned int> ndims = landings_n_flq.get_dim();
    for (unsigned int i = 1; i < indices_min.size(); ++i){
        if ((indices_min[i] < 1) | (indices_min[i] > ndims[i]) | (indices_max[i] > ndims[i])){
            Rcpp::stop("In FLCatch catch_q_params subsetter. indices_min and indices_max dims 2-6 must be within range of FLCatch FLQuant members.\n");
        }
    }
    // Build new FLQuant to store catch_q_params in
    std::vector<unsigned int> new_dims {indices_max[0] - indices_min[0] + 1, indices_max[1] - indices_min[1] + 1, indices_max[2] - indices_min[2] + 1, indices_max[3] - indices_min[3] + 1, indices_max[4] - indices_min[4] + 1, indices_max[5] - indices_min[5] + 1};
    FLQuant q_params(new_dims[0], new_dims[1], new_dims[2], new_dims[3], new_dims[4], new_dims[5]);
    std::vector<unsigned int> q_params_indices{1,1,1,1,1,1};
    for (unsigned int quant_count = 1; quant_count <= new_dims[0]; ++quant_count){
        q_params_indices[0] = quant_count + indices_min[0] - 1;
        for (unsigned int year_count = 1; year_count <= new_dims[1]; ++year_count){
            if (qdims[1] > 1){
                q_params_indices[1] = year_count + indices_min[1] - 1;
            }
            for (unsigned int unit_count = 1; unit_count <= new_dims[2]; ++unit_count){
                if (qdims[2] > 1){
                    q_params_indices[2] = unit_count + indices_min[2] - 1;
                }
                for (unsigned int season_count = 1; season_count <= new_dims[3]; ++season_count){
                    if (qdims[3] > 1){
                        q_params_indices[3] = season_count + indices_min[3] - 1;
                    }
                    for (unsigned int area_count = 1; area_count <= new_dims[4]; ++area_count){
                        if (qdims[4] > 1){
                            q_params_indices[4] = area_count + indices_min[4] - 1;
                        }
                        for (unsigned int iter_count = 1; iter_count <= new_dims[5]; ++iter_count){
                            if (qdims[5] > 1){
                                q_params_indices[5] = iter_count + indices_min[5] - 1;
                            }
                            q_params(quant_count, year_count, unit_count, season_count, area_count, iter_count) = catch_q_flq(q_params_indices);
    }}}}}}
    return q_params;
}

// catch_q_flq is a little different as it checks if unit / season / etc are > 1
// parameters are stored in the first dimension
template <typename T>
std::vector<double> FLCatch_base<T>::catch_q_params(unsigned int year, unsigned int unit, unsigned int season, unsigned int area, unsigned int iter) const {
    std::vector<unsigned int> dims = catch_q_flq.get_dim();
    std::vector<double> q_out (dims[0],0.0);
    // Sort out dims - if years > no years in the catch_q_flq object (i.e. catch_q_flq are not disaggregated by time etc.) just pick the first 
    // The real checking should be done in the R side
    if (year > dims[1]){
        year = 1;
    }
    if (unit > dims[2]){
        unit = 1;
    }
    if (season > dims[3]){
        season = 1;
    }
    if (area > dims[4]){
        area = 1;
    }
    if (iter > dims[5]){
        iter = 1;
    }
    for (unsigned int i = 1; i <= dims[0]; ++i){
        q_out[i-1] = catch_q_flq(i,year,unit,season,area,iter);
    }
    return q_out;
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

template <typename T>
FLQuant& FLCatch_base<T>::catch_q_params() {
    return catch_q_flq;
}

// Methods
template <typename T>
FLQuant_base<T> FLCatch_base<T>::revenue() const {
    // Call to revenue has indices only 5 long - no age
    std::vector<unsigned int> indices_min {1,1,1,1,1};
    std::vector<unsigned int> indices_max = landings_wt_flq.get_dim();
    indices_max.erase(indices_max.begin());
    return revenue(indices_min, indices_max);
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::revenue(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    // Revenue is not age structured
    if((indices_min.size() != 5) | (indices_max.size() != 5)){
        Rcpp::stop("In FLCatch revenue indices subsetter. indices_min and max must be of length 5\n");
    }
    // price * landings.n * landings.wt are all age structured so need to add age dim to indices_min and max
    std::vector<unsigned int> quant_indices_min = indices_min;
    quant_indices_min.insert(quant_indices_min.begin(), 1);
    std::vector<unsigned int> quant_indices_max = indices_max;
    std::vector<unsigned int> dims = landings_wt_flq.get_dim();
    quant_indices_max.insert(quant_indices_max.begin(), dims[0]);
    // Summed over all ages and requested unit range
    FLQuant_base<T> revenue = quant_sum(price(quant_indices_min, quant_indices_max) * landings_n(quant_indices_min, quant_indices_max) * landings_wt(quant_indices_min, quant_indices_max));
    return revenue;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::landings() const {
    std::vector<unsigned int> indices_min {1,1,1,1,1};
    std::vector<unsigned int> indices_max = landings_wt_flq.get_dim();
    indices_max.erase(indices_max.begin());
    return landings(indices_min, indices_max);
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::landings(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if((indices_min.size() != 5) | (indices_max.size() != 5)){
        Rcpp::stop("In FLCatch landings indices subsetter. indices_min and max must be of length 5\n");
    }
    std::vector<unsigned int> quant_indices_min = indices_min;
    quant_indices_min.insert(quant_indices_min.begin(), 1);
    std::vector<unsigned int> quant_indices_max = indices_max;
    std::vector<unsigned int> dims = landings_wt_flq.get_dim();
    quant_indices_max.insert(quant_indices_max.begin(), dims[0]);
    FLQuant_base<T> landings = quant_sum(landings_n(quant_indices_min, quant_indices_max) * landings_wt(quant_indices_min, quant_indices_max));
    return landings;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::discards() const {
    std::vector<unsigned int> indices_min {1,1,1,1,1};
    std::vector<unsigned int> indices_max = discards_wt_flq.get_dim();
    indices_max.erase(indices_max.begin());
    return discards(indices_min, indices_max);
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::discards(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if((indices_min.size() != 5) | (indices_max.size() != 5)){
        Rcpp::stop("In FLCatch discards indices subsetter. indices_min and max must be of length 5\n");
    }
    std::vector<unsigned int> quant_indices_min = indices_min;
    quant_indices_min.insert(quant_indices_min.begin(), 1);
    std::vector<unsigned int> quant_indices_max = indices_max;
    std::vector<unsigned int> dims = discards_wt_flq.get_dim();
    quant_indices_max.insert(quant_indices_max.begin(), dims[0]);
    FLQuant_base<T> discards = quant_sum(discards_n(quant_indices_min, quant_indices_max) * discards_wt(quant_indices_min, quant_indices_max));
    return discards;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::catch_n() const {
    std::vector<unsigned int> indices_min {1,1,1,1,1,1};
    std::vector<unsigned int> indices_max = landings_wt_flq.get_dim();
    return catch_n(indices_min, indices_max);
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::catch_n(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    FLQuant_base<T> catch_n = discards_n(indices_min, indices_max) + landings_n(indices_min, indices_max);
    return catch_n;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::catches() const {
    std::vector<unsigned int> indices_min {1,1,1,1,1};
    std::vector<unsigned int> indices_max = discards_wt_flq.get_dim();
    indices_max.erase(indices_max.begin());
    return catches(indices_min, indices_max);
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::catches(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if((indices_min.size() != 5) | (indices_max.size() != 5)){
        Rcpp::stop("In FLCatch catches indices subsetter. indices_min and max must be of length 5\n");
    }
    std::vector<unsigned int> quant_indices_min = indices_min;
    quant_indices_min.insert(quant_indices_min.begin(), 1);
    std::vector<unsigned int> quant_indices_max = indices_max;
    std::vector<unsigned int> dims = discards_wt_flq.get_dim();
    quant_indices_max.insert(quant_indices_max.begin(), dims[0]);
    FLQuant_base<T> catches = quant_sum((discards_n(quant_indices_min, quant_indices_max) * discards_wt(quant_indices_min, quant_indices_max))  + (landings_n(quant_indices_min, quant_indices_max) * landings_wt(quant_indices_min, quant_indices_max)));
    return catches;
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::catch_wt() const {
    std::vector<unsigned int> indices_min {1,1,1,1,1,1};
    std::vector<unsigned int> indices_max = landings_wt_flq.get_dim();
    return catch_wt(indices_min, indices_max);
}

template <typename T>
FLQuant_base<T> FLCatch_base<T>::catch_wt(const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    FLQuant_base<T> catch_wt = ((landings_wt(indices_min, indices_max) * landings_n(indices_min, indices_max)) + (discards_wt(indices_min, indices_max) * discards_n(indices_min, indices_max))) / (landings_n(indices_min, indices_max) + discards_n(indices_min, indices_max));
    return catch_wt;
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

// Explicit instantiation of classes
template class FLCatch_base<double>;
template class FLCatch_base<adouble>;

/*------------------------------------------------------------*/
// FLCatches class


// Default constructor
// Does nothing
template <typename T>
FLCatches_base<T>::FLCatches_base(){
}

// Constructor from a list of SEXP S4 FLCatch objects
// Used as intrusive 'as'
template <typename T>
FLCatches_base<T>::FLCatches_base(SEXP flcs_sexp){
    Rcpp::S4 flcs_s4 = Rcpp::as<Rcpp::S4>(flcs_sexp);
    desc = Rcpp::as<std::string>(flcs_s4.slot("desc"));
    names = flcs_s4.slot("names");
    Rcpp::List catch_list = Rcpp::as<Rcpp::List>(flcs_s4.slot(".Data"));
    auto no_catches = catch_list.size();
    catches.reserve(no_catches);
    for (auto flcatch : catch_list){
        catches.push_back(flcatch);
    }
}

// Intrusive wrap
// List is unamed
template<typename T>
FLCatches_base<T>::operator SEXP() const{
    Rcpp::S4 flcs_s4("FLCatches");
    Rcpp::List list_out;
    for (auto flcatch : catches){
        list_out.push_back(flcatch);
    }
    flcs_s4.slot(".Data") = list_out;
    flcs_s4.slot("desc") = desc;
    flcs_s4.slot("names") = names;
    return flcs_s4;
}

// Constructor from an FLCatch
template <typename T> 
FLCatches_base<T>::FLCatches_base(const FLCatch_base<T>& flc){
    catches.push_back(flc);
}

// Copy constructor - else 'data' can be pointed at by multiple instances
template<typename T>
FLCatches_base<T>::FLCatches_base(const FLCatches_base<T>& FLCatches_source){
	catches  = FLCatches_source.catches; // std::vector always does deep copy
    desc = FLCatches_source.desc;
    names = Rcpp::clone<Rcpp::CharacterVector>(FLCatches_source.names);
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
template<typename T>
FLCatches_base<T>& FLCatches_base<T>::operator = (const FLCatches_base<T>& FLCatches_source){
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
void FLCatches_base<T>::operator() (const FLCatch_base<T>& flc){
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

// begin and end methods for for_range and iterators
template <typename T>
typename FLCatches_base<T>::iterator FLCatches_base<T>::begin(){
    return catches.begin();
}

template <typename T>
typename FLCatches_base<T>::iterator FLCatches_base<T>::end(){
    return catches.end();
}

template <typename T>
typename FLCatches_base<T>::const_iterator FLCatches_base<T>::begin() const {
    return catches.begin();
}

template <typename T>
typename FLCatches_base<T>::const_iterator FLCatches_base<T>::end() const {
    return catches.end();
}

// Explicit instantiation of classes
template class FLCatches_base<double>;
template class FLCatches_base<adouble>;


