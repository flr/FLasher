/* 
 * Copyright 2013 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/fwdControl.h"

// maps the quant type character string to the enumerated types
void fwdControl::init_target_map(){
    // Fill up the map
    target_map["f"] = target_fbar;
    target_map["fbar"] = target_fbar;
    target_map["catch"] = target_catch;
    target_map["landings"] = target_landings;
    target_map["discards"] = target_discards;
    target_map["srp"] = target_srp;
    target_map["ssb"] = target_ssb; // Does the same thing as SRP at the moment
    target_map["biomass"] = target_biomass;
    target_map["effort"] = target_effort;
    target_map["revenue"] = target_revenue;
    return;
}

// Empty constructor
fwdControl::fwdControl(){
    target = Rcpp::DataFrame();
    target_iters = Rcpp::NumericVector();
    FCB = Rcpp::IntegerMatrix();
}

// Constructor used as intrinsic 'as'
fwdControl::fwdControl(SEXP fwd_control_sexp){
	Rcpp::S4 fwd_control_s4 = Rcpp::as<Rcpp::S4>(fwd_control_sexp);
    target_iters = fwd_control_s4.slot("iters");
    target = fwd_control_s4.slot("target");
    FCB = Rcpp::as<Rcpp::IntegerMatrix>(fwd_control_s4.slot("FCB"));
    init_target_map();
}

// Intrinsic 'wrap' - does not return FCB as not part of class
fwdControl::operator SEXP() const{
    Rcpp::S4 fwd_control_s4("fwdControl");
    fwd_control_s4.slot("target") = target;
    fwd_control_s4.slot("iters") = target_iters;
    return Rcpp::wrap(fwd_control_s4);
}

// Copy constructor - else 'data' can be pointed at by multiple instances
fwdControl::fwdControl(const fwdControl& fwdControl_source){
    target = Rcpp::clone<Rcpp::DataFrame>(fwdControl_source.target); // Need to clone for a deep copy
    target_iters = Rcpp::clone<Rcpp::NumericVector>(fwdControl_source.target_iters); // Need to clone 
    target_map = fwdControl_source.target_map;
    FCB = Rcpp::clone<Rcpp::IntegerMatrix>(fwdControl_source.FCB); // Need to clone
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
fwdControl& fwdControl::operator = (const fwdControl& fwdControl_source){
	if (this != &fwdControl_source){
        target = Rcpp::clone<Rcpp::DataFrame>(fwdControl_source.target); // Need to clone for a deep copy
        target_iters = Rcpp::clone<Rcpp::NumericVector>(fwdControl_source.target_iters); // Need to clone 
        target_map = fwdControl_source.target_map;
        FCB = Rcpp::clone<Rcpp::IntegerMatrix>(fwdControl_source.FCB); // Need to clone
	}
	return *this;
}

Rcpp::DataFrame fwdControl::get_target() const{
    return target;
}

// Returns the number of targets in the control object
// Target numbers should be contiguous starting from 1
unsigned int fwdControl::get_ntarget() const{
    // Check that the target column exists 
    std::vector<std::string> col_names = target.attr("names");
    if (std::find(col_names.begin(), col_names.end(), "order") == col_names.end()){
        Rcpp::stop("In fwdControl::get_ntarget - no order column in control dataframe\n");
    }
    std::vector<unsigned int> target_no = target["order"];
    auto minmax = std::minmax_element(target_no.begin(), target_no.end());
    auto ntarget = (*minmax.second - *minmax.first + 1);
    return ntarget;
}

// Returns the number of iterations in the target_iters member object
unsigned int fwdControl::get_niter() const{
    Rcpp::IntegerVector dim = target_iters.attr("dim");
    return dim[2];
}

// Returns the age range - just the values in target
// Not indices - these will be actual ages that need to be matched to the dimnames in the Biol
std::vector<unsigned int> fwdControl::get_age_range(const unsigned int target_no, const unsigned int sim_target_no) const{
    unsigned int row = get_target_row(target_no, sim_target_no);
    std::vector<unsigned int> min_age = target["minAge"];
    std::vector<unsigned int> max_age = target["maxAge"];
    std::vector<unsigned int> age_range(2);
    age_range[0] = min_age[row];
    age_range[1] = max_age[row];
    return age_range;
} 

/*! \brief Get the number of simultaneous targets associated with a target number
 *
 * A target can have multiple simultaneous targets. For example, if you have 2 FLFishery objects aiming to hit 2 catches at the same time. 
 * This method returns the number of simultanous targets associated with a target.
 * \param target_no References the target column in the control dataframe.
 */
unsigned int fwdControl::get_nsim_target(unsigned int target_no) const{
    // Check that the order column exists 
    std::vector<std::string> col_names = target.attr("names");
    if (std::find(col_names.begin(), col_names.end(), "order") == col_names.end()){
        Rcpp::stop("In fwdControl::get_nsim_target - no order column in control dataframe\n");
    }
    std::vector<unsigned int> targets = target["order"];
    // Sort them
    std::sort(targets.begin(), targets.end());
    // [&] means capture variable, means we can get target_no
    // Get the position after the last target_no
    auto it_greater = std::find_if(targets.begin(), targets.end(), [&] (const unsigned int& x) {return x > target_no;});
    // Get the position of the first target_no
    auto it_value = std::find(targets.begin(), targets.end(), target_no);
    if (it_value == targets.end()){
        Rcpp::stop("In fwdControl::get_nsim_target. target_no not found in order column\n");
    }
    return it_greater - it_value;
}

/*! \name get row(s) of the control dataframe given the target number
 */
//@{
/*! \brief Given the target_no return the corresponding row numbers in the control object.
 *
 * Returned row number starts at 0
 * \param target_no References the target column in the control dataframe.
 */
std::vector<unsigned int> fwdControl::get_target_row(unsigned int target_no) const {
    // Check that the target column exists 
    std::vector<std::string> col_names = target.attr("names");
    if (std::find(col_names.begin(), col_names.end(), "order") == col_names.end()){
        Rcpp::stop("In fwdControl::get_target_row - no order column in control dataframe\n");
    }
    std::vector<unsigned int> targets = target["order"];
    unsigned int nsim_target = get_nsim_target(target_no);
    std::vector<unsigned int> rows(nsim_target);
    auto current_target = targets.begin();
    for (unsigned int target_count = 0; target_count < nsim_target; ++target_count){
        current_target = std::find(current_target, targets.end(), target_no);
        if (current_target == targets.end()){
            Rcpp::stop("In fwdControl::get_target_row. order row not found\n");
        }
        rows[target_count] = current_target - targets.begin();
        current_target++;
    }
    return rows;
}

/*! \brief Given the target_no and the sim_target_no, return the corresponding row number in the control object.
 *
 * Returned row number starts at 0
 * \param target_no References the target column in the control dataframe.
 * \param sim_target_no Number of the simultaneous target within the target set.
 */
unsigned int fwdControl::get_target_row(unsigned int target_no, unsigned int sim_target_no) const{
    std::vector<unsigned int> rows = get_target_row(target_no);
    if (sim_target_no > rows.size()){
            Rcpp::stop("In fwdControl::get_target_row. sim_target_no out of range\n");
    }
    return rows[sim_target_no-1];
}
//@}

/*! \name Get the target value from the control object
 */
//@{
/*! \brief Get the target value from the control object for all simultaneous targets.
 *
 * Get all iterations, from all simultaneous targets, in the control frame.
 * Indexing starts at 1.
 * Returns a single vector of length niter * nsimtarget
 * with simtarget1 taking indices 0:(niter-1), simtarget2 taking indices niter:(2*niter-1) and so on.
 * Returns the value, min or max values depending on the 'col' argument.
 * \param target_no References the target column in the control dataframe.
 * \param col 1 for min, 2 for value, 3 for max column
 */
std::vector<double> fwdControl::get_target_value(const int target_no, const int col) const{
    auto nsim_target = get_nsim_target(target_no);
    std::vector<double> out;
    for (unsigned int sim_target_count = 1; sim_target_count <= nsim_target; ++sim_target_count){
        std::vector<double> sim_target_value = get_target_value(target_no, sim_target_count, col);
        out.insert(out.end(), sim_target_value.begin(), sim_target_value.end());
    }
    return out;
}

/*! \brief Get the target value from the control object for one simultaneous target.
 *
 * Get all iterations, from a single simultaneous target, in the control frame.
 * Indexing starts at 1.
 * Returns a single vector of length niter 
 * Returns the value, min or max values depending on the 'col' argument.
 * \param target_no References the target column in the control dataframe.
 * \param sim_target_no The number of the simultaneous target.
 * \param col 1 for min, 2 for value, 3 for max column.
 */
std::vector<double> fwdControl::get_target_value(const int target_no, const int sim_target_no, const int col) const{
    Rcpp::IntegerVector dim = target_iters.attr("dim");
    std::vector<double> out(dim[2]); // 3rd dim is the iterations
    unsigned int target_element = 0;
    unsigned int target_row = 0;
    target_row = get_target_row(target_no, sim_target_no); // target row is posn. on first dim
    for (unsigned int iter_count = 0; iter_count < dim[2]; ++iter_count){
        target_element = (dim[1] * dim[0] * (iter_count)) + (dim[0] * (col - 1)) + target_row; 
        out[iter_count] = target_iters(target_element);
    }
    return out;
}
//@}

/*! \name Get the value(s) of an integer column in the control dataframe
 */
//@{
/*! \brief Subset an integer column in the control object by the target_no
 *
 * Rcpp::IntegerVector is used as return type as this preserves any NAs passed from R.
 * Converting to std::vector<unsigned int> does not work with is_na() (but it does compile).
 * Can be used on non-Integer columns (no check is made) but who knows what the result will be?!?!
 * \param target_no The target number as given by the target column in the control dataframe.
 * \param col The name of the integer column in the control dataframe.
 */
Rcpp::IntegerVector fwdControl::get_target_int_col(const int target_no, const std::string col) const {
    // Check that column exists in data.frame
    std::vector<std::string> names = target.attr("names");
    auto it = std::find(names.begin(), names.end(), col);
    if (it == names.end()){
        Rcpp::stop("In fwdControl::get_target_int_col. Column name '%s' not found,\n", col);
    }
    Rcpp::IntegerVector all = target[col];
    std::vector<unsigned int> rows = get_target_row(target_no);
    Rcpp::IntegerVector subset(rows.size());
    for (auto i=0; i < subset.size(); i++){
        subset[i] = all[rows[i]];
    }
    return subset;
}

/*! \brief Pull out a value of an integer column in the control object by the target and simultaneous target nos
 *
 * The returned unsigned int is still able to handle NA values as it is pulled from an Rcpp::IntegerVector.
 * Can be used on non-Integer columns (no check is made) but who knows what the result will be?!?!
 * \param target_no References the target column in the control dataframe.
 * \param sim_target_no The simultaneous target number
 */
unsigned int fwdControl::get_target_int_col(const int target_no, const int sim_target_no, const std::string col) const {
    Rcpp::IntegerVector values = get_target_int_col(target_no, col);
    if (sim_target_no > values.size()){
        Rcpp::stop("In fwdControl::get_target_int_col. sim_target_no is too big\n");
    }
    return values[sim_target_no-1];
}
//@}

/*! \name Get the value(s) of a numeric column in the control dataframe
 */
//@{
/*! \brief Subset an numeric column in the control object by the target_no
 *
 * Rcpp::NumericVector is used as return type as this preserves any NAs passed from R.
 * Converting to std::vector<unsigned int> does not work with is_na() (but it does compile).
 * Can be used on non-Numeric columns (no check is made) but who knows what the result will be?!?!
 * \param target_no References the target column in the control dataframe.
 */
Rcpp::NumericVector fwdControl::get_target_num_col(const int target_no, const std::string col) const {
    // Check that column exists in data.frame
    std::vector<std::string> names = target.attr("names");
    auto it = std::find(names.begin(), names.end(), col);
    if (it == names.end()){
        Rcpp::stop("In fwdControl::get_target_num_col. Column name not found,\n");
    }
    Rcpp::NumericVector all = target[col];
    std::vector<unsigned int> rows = get_target_row(target_no);
    Rcpp::NumericVector subset(rows.size());
    for (auto i=0; i < subset.size(); i++){
        subset[i] = all[rows[i]];
    }
    return subset;
}

/*! \brief Pull out a value of a numeric column in the control object by the target and simultaneous target nos
 *
 * The returned double is still able to handle NA values as it is pulled from an Rcpp::NumericVector.
 * Can be used on non-Numeric columns (no check is made) but who knows what the result will be?!?!
 * \param target_no References the target column in the control dataframe.
 * \param sim_target_no The simultaneous target number
 */
double fwdControl::get_target_num_col(const int target_no, const int sim_target_no, const std::string col) const {
    Rcpp::NumericVector values = get_target_num_col(target_no, col);
    if (sim_target_no > values.size()){
        Rcpp::stop("In fwdControl::get_target_int_col. sim_target_no is too big\n");
    }
    return values[sim_target_no-1];
}
//@}

/*! \brief Get the target quantity from the control object
 *
 * 
 * \param target_no References the target column in the control dataframe.
 * \param sim_target_no
 */
std::string fwdControl::get_target_quantity(const int target_no, const int sim_target_no, const bool relative) const{
    auto row = get_target_row(target_no, sim_target_no);
    Rcpp::CharacterVector quantities;
    if (relative){
        quantities = target["relQuant"];
    }
    else {
        quantities = target["quant"];
    }
    return Rcpp::as<std::string>(quantities[row]);
}

fwdControlTargetType fwdControl::get_target_type(const int target_no, const int sim_target_no, const bool relative) const{
    std::string quantity = get_target_quantity(target_no, sim_target_no, relative);
    return get_target_type(quantity);
}

fwdControlTargetType fwdControl::get_target_type(const std::string quantity) const{
    target_map_type::const_iterator type_pair_found = target_map.find(quantity);
    if (type_pair_found == target_map.end()){
        Rcpp::stop("Unable to find target quantity in fwdControl target_map\n");
    }
    return type_pair_found->second;
}

/*--------------------- FCB accessors ------------------------------*/

/*! \brief Returns the FCB table
 *
 * Returns the FCB table
 */
Rcpp::IntegerMatrix fwdControl::get_FCB() const{
    return FCB;
}

// Given the Biol no, what fishery / catch fish it?
Rcpp::IntegerMatrix fwdControl::get_FC(const int biol_no) const{
    std::vector<int> rows;
    for (unsigned int row_counter=0; row_counter < FCB.nrow(); ++row_counter){
        if(FCB(row_counter,2) == biol_no){
            rows.push_back(row_counter);
        }
    }
    Rcpp::IntegerMatrix FC(rows.size(),2);
    for (unsigned int row_counter=0; row_counter < rows.size(); ++ row_counter){
        FC(row_counter,0) = FCB(rows[row_counter],0);
        FC(row_counter,1) = FCB(rows[row_counter],1);
    }
    return FC;
}

// Given the Fishery / catch no, what Biols do they fish?
std::vector<int> fwdControl::get_B(const int fishery_no, const int catch_no) const{
    std::vector<int> rows;
    for (unsigned int row_counter=0; row_counter < FCB.nrow(); ++row_counter){
        if(FCB(row_counter,0) == fishery_no){
            if(FCB(row_counter,1) == catch_no){
                rows.push_back(row_counter);
            }
        }
    }
    std::vector<int> B(rows.size(),0.0);
    for (unsigned int row_counter=0; row_counter < rows.size(); ++ row_counter){
        B[row_counter] = FCB(rows[row_counter],2);
    }
    return B;
}

/*! \brief Get the number of rows in the FCB matrix
 *
 * Just the length of the first dimension.
 */
unsigned int fwdControl::get_FCB_nrow() const{
    return FCB.nrow();
}

/*! \brief Get the row number of the FCB matrix
 *
 * Row number starts at 0.
 *
 * \param fishery_no The position of the fishery in the fishery list.
 * \param catch_no The position of the catch in the catches list.
 * \param biol_no The position of the biol in the biols list.
 */
unsigned int fwdControl::get_FCB_row_no(const unsigned int fishery_no, const unsigned int catch_no, const unsigned int biol_no) const{
    unsigned int nrow = FCB.nrow();
    unsigned int row_counter = 0;
    bool success = false;
    while ((row_counter <= nrow) & (success == false)){
        if((FCB(row_counter,0) == fishery_no) & (FCB(row_counter,1) == catch_no) & (FCB(row_counter,2) == biol_no)){
            success = true;
        }
        ++row_counter;
    }
    if (success == false){
        Rcpp::stop("In fwdControl::get_FCB_row_no. Row not found.\n");
    }
    return row_counter - 1;
}

/*! \brief Interrogate the control object for fishery, catch and biol numbers
 *
 * Relative fishery etc is offered.
 * Basic sanity check offered.
 *
 * \param target_no Target number
 * \param sim_target_no Simultaneous target number
 * \param relative Are we checking the relative fishery etc.
 * \param check Are we checking for NAs
 */

std::vector<unsigned int> fwdControl::get_FCB_nos(const unsigned int target_no, const unsigned int sim_target_no, const bool relative, const bool check) const{
    unsigned int fishery_no, catch_no, biol_no;
    if (relative){
        fishery_no = get_target_int_col(target_no, sim_target_no, "relFishery");
        catch_no = get_target_int_col(target_no, sim_target_no, "relCatch");
        biol_no = get_target_int_col(target_no, sim_target_no, "relBiol");
    }
    if (!relative){
        fishery_no = get_target_int_col(target_no, sim_target_no, "fishery");
        catch_no = get_target_int_col(target_no, sim_target_no, "catch");
        biol_no = get_target_int_col(target_no, sim_target_no, "biol");
    }
    if (check){
        bool fishery_na = Rcpp::IntegerVector::is_na(fishery_no);
        bool catch_na = Rcpp::IntegerVector::is_na(catch_no);
        bool biol_na = Rcpp::IntegerVector::is_na(biol_no);
        // If all three of FCB are NA then something is very wrong
        if (biol_na & fishery_na & catch_na){
            Rcpp::stop("In fwdControl::get_FCB_nos. You need at least a fishery, catch or biol in control.\n");
        }
        // If we have a Catch we must have a Fishery
        if (!catch_na & fishery_na){
            Rcpp::stop("In fwdControl::get_FCB_nos. If catch specified, fishery must also be specified.\n");
        }
    }
    std::vector<unsigned int> FCB_nos = {fishery_no, catch_no, biol_no};
    return FCB_nos;
}

/*------------------------------------------------------------------*/

/* Just some tests to operate on data.frames */
// [[Rcpp::export]]
Rcpp::IntegerVector get_dataframe_year(Rcpp::DataFrame ctrl){
    Rcpp::IntegerVector year = ctrl["year"];
    return year;
}

// [[Rcpp::export]]
Rcpp::NumericVector get_dataframe_value(Rcpp::DataFrame ctrl){
    Rcpp::NumericVector value = ctrl["value"];
    return value;
}

// [[Rcpp::export]]
Rcpp::CharacterVector get_dataframe_quantity(Rcpp::DataFrame ctrl){
    Rcpp::CharacterVector quantity = ctrl["quant"];
    return quantity;
}
