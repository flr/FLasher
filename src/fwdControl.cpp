// Change the name of this

/* 
 * Copyright 2013 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/fwdControl.h"

//Rcpp::IntegerVector v =
//Rcpp::IntegerVector::create(7,8,9);
//std::vector<std::string> s(3);
//s[0] = "x";
//s[1] = "y";
//s[2] = "z";
//return Rcpp::DataFrame::create(Rcpp::Named("a")=v,
//Rcpp::Named("b")=s);


// maps the quantity type character string to the enumerated types
void fwdControl::init_target_map(){
    // Fill up the map
    target_map["f"] = target_f;
    target_map["catch"] = target_catch;
    target_map["landings"] = target_landings;
    target_map["discards"] = target_discards;
    target_map["ssb"] = target_ssb;
    target_map["biomass"] = target_biomass;
    return;
}

// Empty constructor
fwdControl::fwdControl(){
    target = Rcpp::DataFrame();
    target_iters = Rcpp::NumericVector();
    FCB = Rcpp::IntegerMatrix();
}

// Constructor used as intrinsic 'as'
// Add FCB
fwdControl::fwdControl(SEXP fwd_control_sexp){
	Rcpp::S4 fwd_control_s4 = Rcpp::as<Rcpp::S4>(fwd_control_sexp);
	Rcpp::S4 target_s4 = fwd_control_s4.slot("target");
    target_iters = target_s4.slot("iters");
    target = target_s4.slot("element");
    FCB = Rcpp::as<Rcpp::IntegerMatrix>(target_s4.slot("FCB")); // Why does need an as?
    init_target_map();
}

// intrinsic 'wrap' 
// Add FCB
fwdControl::operator SEXP() const{
    Rcpp::S4 fwd_control_s4("fwdControl");
    Rcpp::S4 fwd_element_s4("fwdElement");
    fwd_element_s4.slot("element") = target;
    fwd_element_s4.slot("iters") = target_iters;
    // fwd_element_s4.slot("FCB") = FCB;
    fwd_control_s4.slot("target") = fwd_element_s4;
    return Rcpp::wrap(fwd_control_s4);
}

// Copy constructor - else 'data' can be pointed at by multiple instances
fwdControl::fwdControl(const fwdControl& fwdControl_source){
    target = Rcpp::clone<Rcpp::DataFrame>(fwdControl_source.target); // Need to clone for a deep copy
    target_iters = Rcpp::clone<Rcpp::NumericVector>(fwdControl_source.target_iters); // Need to clone 
    target_map = fwdControl_source.target_map;
    FCB = fwdControl_source.FCB;
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
fwdControl& fwdControl::operator = (const fwdControl& fwdControl_source){
	if (this != &fwdControl_source){
        target = Rcpp::clone<Rcpp::DataFrame>(fwdControl_source.target); // Need to clone for a deep copy
        target_iters = Rcpp::clone<Rcpp::NumericVector>(fwdControl_source.target_iters); // Need to clone 
        target_map = fwdControl_source.target_map;
        FCB = fwdControl_source.FCB;
	}
	return *this;
}

Rcpp::DataFrame fwdControl::get_target() const{
    return target;
}

// How many timesteps are we dealing with here
// Just interrogates the timestep column in the dataframe
unsigned int fwdControl::get_ntimestep() const{
    // Check that the timestep exists 
    std::vector<std::string> col_names = target.attr("names");
    if (std::find(col_names.begin(), col_names.end(), "timestep") == col_names.end()){
        Rcpp::stop("In fwdControl::get_ntimestep - no timestep column in control dataframe\n");
    }
    std::vector<unsigned int> timestep = target["timestep"];
    // Assume that they are contiguous
    // get max, get min
    auto max = *std::max_element(timestep.begin(), timestep.end());
    auto min = *std::min_element(timestep.begin(), timestep.end());
    auto ntimestep = (max - min + 1);
    return ntimestep;

}

// Returns the number of targets in the control object
// Target numbers should be contiguous starting from 1
unsigned int fwdControl::get_ntarget() const{
    // Check that the target column exists 
    std::vector<std::string> col_names = target.attr("names");
    if (std::find(col_names.begin(), col_names.end(), "target") == col_names.end()){
        Rcpp::stop("In fwdControl::get_ntarget - no target column in control dataframe\n");
    }
    std::vector<unsigned int> target_no = target["target"];
    auto minmax = std::minmax_element(target_no.begin(), target_no.end());
    auto ntarget = (*minmax.second - *minmax.first + 1);
    return ntarget;
}

// Returns the number of iterations in the target_iters member object
unsigned int fwdControl::get_niter() const{
    Rcpp::IntegerVector dim = target_iters.attr("dim");
     return dim[2];
}

// Returns the age range - literally just the values in target
Rcpp::IntegerVector fwdControl::get_age_range(const int target_no) const{
    Rcpp::IntegerVector min_age = target["minAge"];
    Rcpp::IntegerVector max_age = target["maxAge"];
    if (target_no > min_age.size()){
        Rcpp::stop("In fwdControl::get_target_fishery. target_no > number of targets\n");
    }
    Rcpp::IntegerVector age_range(2);
    age_range[0] = min_age[target_no-1];
    age_range[1] = max_age[target_no-1];
    return age_range;

} 

/*! \brief Get the number of simultaneous targets associated with a target number
 *
 * A target can have multiple simultaneous targets. For example, if you have 2 FLFishery objects aiming to hit 2 catches at the same time. 
 * This method returns the number of simultanous targets associated with a target.
 * \param target_no References the target column in the control dataframe.
 */
unsigned int fwdControl::get_nsim_target(unsigned int target_no) const{
    Rcpp::IntegerVector targets = target["target"];
    // Do it all with STL - probably slower but looks fancy
    // Sort them
    std::sort(targets.begin(), targets.end());
    // [&] means capture variable, means we can get target_no
    auto it_greater = std::find_if(targets.begin(), targets.end(), [&] (const unsigned int& x) {return x > target_no;});
    auto it_value = std::find(targets.begin(), targets.end(), target_no);
    if (it_value == targets.end()){
        Rcpp::stop("In fwdControl::get_nsim_target. target_no not found in target column\n");
    }
    return it_greater - it_value;
}


/*! \name
 */
//@{
/*! \brief Given the target_no return the corresponding row numbers in the control object.
 *
 * Returned row number starts at 0
 * \param target_no References the target column in the control dataframe.
 */
std::vector<unsigned int> fwdControl::get_target_row(unsigned int target_no) const {
    Rcpp::IntegerVector targets = target["target"];
    unsigned int nsim_target = get_nsim_target(target_no);
    std::vector<unsigned int> rows(nsim_target);
    auto current_target = targets.begin();
    for (unsigned int target_count = 0; target_count < nsim_target; ++target_count){
        current_target = std::find(current_target, targets.end(), target_no);
        if (current_target == targets.end()){
            Rcpp::stop("In fwdControl::get_target_row. target row not found\n");
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

// It's a 3D array and we want the 2nd column of the 2nd dimension
// Indexing starts at 1
// Get all iters
// Could write this with some container magic
/*! \brief Get the target value from the control object
 *
 * Get all iterations, from all simultaneous targets, in the control frame.
 * Returns a single vector of length niter * nsimtarget
 * with simtarget1 taking indices 0:(niter-1), simtarget2 taking indices niter:(2*niter-1) and so on.
 * Returns the value, min or max values depending on the 'col' argument.
 * \param target_no References the target column in the control dataframe.
 * \param col 1 for min, 2 for value, 3 for max column
 */
std::vector<double> fwdControl::get_target_value(const int target_no, const int col) const{
    auto nsim_target = get_nsim_target(target_no);
    Rcpp::IntegerVector dim = target_iters.attr("dim");
    std::vector<double> out(dim[2] * nsim_target, 0.0);
    unsigned int target_element = 0;
    unsigned int target_row = 0;
    for (unsigned int sim_target_count = 1; sim_target_count <= nsim_target; ++sim_target_count){
        target_row = get_target_row(target_no, sim_target_count);
        for (unsigned int iter_count = 0; iter_count < dim[2]; ++iter_count){
            target_element = (dim[1] * dim[0] * (iter_count)) + (dim[0] * (col - 1)) + target_row; 
            out[iter_count + (dim[2] * (sim_target_count-1))] = target_iters(target_element);
        }
    }
    return out;
}

/*! \brief Subset an integer column in the control object by the target_no
 *
 * Can be used on non-Integer columns (no check is made) but who knows what the result will be?!?!
 * \param target_no References the target column in the control dataframe.
 */
Rcpp::IntegerVector fwdControl::get_target_int_col(const int target_no, const std::string col) const {
    // Check that column exists in data.frame
    std::vector<std::string> names = target.attr("names");
    auto it = std::find(names.begin(), names.end(), col);
    if (it == names.end()){
        Rcpp::stop("In fwdControl::get_target_int_col. Column name not found,\n");
    }
    Rcpp::IntegerVector all = target[col];
    std::vector<unsigned int> rows = get_target_row(target_no);
    Rcpp::IntegerVector subset(rows.size());
    for (auto i=0; i < subset.size(); i++){
        subset[i] = all[rows[i]];
    }
    return subset;
}


//int fwdControl::get_target_fishery(const int target_no) const {
//    Rcpp::IntegerVector fishery = target["fishery"];
//    if (target_no > fishery.size()){
//        Rcpp::stop("In fwdControl::get_target_fishery. target_no > number of targets\n");
//    }
//    return fishery[target_no-1];
//}


// target_no starts at 1
std::string fwdControl::get_target_quantity(const int target_no) const{
    Rcpp::CharacterVector quantities = target["quantity"];
    if (target_no > quantities.size()){
        Rcpp::stop("In fwdControl::get_target_type. target_no > number of targets\n");
    }
    return Rcpp::as<std::string>(quantities[target_no - 1]);
}

// target_no starts at 1
fwdControlTargetType fwdControl::get_target_type(const int target_no) const{
    std::string quantity = get_target_quantity(target_no);
    target_map_type::const_iterator type_pair_found = target_map.find(quantity);
    if (type_pair_found == target_map.end()){
        Rcpp::stop("Unable to find target quantity in fwdControl target_map\n");
    }
    return type_pair_found->second;
}

/*--------------------- FCB accessors ------------------------------*/

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
    Rcpp::CharacterVector quantity = ctrl["quantity"];
    return quantity;
}
