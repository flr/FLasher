// Change the name of this

/* 
 * Copyright 2013 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/control.h"

//Rcpp::IntegerVector v =
//Rcpp::IntegerVector::create(7,8,9);
//std::vector<std::string> s(3);
//s[0] = "x";
//s[1] = "y";
//s[2] = "z";
//return Rcpp::DataFrame::create(Rcpp::Named("a")=v,
//Rcpp::Named("b")=s);



void fwdControl::init_target_map(){
    // Fill up the map
    target_map["f"] = target_f;
    target_map["catch"] = target_catch;
    target_map["ssb"] = target_ssb;
    target_map["biomass"] = target_biomass;
    return;
}


// Empty constructor
fwdControl::fwdControl(){
    target = Rcpp::DataFrame();
    target_iters = Rcpp::NumericVector();
}

// Constructor used as intrinsic 'as'
fwdControl::fwdControl(SEXP fwd_control_sexp){
	Rcpp::S4 fwd_control_s4 = Rcpp::as<Rcpp::S4>(fwd_control_sexp);
    target_iters = fwd_control_s4.slot("target_iters");
    target = fwd_control_s4.slot("target");
    init_target_map();
}

// intrinsic 'wrap' 
fwdControl::operator SEXP() const{
    Rcpp::S4 fwd_control_s4("fwdControlTest");
    fwd_control_s4.slot("target") = target;
    fwd_control_s4.slot("target_iters") = target_iters;
    return Rcpp::wrap(fwd_control_s4);
}

// Copy constructor - else 'data' can be pointed at by multiple instances
fwdControl::fwdControl(const fwdControl& fwdControl_source){
    target = Rcpp::clone<Rcpp::DataFrame>(fwdControl_source.target); // Need to clone for a deep copy
    target_iters = Rcpp::clone<Rcpp::NumericVector>(fwdControl_source.target_iters); // Need to clone 
    target_map = fwdControl_source.target_map;
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
fwdControl& fwdControl::operator = (const fwdControl& fwdControl_source){
	if (this != &fwdControl_source){
        target = Rcpp::clone<Rcpp::DataFrame>(fwdControl_source.target); // Need to clone for a deep copy
        target_iters = Rcpp::clone<Rcpp::NumericVector>(fwdControl_source.target_iters); // Need to clone 
        target_map = fwdControl_source.target_map;
	}
	return *this;
}

Rcpp::DataFrame fwdControl::get_target() const{
    return target;
}

// Returns the number of targets in the control object
int fwdControl::get_ntarget() const{
     return target.nrows();
}

// Returns the number of iterations in the target_iters member object
int fwdControl::get_niter() const{
    Rcpp::IntegerVector dim = target_iters.attr("dim");
     return dim[2];
}

// Returns the year and season of the target - used to calculate the timestep in the projection loop
// These are integer indices - not characters
int fwdControl::get_target_year(const int target_no) const {
    Rcpp::IntegerVector year = target["year"];
    if (target_no > year.size()){
        Rcpp::stop("In fwdControl::get_target_year. target_no > number of targets\n");
    }
    return year[target_no-1];
}

int fwdControl::get_target_rel_year(const int target_no) const {
    Rcpp::IntegerVector rel_year = target["rel_year"];
    if (target_no > rel_year.size()){
        Rcpp::stop("In fwdControl::get_target_rel_year. target_no > number of targets\n");
    }
    return rel_year[target_no-1];
}

int fwdControl::get_target_season(const int target_no) const {
    Rcpp::IntegerVector season = target["season"];
    if (target_no > season.size()){
        Rcpp::stop("In fwdControl::get_target_season. target_no > number of targets\n");
    }
    return season[target_no-1];
}

int fwdControl::get_target_rel_season(const int target_no) const {
    Rcpp::IntegerVector rel_season = target["rel_season"];
    if (target_no > rel_season.size()){
        Rcpp::stop("In fwdControl::get_target_rel)season. target_no > number of targets\n");
    }
    return rel_season[target_no-1];
}

int fwdControl::get_target_fishery(const int target_no) const {
    Rcpp::IntegerVector fishery = target["fishery"];
    if (target_no > fishery.size()){
        Rcpp::stop("In fwdControl::get_target_fishery. target_no > number of targets\n");
    }
    return fishery[target_no-1];
}

// Returns the age range - literally just the values in target
Rcpp::IntegerVector fwdControl::get_age_range(const int target_no) const{
    Rcpp::IntegerVector min_age = target["min_age"];
    Rcpp::IntegerVector max_age = target["max_age"];
    if (target_no > min_age.size()){
        Rcpp::stop("In fwdControl::get_target_fishery. target_no > number of targets\n");
    }
    Rcpp::IntegerVector age_range(2);
    age_range[0] = min_age[target_no-1];
    age_range[1] = max_age[target_no-1];
    return age_range;

} 



// It's a 3D array and we want the 2nd column of the 2nd dimension
// Indexing starts at 1
double fwdControl::get_target_value(const int target_no, const int col, const int iter) const{
    Rcpp::IntegerVector dim = target_iters.attr("dim");
    unsigned int element = (dim[1] * dim[0] * (iter - 1)) + (dim[0] * (col - 1)) + (target_no - 1); 
    return target_iters(element);
}

// It's a 3D array and we want the 2nd column of the 2nd dimension
// Indexing starts at 1
// Get all iters
// Could write this with some container magic
std::vector<double> fwdControl::get_target_value(const int target_no, const int col) const{
    Rcpp::IntegerVector dim = target_iters.attr("dim");
    std::vector<double> out(dim[2], 0.0);
    unsigned int element;
    for (int iter_count = 0; iter_count < dim[2]; ++iter_count){
        element = (dim[1] * dim[0] * (iter_count)) + (dim[0] * (col - 1)) + (target_no - 1); 
        out[iter_count] = target_iters(element);
    }
    return out;
}


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
