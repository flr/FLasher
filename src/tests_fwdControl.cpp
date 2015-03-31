/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/fwdControl.h"

// [[Rcpp::export]]
fwdControl test_as_wrap_fwdControl(fwdControl control){
    return control;
}

// Checking that a deep copy has been made
// Not so important as we not actually changing any of these values
// In fact we don't have any 'write' accessors for this class
// [[Rcpp::export]]
Rcpp::List test_fwdControl_copy_constructor(fwdControl fc1){
    // Just make a copy
	fwdControl fc2(fc1); 
	return Rcpp::List::create(Rcpp::Named("fc1", fc1),
                            Rcpp::Named("fc2",fc2));
}

// [[Rcpp::export]]
fwdControl test_fwdControl_assignment_operator(fwdControl fc){
    fwdControl out;
    out = fc;
    return out;
}

// [[Rcpp::export]]
unsigned int test_fwdControl_get_ntimestep(const fwdControl control){
    return control.get_ntimestep();
}

// [[Rcpp::export]]
Rcpp::DataFrame test_fwdControl_get_target(const fwdControl control){
    return control.get_target();
}

// [[Rcpp::export]]
int test_fwdControl_get_ntarget(const fwdControl control){
    return control.get_ntarget();
}

// [[Rcpp::export]]
int test_fwdControl_get_niter(const fwdControl control){
    return control.get_niter();
}

// [[Rcpp::export]]
int test_fwdControl_get_target_fishery(const fwdControl control, const int target_no){
    return control.get_target_fishery(target_no);
}

// [[Rcpp::export]]
int test_fwdControl_get_target_year(const fwdControl control, const int target_no){
    return control.get_target_year(target_no);
}

// [[Rcpp::export]]
int test_fwdControl_get_target_season(const fwdControl control, const int target_no){
    return control.get_target_season(target_no);
}

// [[Rcpp::export]]
int test_fwdControl_get_target_rel_year(const fwdControl control, const int target_no){
    return control.get_target_rel_year(target_no);
}

// [[Rcpp::export]]
int test_fwdControl_get_target_rel_season(const fwdControl control, const int target_no){
    return control.get_target_rel_season(target_no);
}

// [[Rcpp::export]]
Rcpp::List test_fwdControl_get_target_value(const fwdControl control, const int target_no, const int col, const int iter){
    double value =  control.get_target_value(target_no, col, iter);
    std::vector<double> values =  control.get_target_value(target_no, col);
	return Rcpp::List::create(Rcpp::Named("value", value),
                            Rcpp::Named("values",values));
}

// [[Rcpp::export]]
std::string test_fwdControl_get_target_quantity(const fwdControl control, const int target_no){
    return control.get_target_quantity(target_no);
}

// [[Rcpp::export]]
Rcpp::IntegerVector test_fwdControl_get_age_range(const fwdControl control, const int target_no){
    return control.get_age_range(target_no);
}

// [[Rcpp::export]]
Rcpp::IntegerMatrix test_fwdControl_get_FC(const fwdControl control, const int biol_no){
    return control.get_FC(biol_no);
}

// [[Rcpp::export]]
std::vector<int> test_fwdControl_get_B(const fwdControl control, const int fishery_no, const int catch_no){
    return control.get_B(fishery_no, catch_no);
}




