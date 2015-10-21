/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/FLQuant_base.h"

// CppAD (QUANT) constructor

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_as_wrap(FLQuantAD flq){
	return flq;
}

// [[Rcpp::export]]
void test_FLQuantAD_basic_constructor(){
    FLQuantAD flq;
    return;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_sexp_constructor(SEXP flq_sexp){
	FLQuantAD flq(flq_sexp);
	return flq;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_dim_constructor(int nquant, int nyear, int nunit, int nseason, int narea, int niter){
    FLQuantAD flq(nquant, nyear, nunit, nseason, narea, niter);
    return flq;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_copy_constructor(FLQuantAD flq){
    FLQuantAD out(flq);
    return out;
}

// Checking that a deep copy has been made
// [[Rcpp::export]]
Rcpp::List test_FLQuantAD_copy_constructor2(FLQuantAD flq1, int quant, int year, int unit, int season, int area, int iter, double value){
	FLQuantAD flq2(flq1); 
    adouble ad_value = value;
	flq2(quant,year,unit,season,area,iter) = ad_value;
	return Rcpp::List::create(Rcpp::Named("flq1", flq1),
                            Rcpp::Named("flq2",flq2));
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_assignment_operator(FLQuantAD flq){
    FLQuantAD out;
    out = flq;
    return out;
}

// Checking that a deep copy has been made
// [[Rcpp::export]]
Rcpp::List test_FLQuantAD_assignment_operator2(FLQuantAD flq1, int quant, int year, int unit, int season, int area, int iter, double value){
	FLQuantAD flq2;
    flq2 = flq1; 
    adouble ad_value = value;
	flq2(quant,year,unit,season,area,iter) = ad_value;
	return Rcpp::List::create(Rcpp::Named("flq1", flq1),
				Rcpp::Named("flq2",flq2));
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_from_FLQuant_constructor(FLQuant flq){
    FLQuantAD flqad(flq);
    return flqad;
}

//------------------ Accessors ----------------------

// [[Rcpp::export]]
std::string test_FLQuantAD_get_units(FLQuantAD flq){
	return flq.get_units();
}

// [[Rcpp::export]]
std::vector<unsigned int> test_FLQuantAD_get_dim(FLQuantAD flq){
	return flq.get_dim();
}

// [[Rcpp::export]]
Rcpp::List test_FLQuantAD_get_dimnames(FLQuantAD flq){
	return flq.get_dimnames();
}

// [[Rcpp::export]]
int test_FLQuantAD_get_size(FLQuantAD flq){
	return flq.get_size();
}

// [[Rcpp::export]]
int test_FLQuantAD_get_nquant(FLQuantAD flq){
	return flq.get_nquant();
}

// [[Rcpp::export]]
int test_FLQuantAD_get_nyear(FLQuantAD flq){
	return flq.get_nyear();
}

// [[Rcpp::export]]
int test_FLQuantAD_get_nunit(FLQuantAD flq){
	return flq.get_nunit();
}

// [[Rcpp::export]]
int test_FLQuantAD_get_nseason(FLQuantAD flq){
	return flq.get_nseason();
}

// [[Rcpp::export]]
int test_FLQuantAD_get_narea(FLQuantAD flq){
	return flq.get_narea();
}

// [[Rcpp::export]]
int test_FLQuantAD_get_niter(FLQuantAD flq){
	return flq.get_niter();
}

//---------- () accessors -----------------

// [[Rcpp::export]]
int test_FLQuantAD_get_data_element(const FLQuantAD flq, int quant, int year, int unit, int season, int area, int iter){
	int out = 0;
	out = flq.get_data_element(quant,year,unit,season,area,iter);
	return out;
}

// [[Rcpp::export]]
double test_FLQuantAD_get_const_single_index_accessor(const FLQuantAD flq, const int element){
	adouble output = 0.0;
	output = flq(element);
	return Value(output);
}

// [[Rcpp::export]]
double test_FLQuantAD_get_single_index_accessor(FLQuantAD flq, int element){
	adouble output = 0.0;
	output = flq(element);
	return Value(output);
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_set_single_index_accessor(FLQuantAD flq, int element, double value){
    adouble ad_value = value;
    flq(element) = ad_value;
    return flq;
}

// [[Rcpp::export]]
double test_FLQuantAD_const_get_accessor(const FLQuantAD flq, int quant, int year, int unit, int season, int area, int iter){
	adouble output = 0.0;
	output = flq(quant,year,unit,season,area,iter);
	return Value(output);
}

// [[Rcpp::export]]
double test_FLQuantAD_get_accessor(FLQuantAD flq, int quant, int year, int unit, int season, int area, int iter){
	adouble output = 0.0;
	output = flq(quant,year,unit,season,area,iter);
	return Value(output);
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_set_accessor(FLQuantAD flq, int quant, int year, int unit, int season, int area, int iter, double value){
    adouble ad_value = value;
	flq(quant,year,unit,season,area,iter) = ad_value;
	return flq;
}

// [[Rcpp::export]]
double test_FLQuantAD_get_const_indices_accessor(const FLQuantAD flq, std::vector<unsigned int> indices){
	return Value(flq(indices));
}

// [[Rcpp::export]]
double test_FLQuantAD_get_indices_accessor(FLQuantAD flq, std::vector<unsigned int> indices){
	return Value(flq(indices));
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_set_indices_accessor(FLQuantAD flq, std::vector<unsigned int> indices, double value){
    adouble ad_value = value;
    flq(indices) = ad_value;
    return flq;
}
//------------ Set methods -------------------------

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_set_dimnames(FLQuantAD flq, Rcpp::List new_dimnames){
    flq.set_dimnames(new_dimnames);
    return flq;
}

//------------------ Others --------------------------------------
// [[Rcpp::export]]
int test_FLQuantAD_FLQuantAD_match_dims(FLQuantAD flq1, FLQuantAD flq2){
    return flq1.match_dims(flq2);
}

// [[Rcpp::export]]
int test_FLQuantAD_FLQuant_match_dims(FLQuantAD flq1, FLQuant flq2){
    return flq1.match_dims(flq2);
}

// [[Rcpp::export]]
int test_FLQuant_FLQuantAD_match_dims(FLQuant flq1, FLQuantAD flq2){
    return flq1.match_dims(flq2);
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_fill(FLQuantAD flq, const double value) {
    adouble value_ad = value;
    flq.fill(value_ad);
    return flq;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_fill_double(FLQuantAD flq, const double value) {
    flq.fill(value);
    return flq;
}
