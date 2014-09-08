/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/FLQuant_multidim.h"

// [[Rcpp::export]]
FLQuant7 test_FLQuant7_as_wrap(FLQuant7 flq7){
    return flq7;
}

// [[Rcpp::export]]
FLQuant7 test_FLQuant7_empty_wrap(){
	FLQuant7 flq7;
	return flq7;
}

// [[Rcpp::export]]
void test_FLQuant7_basic_constructor(){
    FLQuant7 flq;
    return;
}

// [[Rcpp::export]]
FLQuant7 test_FLQuant7_sexp_constructor(SEXP flq_sexp1){
	FLQuant7 flq7(flq_sexp1);
	return flq7;
}

// [[Rcpp::export]]
FLQuant7 test_FLQuant7_FLQuant_constructor(FLQuant flq){
	FLQuant7 flq7(flq);
	return flq7;
}

// [[Rcpp::export]]
FLQuant7 test_FLQuant7_copy_constructor(FLQuant7 flq7){
    FLQuant7 out(flq7);
    return out;
}

// Checking that a deep copy has been made
// [[Rcpp::export]]
Rcpp::List test_FLQuant7_copy_constructor2(FLQuant7 flq71, int dim7, int quant, int year, int unit, int season, int area, int iter, double value){
	FLQuant7 flq72(flq71); 
	flq72(quant,year,unit,season,area,iter, dim7) = value;
	return Rcpp::List::create(Rcpp::Named("flq71", flq71),
                            Rcpp::Named("flq72",flq72));
}

// [[Rcpp::export]]
FLQuant7 test_FLQuant7_assignment_operator(FLQuant7 flq7){
    FLQuant7 out;
    out = flq7;
    return out;
}

// Checking that a deep copy has been made
// [[Rcpp::export]]
Rcpp::List test_FLQuant7_assignment_operator2(FLQuant7 flq71, int dim7, int quant, int year, int unit, int season, int area, int iter, double value){
	FLQuant7 flq72;
    flq72 = flq71; 
	flq72(quant,year,unit,season,area,iter, dim7) = value;
	return Rcpp::List::create(Rcpp::Named("flq71", flq71),
				Rcpp::Named("flq72",flq72));
}

// Testing adding another FLQ into FLQ7 - also tests wrap
// [[Rcpp::export]]
FLQuant7 test_FLQuant7_function_operator(FLQuant7 flq7, FLQuant flq){
    flq7(flq);
	return flq7;
}

// [[Rcpp::export]]
int test_FLQuant7_get_ndim7(SEXP flq_sexp1){
	FLQuant7 flq7(flq_sexp1);
    int length = flq7.get_ndim7();
	return length;
}

// Test copy and assignment constructors

// [[Rcpp::export]]
FLQuant test_FLQuant7_const_get_single_index_accessor(const FLQuant7 flq7, const int element){
    FLQuant flq = flq7(element);
    return flq;
}

// [[Rcpp::export]]
FLQuant test_FLQuant7_get_single_index_accessor(FLQuant7 flq7, const int element){
    FLQuant flq = flq7(element);
    return flq;
}

// [[Rcpp::export]]
FLQuant7 test_FLQuant7_set_single_index_accessor(FLQuant7 flq7, const int element, const FLQuant flq){
    flq7(element) = flq;
    return flq7;
}

// [[Rcpp::export]]
double test_FLQuant7_const_get_accessor(const FLQuant7 flq7, const int dim7, const int quant, const int year, const int unit, const int season, const int area, const int iter){
	double output = 0.0;
	output = flq7(quant,year,unit,season,area,iter, dim7);
	return output;
}

// [[Rcpp::export]]
double test_FLQuant7_get_accessor(FLQuant7 flq7, const int dim7, const int quant, const int year, const int unit, const int season, const int area, const int iter){
	double output = 0.0;
	output = flq7(quant,year,unit,season,area,iter, dim7);
	return output;
}

// [[Rcpp::export]]
FLQuant7 test_FLQuant7_set_accessor(FLQuant7 flq7, const int dim7, const int quant, const int year, const int unit, const int season, const int area, const int iter, const double value){
	flq7(quant,year,unit,season,area,iter, dim7) = value;
	return flq7;
}

// [[Rcpp::export]]
FLQuant test_FLQuant7_const_default_dim7_get_accessor(const FLQuant7 flq7){
    return flq7();
}

// [[Rcpp::export]]
FLQuant test_FLQuant7_default_dim7_get_accessor(FLQuant7 flq7){
    return flq7();
}

// [[Rcpp::export]]
FLQuant7 test_FLQuant7_default_dim7_set_accessor(FLQuant7 flq7, FLQuant flq){
    flq7() = flq;
    return flq7;
}

// [[Rcpp::export]]
double test_FLQuant7_const_default_dim7_get_accessor_multi(const FLQuant7 flq7, const int quant, const int year, const int unit, const int season, const int area, const int iter){
    return flq7(quant,year,unit,season,area,iter);
}

// [[Rcpp::export]]
double test_FLQuant7_default_dim7_get_accessor_multi(FLQuant7 flq7, const int quant, const int year, const int unit, const int season, const int area, const int iter){
    return flq7(quant,year,unit,season,area,iter);
}

// [[Rcpp::export]]
FLQuant7 test_FLQuant7_default_dim7_set_accessor_multi(FLQuant7 flq7, const int quant, const int year, const int unit, const int season, const int area, const int iter, const double value){
    flq7(quant,year,unit,season,area,iter) = value;
    return flq7;
}


/*------------------------------------------------------------*/
// AD versions

// [[Rcpp::export]]
FLQuant7AD test_FLQuant7AD_as_wrap(FLQuant7AD flq7){
    return flq7;
}

// [[Rcpp::export]]
FLQuant7AD test_FLQuant7AD_empty_wrap(){
	FLQuant7AD flq7;
	return flq7;
}

// [[Rcpp::export]]
void test_FLQuant7AD_basic_constructor(){
    FLQuant7AD flq;
    return;
}

// [[Rcpp::export]]
FLQuant7AD test_FLQuant7AD_sexp_constructor(SEXP flq_sexp1){
	FLQuant7AD flq7(flq_sexp1);
	return flq7;
}

// [[Rcpp::export]]
FLQuant7AD test_FLQuant7AD_FLQuant_constructor(FLQuantAD flq){
	FLQuant7AD flq7(flq);
	return flq7;
}

// [[Rcpp::export]]
FLQuant7AD test_FLQuant7AD_copy_constructor(FLQuant7AD flq7){
    FLQuant7AD out(flq7);
    return out;
}

// Checking that a deep copy has been made
// [[Rcpp::export]]
Rcpp::List test_FLQuant7AD_copy_constructor2(FLQuant7AD flq71, int dim7, int quant, int year, int unit, int season, int area, int iter, double value){
    adouble ad_value = value;
	FLQuant7AD flq72(flq71); 
	flq72(quant,year,unit,season,area,iter, dim7) = ad_value;
	return Rcpp::List::create(Rcpp::Named("flq71", flq71),
                            Rcpp::Named("flq72",flq72));
}

// [[Rcpp::export]]
FLQuant7AD test_FLQuant7AD_assignment_operator(FLQuant7AD flq7){
    FLQuant7AD out;
    out = flq7;
    return out;
}

// Checking that a deep copy has been made
// [[Rcpp::export]]
Rcpp::List test_FLQuant7AD_assignment_operator2(FLQuant7AD flq71, int dim7, int quant, int year, int unit, int season, int area, int iter, double value){
    adouble ad_value = value;
	FLQuant7AD flq72;
    flq72 = flq71; 
	flq72(quant,year,unit,season,area,iter, dim7) = ad_value;
	return Rcpp::List::create(Rcpp::Named("flq71", flq71),
				Rcpp::Named("flq72",flq72));
}

// Testing adding another FLQ into FLQ7 - also tests wrap
// [[Rcpp::export]]
FLQuant7AD test_FLQuant7AD_function_operator(FLQuant7AD flq7, FLQuantAD flq){
    flq7(flq);
	return flq7;
}

// [[Rcpp::export]]
int test_FLQuant7AD_get_ndim7(SEXP flq_sexp1){
	FLQuant7AD flq7(flq_sexp1);
    int length = flq7.get_ndim7();
	return length;
}

// Test copy and assignment constructors

// [[Rcpp::export]]
FLQuantAD test_FLQuant7AD_const_get_single_index_accessor(const FLQuant7AD flq7, const int element){
    FLQuantAD flq = flq7(element);
    return flq;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuant7AD_get_single_index_accessor(FLQuant7AD flq7, const int element){
    FLQuantAD flq = flq7(element);
    return flq;
}

// [[Rcpp::export]]
FLQuant7AD test_FLQuant7AD_set_single_index_accessor(FLQuant7AD flq7, const int element, const FLQuantAD flq){
    flq7(element) = flq;
    return flq7;
}

// [[Rcpp::export]]
double test_FLQuant7AD_const_get_accessor(const FLQuant7AD flq7, const int dim7, const int quant, const int year, const int unit, const int season, const int area, const int iter){
	adouble output = 0.0;
	output = flq7(quant,year,unit,season,area,iter, dim7);
	//return output.value();
    return Value(output);
}

// [[Rcpp::export]]
double test_FLQuant7AD_get_accessor(FLQuant7AD flq7, const int dim7, const int quant, const int year, const int unit, const int season, const int area, const int iter){
	adouble output = 0.0;
	output = flq7(quant,year,unit,season,area,iter, dim7);
	//return output.value();
    return Value(output);
}

// [[Rcpp::export]]
FLQuant7AD test_FLQuant7AD_set_accessor(FLQuant7AD flq7, const int dim7, const int quant, const int year, const int unit, const int season, const int area, const int iter, const double value){
    adouble ad_value = value;
	flq7(quant,year,unit,season,area,iter, dim7) = ad_value;
	return flq7;
}


// [[Rcpp::export]]
FLQuantAD test_FLQuant7AD_const_default_dim7_get_accessor(const FLQuant7AD flq7){
    return flq7();
}

// [[Rcpp::export]]
FLQuantAD test_FLQuant7AD_default_dim7_get_accessor(FLQuant7AD flq7){
    return flq7();
}

// [[Rcpp::export]]
FLQuant7AD test_FLQuant7AD_default_dim7_set_accessor(FLQuant7AD flq7, FLQuantAD flq){
    flq7() = flq;
    return flq7;
}

// [[Rcpp::export]]
double test_FLQuant7AD_const_default_dim7_get_accessor_multi(const FLQuant7AD flq7, const int quant, const int year, const int unit, const int season, const int area, const int iter){
    //return flq7(quant,year,unit,season,area,iter).value();
    return Value(flq7(quant,year,unit,season,area,iter));
}

// [[Rcpp::export]]
double test_FLQuant7AD_default_dim7_get_accessor_multi(FLQuant7AD flq7, const int quant, const int year, const int unit, const int season, const int area, const int iter){
    //return flq7(quant,year,unit,season,area,iter).value();
    return Value(flq7(quant,year,unit,season,area,iter));
}

// [[Rcpp::export]]
FLQuant7AD test_FLQuant7AD_default_dim7_set_accessor_multi(FLQuant7AD flq7, const int quant, const int year, const int unit, const int season, const int area, const int iter, const double value){
    adouble ad_value = value;
    flq7(quant,year,unit,season,area,iter) = ad_value;
    return flq7;
}



