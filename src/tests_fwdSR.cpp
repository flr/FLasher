/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/fwdSR.h"

/*-------------------------------------------------------*/

// [[Rcpp::export]]
void test_fwdSR_empty_constructor(){
	fwdSR flsr;
	return;
}

// [[Rcpp::export]]
fwdSR test_fwdSR_constructor_wrap(const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult){
    fwdSR fwdsr(model_name, params, timelag, residuals, residuals_mult);
    return fwdsr;
}

// [[Rcpp::export]]
double test_fwdSR_constructor_value(std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const double ssb){
    fwdSR fwdsr(model_name, params, timelag, residuals, residuals_mult);
    double out = fwdsr.eval_model(ssb,1,1,1,1,1);
    return out;
}

// [[Rcpp::export]]
int test_fwdSR_get_nparams(std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult){
    fwdSR fwdsr(model_name, params, timelag, residuals, residuals_mult);
    return fwdsr.get_nparams();
}

// [[Rcpp::export]]
double test_fwdSR_eval_simple(std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const double ssb){
    fwdSR fwdsr(model_name, params, timelag, residuals, residuals_mult);
    double rec = fwdsr.eval_model(ssb,1,1,1,1,1);
    return rec;
}

// [[Rcpp::export]]
double test_fwdSR_eval_full(std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const double ssb, const int year, const int unit, const int season, const int area, const int iter){
    fwdSR fwdsr(model_name, params, timelag, residuals, residuals_mult);
    double rec = fwdsr.eval_model(ssb,year,unit,season,area,iter);
    return rec;
}


// [[Rcpp::export]]
Rcpp::List test_fwdSR_copy_constructor(std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const double ssb){
    fwdSR fwdsr1(model_name, params, timelag, residuals, residuals_mult);
	fwdSR fwdsr2(fwdsr1); // uses copy constructor
    double rec1 = fwdsr1.eval_model(ssb,1,1,1,1,1);
    double rec2 = fwdsr2.eval_model(ssb,1,1,1,1,1);
	return Rcpp::List::create(Rcpp::Named("fwdsr1", fwdsr1),
				Rcpp::Named("fwdsr2",fwdsr2),
				Rcpp::Named("rec1",rec1),
				Rcpp::Named("rec2",rec2));
}

// [[Rcpp::export]]
Rcpp::List test_fwdSR_assignment_operator(std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const double ssb){
    fwdSR fwdsr1(model_name, params, timelag, residuals, residuals_mult);
	fwdSR fwdsr2;
    fwdsr2 = fwdsr1; // Assignment happens
    double rec1 = fwdsr1.eval_model(ssb,1,1,1,1,1);
    double rec2 = fwdsr2.eval_model(ssb,1,1,1,1,1);
	return Rcpp::List::create(Rcpp::Named("fwdsr1", fwdsr1),
				Rcpp::Named("fwdsr2",fwdsr2),
				Rcpp::Named("rec1",rec1),
				Rcpp::Named("rec2",rec2));
}


// [[Rcpp::export]]
int test_fwdSR_get_timelag(std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult){
    fwdSR fwdsr(model_name, params, timelag, residuals, residuals_mult);
    return fwdsr.get_timelag();
}

