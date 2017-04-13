/* 
 * Copyright 2013 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/FLStock.h"

//'@title Tests for CPP implementation of FLStock
//
//' Killed by hippie powers
//'@param fls_sexp a parameter
//'@param flq something
//'@param fls something
//'@param fls1 something
//'@param quant something
//'@param year something
//'@param unit something
//'@param season something
//'@param area something
//'@param iter something
//'@param value something
//'@rdname FLStock-cpp-tests
// [[Rcpp::export]]
FLQuant test_FLQ_in_FLS(FLQuant flq){
    return flq;
}

//'@rdname FLStock-cpp-tests
// [[Rcpp::export]]
FLQuant test_FLStock_sexp_constructor(SEXP fls_sexp){
	FLStock fls(fls_sexp);
	return fls.stock_n;
}

//'@rdname FLStock-cpp-tests
// [[Rcpp::export]]
FLStock test_FLStock_wrap(SEXP fls_sexp){
	FLStock fls(fls_sexp);
	return fls;
}

//'@rdname FLStock-cpp-tests
// [[Rcpp::export]]
FLQuant test_FLStock_as(FLStock fls){
	return fls.stock_n;
}

//'@rdname FLStock-cpp-tests
// [[Rcpp::export]]
FLStock test_FLStock_as_wrap(FLStock fls){
	return fls;
}

//'@rdname FLStock-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_FLStock_copy_constructor(FLStock fls1, int quant, int year, int unit, int season, int area, int iter, double value){
	FLStock fls2(fls1); // uses copy constructor
	fls2.stock_n(quant,year,unit,season,area,iter) = value;
	return Rcpp::List::create(Rcpp::Named("fls1", fls1),
				Rcpp::Named("fls2",fls2));
}

//'@rdname FLStock-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_FLStock_assignment_operator(FLStock fls1, int quant, int year, int unit, int season, int area, int iter, double value){
	FLStock fls2;
    fls2 = fls1; 
	fls2.stock_n(quant,year,unit,season,area,iter) = value;
	return Rcpp::List::create(Rcpp::Named("fls1", fls1),
				Rcpp::Named("fls2",fls2));
}
