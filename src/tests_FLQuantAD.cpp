/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/FLQuant_base.h"

// CppAD (QUANT) constructor


//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_as_wrap(FLQuantAD flq){
	return flq;
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
void test_FLQuantAD_basic_constructor(){
    FLQuantAD flq;
    return;
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_sexp_constructor(SEXP flq_sexp){
	FLQuantAD flq(flq_sexp);
	return flq;
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_dim_constructor(int nquant, int nyear, int nunit, int nseason, int narea, int niter){
    FLQuantAD flq(nquant, nyear, nunit, nseason, narea, niter);
    return flq;
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_copy_constructor(FLQuantAD flq){
    FLQuantAD out(flq);
    return out;
}

// Checking that a deep copy has been made
//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_FLQuantAD_copy_constructor2(FLQuantAD flq1, int quant, int year, int unit, int season, int area, int iter, double value){
	FLQuantAD flq2(flq1); 
    adouble ad_value = value;
	flq2(quant,year,unit,season,area,iter) = ad_value;
	return Rcpp::List::create(Rcpp::Named("flq1", flq1),
                            Rcpp::Named("flq2",flq2));
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_assignment_operator(FLQuantAD flq){
    FLQuantAD out;
    out = flq;
    return out;
}

// Checking that a deep copy has been made
//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_FLQuantAD_assignment_operator2(FLQuantAD flq1, int quant, int year, int unit, int season, int area, int iter, double value){
	FLQuantAD flq2;
    flq2 = flq1; 
    adouble ad_value = value;
	flq2(quant,year,unit,season,area,iter) = ad_value;
	return Rcpp::List::create(Rcpp::Named("flq1", flq1),
				Rcpp::Named("flq2",flq2));
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_from_FLQuant_constructor(FLQuant flq){
    FLQuantAD flqad(flq);
    return flqad;
}

//------------------ Accessors ----------------------

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
std::string test_FLQuantAD_get_units(FLQuantAD flq){
	return flq.get_units();
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
std::vector<unsigned int> test_FLQuantAD_get_dim(FLQuantAD flq){
	return flq.get_dim();
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_FLQuantAD_get_dimnames(FLQuantAD flq){
	return flq.get_dimnames();
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
int test_FLQuantAD_get_size(FLQuantAD flq){
	return flq.get_size();
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
int test_FLQuantAD_get_nquant(FLQuantAD flq){
	return flq.get_nquant();
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
int test_FLQuantAD_get_nyear(FLQuantAD flq){
	return flq.get_nyear();
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
int test_FLQuantAD_get_nunit(FLQuantAD flq){
	return flq.get_nunit();
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
int test_FLQuantAD_get_nseason(FLQuantAD flq){
	return flq.get_nseason();
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
int test_FLQuantAD_get_narea(FLQuantAD flq){
	return flq.get_narea();
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
int test_FLQuantAD_get_niter(FLQuantAD flq){
	return flq.get_niter();
}

//---------- () accessors -----------------

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
int test_FLQuantAD_get_data_element(const FLQuantAD flq, int quant, int year, int unit, int season, int area, int iter){
	int out = 0;
	out = flq.get_data_element(quant,year,unit,season,area,iter);
	return out;
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
double test_FLQuantAD_get_const_single_index_accessor(const FLQuantAD flq, const int element){
	adouble output = 0.0;
	output = flq(element);
	return Value(output);
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
double test_FLQuantAD_get_single_index_accessor(FLQuantAD flq, int element){
	adouble output = 0.0;
	output = flq(element);
	return Value(output);
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_set_single_index_accessor(FLQuantAD flq, int element, double value){
    adouble ad_value = value;
    flq(element) = ad_value;
    return flq;
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
double test_FLQuantAD_const_get_accessor(const FLQuantAD flq, int quant, int year, int unit, int season, int area, int iter){
	adouble output = 0.0;
	output = flq(quant,year,unit,season,area,iter);
	return Value(output);
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
double test_FLQuantAD_get_accessor(FLQuantAD flq, int quant, int year, int unit, int season, int area, int iter){
	adouble output = 0.0;
	output = flq(quant,year,unit,season,area,iter);
	return Value(output);
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_set_accessor(FLQuantAD flq, int quant, int year, int unit, int season, int area, int iter, double value){
    adouble ad_value = value;
	flq(quant,year,unit,season,area,iter) = ad_value;
	return flq;
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
double test_FLQuantAD_get_const_indices_accessor(const FLQuantAD flq, std::vector<unsigned int> indices){
	return Value(flq(indices));
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
double test_FLQuantAD_get_indices_accessor(FLQuantAD flq, std::vector<unsigned int> indices){
	return Value(flq(indices));
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_set_indices_accessor(FLQuantAD flq, std::vector<unsigned int> indices, double value){
    adouble ad_value = value;
    flq(indices) = ad_value;
    return flq;
}
//------------ Set methods -------------------------

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_set_dimnames(FLQuantAD flq, Rcpp::List new_dimnames){
    flq.set_dimnames(new_dimnames);
    return flq;
}

//------------------ Others --------------------------------------
//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
int test_FLQuantAD_FLQuantAD_match_dims(FLQuantAD flq1, FLQuantAD flq2){
    return flq1.match_dims(flq2);
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
int test_FLQuantAD_FLQuant_match_dims(FLQuantAD flq1, FLQuant flq2){
    return flq1.match_dims(flq2);
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
int test_FLQuant_FLQuantAD_match_dims(FLQuant flq1, FLQuantAD flq2){
    return flq1.match_dims(flq2);
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_fill(FLQuantAD flq, const double value) {
    adouble value_ad = value;
    flq.fill(value_ad);
    return flq;
}

//'@rdname FLQuant-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_fill_double(FLQuantAD flq, const double value) {
    flq.fill(value);
    return flq;
}

//------------------ Others --------------------------------------


// [[Rcpp::export]]
void testFLCppAD(double scalar){
    std::vector<adouble> xad = {1.0};

    //adouble scalarad = scalar; // 1. OK
    //FLQuantAD scalarad(1,1,1,1,1,1, scalar); // 2. OK - used in calc but not to recieve output
    FLQuantAD scalarad(1,1,1,1,1,1, scalar); // 3. Works OK now - not in sourceCpp
    //FLQuantAD scalarad(1,1,1,1,1,1, scalar); // 4. OK - accepts direct input of AD via ()
    //FLQuantAD yflqad(1,1,1,1,1,1); // 4. OK

    CppAD::Independent(xad);
    std::vector<adouble> yad(1);


    //yad[0] = xad[0] * xad[0] + xad[0] * scalarad - 6.0; // 1. OK
    //yad[0] = xad[0] * xad[0] + xad[0] * scalarad(1,1,1,1,1,1) - 6.0; // 2. OK
    FLQuantAD yflqad = xad[0] * xad[0] + xad[0] * scalarad - 6.0; // 3. OK Now - not in sourceCpp
    yad[0] = yflqad(1,1,1,1,1,1); // 3. OK Now
    //yflqad(1,1,1,1,1,1) = xad[0] * xad[0] + xad[0] * scalarad(1,1,1,1,1,1) - 6.0; // 4. OK
    //yad[0] = yflqad(1,1,1,1,1,1); // 4. OK

    CppAD::ADFun<double> fun(xad, yad);

    // Solve
    std::vector<double> x = {1.0}; 
    std::vector<double> y;
    std::vector<double> jac;
    for (int nrcount = 0; nrcount < 8; ++nrcount){
        y = fun.Forward(0, x); 
        jac = fun.Jacobian(x);
        Rprintf("x: %f. y: %f. jac: %f\n", x[0], y[0], jac[0]);
        x[0] = x[0] - y[0] / jac[0];
    }


}




