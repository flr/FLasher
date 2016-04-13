/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/solver.h"
typedef CppAD::AD<double> adouble;

// [[Rcpp::export]]
double test_euclid_norm(std::vector<double> xvec){
    double norm = euclid_norm(xvec);
    return norm;
}

//--------------------------------------------------------------------
// Newton-Raphson tests

// Multiple 1D quadratics solved independently but at the same time: a1 * x^2 + a2 * x + a3
// Not simultaneous equations
// [[Rcpp::export]]
Rcpp::List test_NR_quad_iters(const Rcpp::NumericMatrix coefs, const double initial_value, const int max_iters, const double indep_min, const double indep_max, const double tolerance){
    unsigned int niters = coefs.nrow();
    std::vector<adouble> x(niters,initial_value); 
    CppAD::Independent(x); // Tape on
    std::vector<adouble> y(niters); 
    for (auto i=0; i<niters; ++i){
        y[i] = coefs(i,0)*pow(x[i],2) + coefs(i,1)*x[i] + coefs(i,2); 
    }
    CppAD::ADFun<double> fun(x, y); // Tape off
    std::vector<double> xsolve(niters,initial_value); 
    std::vector<int> success_code = newton_raphson(xsolve, fun, niters, 1, indep_min, indep_max, max_iters, tolerance);
	return Rcpp::List::create(Rcpp::Named("x", xsolve),
                            Rcpp::Named("success_code",success_code));
}


// Simple 2D: solution: +- 0.8895, 1.7913
// [[Rcpp::export]]
Rcpp::List test_NR2(std::vector<double> initial_value, const int max_iters, const double indep_min, const double indep_max, const double tolerance){
    // f(x) = x^3 - 2x + 2, take 0 as starting point = infinite cycle
    std::vector<CppAD::AD<double> > x(2);
    std::copy(initial_value.begin(), initial_value.end(), x.begin());
    CppAD::Independent(x);
    std::vector<CppAD::AD<double> > y(2); 
    y[0] = pow(x[0],2) + pow(x[1],2) - 4; // A circle!
    y[1] = pow(x[0],2) - x[1] + 1;
    // simple: 2, 3
    //y[0] = 2 * x[1] + x[0] - 8;
    //y[1] = 1 + x[1] - 2*x[0];
    CppAD::ADFun<double> fun(x, y);
    std::vector<double> xsolve = initial_value; 
    std::vector<int> out = newton_raphson(xsolve, fun, 1, 2, indep_min, indep_max, max_iters, tolerance);
	return Rcpp::List::create(Rcpp::Named("x", xsolve),
                            Rcpp::Named("out",out));
}
