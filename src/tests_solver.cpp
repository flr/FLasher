/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/solver.h"
typedef CppAD::AD<double> adouble;

//'@title Test the Euclid norm function
//
//' Metallic spheres
//'@param xvec  A vector of doubles.
//'@rdname solver-cpp-tests
// [[Rcpp::export]]
double test_euclid_norm(std::vector<double> xvec){
    double norm = euclid_norm(xvec);
    return norm;
}

//--------------------------------------------------------------------
// Newton-Raphson tests

// Multiple 1D quadratics solved independently but at the same time: a1 * x^2 + a2 * x + a3
// Not simultaneous equations

//'@title Tests for the CPP NR solver
//
//' Glamrock cops
//'@param coefs a parameter
//'@param initial_value something
//'@param max_iters something
//'@param indep_min something
//'@param indep_max something
//'@param tolerance something
//'@rdname solver-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_NR_quad_iters(const Rcpp::NumericMatrix coefs, const double initial_value, const unsigned int max_iters, const double indep_min, const double indep_max, const double tolerance){
    unsigned int niters = coefs.nrow();
    std::vector<adouble> x(niters,initial_value); 
    CppAD::Independent(x); // Tape on
    std::vector<adouble> y(niters); 
    for (unsigned int i=0; i<niters; ++i){
        y[i] = coefs(i,0)*pow(x[i],2) + coefs(i,1)*x[i] + coefs(i,2); 
    }
    CppAD::ADFun<double> fun(x, y); // Tape off
    std::vector<double> xsolve(niters,initial_value); 
    std::vector<int> success_code = newton_raphson(xsolve, fun, niters, 1, indep_min, indep_max, max_iters, tolerance);
	return Rcpp::List::create(Rcpp::Named("x", xsolve),
                            Rcpp::Named("success_code",success_code));
}

// General coupled linear equation: 
// a11 x + a12 y + a13 z + ... + a14 = 0
// a21 x + a22 y + a23 z + ... + a24 = 0
// a31 x + a32 y + a33 z + ... + a34 = 0
// ... x + ... y + ... z + ... + ... = 0
//'@rdname solver-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_NR_linear(std::vector<double> initial_value, const Rcpp::NumericMatrix coefs, const unsigned int max_iters, const double indep_min, const double indep_max, const double tolerance){
    // Dim of coefs = n x (n+1)
    // Number of indeps = nrow coefs
    auto nindeps = coefs.nrow();
    std::vector<adouble> x(nindeps);
    std::copy(initial_value.begin(), initial_value.end(), x.begin());
    CppAD::Independent(x); // Tape on
    std::vector<adouble> y(nindeps); 
    for (auto row_count=0; row_count < coefs.nrow(); ++row_count){
        y[row_count] = 0.0;
        for (auto indep_count = 0; indep_count < nindeps; ++indep_count){
            y[row_count] = y[row_count] + coefs(row_count,indep_count) * x[indep_count];
        }
        y[row_count] = y[row_count] + coefs(row_count, nindeps); // Add on final coefficient with no multiplier
    }
    CppAD::ADFun<double> fun(x, y);
    std::vector<double> xsolve = initial_value; 
    std::vector<int> success_code = newton_raphson(xsolve, fun, 1, nindeps, indep_min, indep_max, max_iters, tolerance);
	return Rcpp::List::create(Rcpp::Named("x", xsolve),
                            Rcpp::Named("success_code",success_code));
}


