/* 
 * Copyright 2015 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include <cppad/cppad.hpp> // CppAD package http://www.coin-or.org/CppAD/

#include <Rcpp.h>

double euclid_norm(std::vector<double> x);

// A Newton Raphson solver for a function that has already been taped.
// Pass in the independent variables, tape no. and control parameters
// int newton_raphson(std::vector<double>& indep, const int adolc_tape, const int max_iters= 50, const double max_limit = 100, const double tolerance = 1e-12);
std::vector<int> newton_raphson(std::vector<double>& indep, CppAD::ADFun<double>& fun, const unsigned int niter, const unsigned int nsim_targets, const double indep_min = 0, const double indep_max = 1e9, const unsigned int max_iters= 50, const double tolerance = 1.5e-8);
