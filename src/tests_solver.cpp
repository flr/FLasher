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


//----------------------------------------------------------------------
typedef std::vector<double> (*funcPtr)(std::vector<double> params); 
typedef std::vector<adouble> (*funcPtrAD)(std::vector<adouble> params); 



//using funcPtr = std::vector<double>(*)(std::vector<double>);
//using funcPtrAD = std::vector<adouble>(*)(std::vector<adouble>);

//'@title Eval user function pointer
//
//' Gahhhhhh
//'@param xpsexp Function pointer 
//'@param params params
//'@rdname plugin-tests
// [[Rcpp::export]]
std::vector<double> eval_user_function(SEXP xpsexp, std::vector<double> params) {
    Rcpp::XPtr<funcPtr> xpfun(xpsexp);
    funcPtr myfunc = *xpfun;
    return myfunc(params);
}

//std::vector<adouble> banana(std::vector<adouble> x){
//    std::vector<adouble> res(1, 0.0);
//    res[0] = 100 * pow((x[1] - x[0] * x[0]), 2.0) + pow((1 - x[0]), 2.0);
//    return res;
//}

// This function seems to work but the gradients for a function pointer are 0
//'@title Eval user gradient function pointer
//
//' Doesn't work
//'@param xpsexp Function pointer 
//'@param params params
//'@rdname plugin-tests
// [[Rcpp::export]]
std::vector<double> eval_user_gradient(SEXP xpsexp, std::vector<double> params){
    Rcpp::XPtr<funcPtrAD> xpfun(xpsexp);
    funcPtrAD func = *xpfun;
    std::vector<adouble> x(params.begin(), params.end());
    CppAD::Independent(x);
    std::vector<adouble> res = func(x); // Use function pointer - not work
    //std::vector<adouble> res = banana(x); // Use function above - works OK
    CppAD::ADFun<double> fun(x, res);
    return fun.Jacobian(params);
}

//// [[Rcpp::export]]
//std::vector<double> eval_hessian(SEXP xpsexp, std::vector<double> params, unsigned int var = 0){
//    // Retrieve user function from pointer
//    Rcpp::XPtr<funcPtrAD> xpfun(xpsexp);
//    funcPtrAD myfunc = *xpfun;
//    std::vector<adouble> params_ad(params.begin(), params.end());
//    CppAD::Independent(params_ad);
//    std::vector<adouble> res = myfunc(params_ad);
//    CppAD::ADFun<double> adfun(params_ad, res);
//    return adfun.Hessian(params, var);
//}
//

