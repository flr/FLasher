// Example to test that CppAD is working
// From:
// http://www.coin-or.org/CppAD/Doc/get_started.cpp.xml 

#include <iostream>      // standard input/output 
#include <vector>        // standard vector
#include <cppad/cppad.hpp> // the CppAD package http://www.coin-or.org/CppAD/

#include <Rcpp.h>


// [[Rcpp::export]]
void cppad_found(){
    using CppAD::AD;           // use AD as abbreviation for CppAD::AD
    using std::vector;         // use vector as abbreviation for std::vector
    size_t n = 1;
    vector< AD<double> > X(n); // vector of domain space variables
    return;
}

// f(x) = a_0 + a_1 x^(1) + a_2 x^(2) ... a_k-1 x^(k-1)
namespace { 
      // define y(x) = Poly(a, x) in the empty namespace
      template <class Type>
      Type Poly(const std::vector<double> &a, const Type &x)
      {     size_t k  = a.size();
            Type y   = 0.;  // initialize summation
            Type x_i = 1.;  // initialize x^i
            size_t i;
            for(i = 0; i < k; i++)
            {     y   += a[i] * x_i;  // y   = y + a_i * x^i
                  x_i *= x;           // x_i = x_i * x
            }
            return y;
      }
}

// int main(void)
// [[Rcpp::export]]
std::vector<double> poly(unsigned int k_in, double x_in)
{
    Rprintf("In poly\n");
    using CppAD::AD;           // use AD as abbreviation for CppAD::AD
    using std::vector;         // use vector as abbreviation for std::vector
    size_t i;                  // a temporary index

    // vector of polynomial coefficients
    //size_t k = 5;              // number of polynomial coefficients
    size_t k = k_in;              // number of polynomial coefficients
    vector<double> a(k);       // vector of polynomial coefficients
    for(i = 0; i < k; i++)
        a[i] = 1.;           // value of polynomial coefficients

    // domain space vector
    size_t n = 1;              // number of domain space variables
    vector< AD<double> > X(n); // vector of domain space variables
    X[0] = 3.;                 // value corresponding to operation sequence

    // declare independent variables and start recording operation sequence
    CppAD::Independent(X);

    // range space vector
    size_t m = 1;              // number of ranges space variables
    vector< AD<double> > Y(m); // vector of ranges space variables
    Y[0] = Poly(a, X[0]);      // value during recording of operations

    // Check X value here?
	// Rprintf("X value while taping: %f\n", Value(X[0]));
    // Seems OK
    // http://www.coin-or.org/CppAD/Doc/value.cpp.xml 
    // Suggested it would fail

    // store operation sequence in f: X -> Y and stop recording
    CppAD::ADFun<double> f(X, Y);

    // compute derivative using operation sequence stored in f
    vector<double> jac(m * n); // Jacobian of f (m by n matrix)
    vector<double> x(n);       // domain space vector
    //x[0] = 3.;                 // argument value for derivative
    x[0] = x_in;                 // argument value for derivative
    jac  = f.Jacobian(x);      // Jacobian for operation sequence

    // print the results
    //std::cout << "f'(3) computed by CppAD = " << jac[0] << std::endl;
    Rprintf("f'(%f) computed by CppAD = %f\n", x[0], jac[0]);
    Rprintf("Poly %f computed by CppAD = %f\n", Value(X[0]), Value(Y[0])); // Trying to print AD container

    return jac;
}






