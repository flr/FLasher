// Simple function to check that Rcpp is working properly - will delete when package better developed
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_hello_world() {

    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 3.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}
