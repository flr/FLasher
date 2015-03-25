/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include <cppad/cppad.hpp> // the CppAD package http://www.coin-or.org/CppAD/

#include <Rcpp.h>

#define _FLQuant_base_
/*
 * FLQuant_base<T> template class
 * FLQuant_base<double> is an FLQuant
 * I was originally thinking of having FLQuantAdolc as a new class that inherits FLQuant_base<adouble>.
 * This would mean we could additional member variables and methods that were appropriate for FLQuantAdolc.
 * This idea could be further expanded to a templated AD class, FLQuantAD_base<T> that could be used for AutoDiff classes as well as adouble
 * The implementation is not difficult for some methods.
 * Of course, all constructors, copy and assignement methods need to be specialised.
 * But the get_x(), set_x(), element accessor () operators could be defined in the FLQuant_base and then used by all inherited classes with different <T>
 * The problem is with a method that requires a copy to be made. If this method is 'only' declared in the base class, then slicing occurs when called by a derived class.
 * For example, the multiplication operator, *, requires a copy of the lhs to be made (it works on the lhs object, lhs.(*)).
 * If the lhs is actually FLQAdolc which as inherited FLQuant_base<adouble>, what appears in the * method is only the base FLQuant_base<adouble> object, not the full
 * FLQAdolc object. All extra methods and variables are 'sliced' off. When the copy returns, it is only a FLQuant_base<adouble> not FLQAdolc.
 * To get round this it would be possible to define lots of extra overloaded methods (e.g. FLQuantAdolc * FLQuant; FLQuantAdolc * FLQuant; FLQuant * FLQuantAdolc).
 * This doesn't sounds like too much of a hassle but the overhead becomes substantial when we have * / + - operators, as well as ones that only a double, or an adouble.
 * We can revisit this if necessary. For the time being, FLQuantAdolc is just FLQuant_base<adouble>.
 *
 * 08/09/2014
 * Now using CppAD instead of ADOL-C
 * All reference to 'adouble' should be read 'CppAD::AD<double>'
 * The original code is in https://github.com/drfinlayscott/FLRcppAdolc 
 */

// Renaming adouble (ADOL-C type) so can easily use original ADOL-C based code
typedef CppAD::AD<double> adouble;


/*! \brief The FLQuant class
 *
 * This class is similar in dimension and behaviour to the R FLQuant class.
 * The values can either be of type double or adouble, the latter used with the CppAD library.
 * Basic FLQuant manipulation and mathematical operations are available.
 *
 */
template <typename T>
class FLQuant_base {
	public:
        /* Constructors */
		FLQuant_base();
		FLQuant_base(SEXP flq_sexp); // Used as intrusive 'as'
        operator SEXP() const; // Used as intrusive 'wrap'
		FLQuant_base(const FLQuant_base& FLQuant_base_source); // copy constructor to ensure that copies (i.e. when passing to functions) are deep
		FLQuant_base& operator = (const FLQuant_base& FLQuant_source); // Assignment operator for a deep copy
        FLQuant_base(const unsigned int nquant, const unsigned int nyear, const unsigned int nunit, const unsigned int nseason, const unsigned int narea, const unsigned int niter); // Make an empty FLQuant

        template <typename T2>
		FLQuant_base(const FLQuant_base<T2>& FLQuant_source); // Specialised to make an FLQuantAdolc / AD from an FLQuant

		/* Get accessors */
        std::vector<T> get_data() const;
		std::string get_units() const;
        //Rcpp::IntegerVector get_dim() const;
        std::vector<unsigned int> get_dim() const;
        Rcpp::List get_dimnames() const;
		unsigned int get_size() const;
		unsigned int get_nquant() const;
		unsigned int get_nyear() const;
		unsigned int get_nunit() const;
		unsigned int get_nseason() const;
		unsigned int get_narea() const;
		unsigned int get_niter() const;
		int get_data_element(const int quant, const int year, const int unit, const int season, const int area, int iter) const;

		/* Set accessors */
		void set_data(const std::vector<T>& data_in);
        void set_dimnames(const Rcpp::List& dimnames_in);
        void set_units(const std::string& units_in);

        /* () get accessors */
		T operator () (const unsigned int element) const; // only gets an element so const reinforced - 
		T operator () (const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area, const unsigned int iter) const; // only gets an element so const reinforced 
		T operator () (const std::vector<unsigned int> indices) const; // For all the elements - must be of length 6 
		FLQuant_base<T> operator () (const unsigned int quant_min, const unsigned int quant_max, const unsigned int year_min, const unsigned int year_max, const unsigned int unit_min, const unsigned int unit_max, const unsigned int season_min, const unsigned int season_max, const unsigned int area_min, const unsigned int area_max, const unsigned int iter_min, const unsigned int iter_max) const; // Subsetting
        FLQuant_base<T> operator () (const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const; // Neater subsetting
		FLQuant_base<T> operator () (const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area) const; // Access all iters

        /* () get and set accessors */
		T& operator () (const unsigned int element); // gets and sets an element so const not reinforced
		T& operator () (const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area, const unsigned int iter); // gets and sets an element so const not reinforced
		T& operator () (const std::vector<unsigned int> indices); // For all the elements - must be of length 6 
		//FLQuant_base<T>& operator () (const unsigned int quant, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area); // Access all iters
        /* Fill methods */
        void fill(const T value);
        template <typename T2>
        void fill(const T2 value); // specialisation to fill FLQuantAD with double

        /* Mathematical operators */

        // Multiplication
        FLQuant_base<T>& operator *= (const FLQuant_base<T>& rhs);
        FLQuant_base<T>& operator *= (const T& rhs);
        // For the special case of FLQuant_base<adouble> *= FLQuant_base<double>
        template <typename T2>
        FLQuant_base<T>& operator *= (const FLQuant_base<T2>& rhs);
        // For the special case of FLQuant_base<adouble> *= double
        template <typename T2>
        FLQuant_base<T>& operator *= (const T2& rhs);
        // Return same type as itself
        FLQuant_base<T> operator * (const FLQuant_base<T>& rhs) const;
        FLQuant_base<T> operator * (const T& rhs) const;

        // Division
        FLQuant_base<T>& operator /= (const FLQuant_base<T>& rhs);
        FLQuant_base<T>& operator /= (const T& rhs);
        // For the special case of FLQuant_base<adouble> *= FLQuant_base<double>
        template <typename T2>
        FLQuant_base<T>& operator /= (const FLQuant_base<T2>& rhs);
        // For the special case of FLQuant_base<adouble> *= double
        template <typename T2>
        FLQuant_base<T>& operator /= (const T2& rhs);
        // Return same type as itself
        FLQuant_base<T> operator / (const FLQuant_base<T>& rhs) const;
        FLQuant_base<T> operator / (const T& rhs) const;

        // Subtraction
        FLQuant_base<T>& operator -= (const FLQuant_base<T>& rhs);
        FLQuant_base<T>& operator -= (const T& rhs);
        // For the special case of FLQuant_base<adouble> *= FLQuant_base<double>
        template <typename T2>
        FLQuant_base<T>& operator -= (const FLQuant_base<T2>& rhs);
        // For the special case of FLQuant_base<adouble> *= double
        template <typename T2>
        FLQuant_base<T>& operator -= (const T2& rhs);
        // Return same type as itself
        FLQuant_base<T> operator - (const FLQuant_base<T>& rhs) const;
        FLQuant_base<T> operator - (const T& rhs) const;

        // Addition
        FLQuant_base<T>& operator += (const FLQuant_base<T>& rhs);
        FLQuant_base<T>& operator += (const T& rhs);
        // For the special case of FLQuant_base<adouble> += FLQuant_base<double>
        template <typename T2>
        FLQuant_base<T>& operator += (const FLQuant_base<T2>& rhs);
        // For the special case of FLQuant_base<adouble> *= double
        template <typename T2>
        FLQuant_base<T>& operator += (const T2& rhs);
        // Return same type as itself
        FLQuant_base<T> operator + (const FLQuant_base<T>& rhs) const;
        FLQuant_base<T> operator + (const T& rhs) const;

        /* Other methods */
        int match_dims(const FLQuant_base<T>& flq) const;
        template <typename T2>
        int match_dims(const FLQuant_base<T2>& flq) const;
        FLQuant_base<T> propagate_iters(const int iters) const;

        /* begin and end and const versions for iterators */
        typedef typename std::vector<T>::iterator iterator;
        iterator begin();
        iterator end();
        typedef typename std::vector<T>::const_iterator const_iterator;
        const_iterator begin() const;
        const_iterator end() const;

    protected:
        std::vector<T> data;
		std::string units;	
        //Rcpp::IntegerVector dim;
        std::vector<unsigned int> dim;
        Rcpp::List dimnames;
};


typedef FLQuant_base<double> FLQuant;
// typedef FLQuant_base<adouble> FLQuantAdolc;
typedef FLQuant_base<adouble> FLQuantAD;

//---------- Other useful functions ------------------------

//int dim_matcher(const Rcpp::IntegerVector a, const Rcpp::IntegerVector b);
//int dim5_matcher(const Rcpp::IntegerVector a, const Rcpp::IntegerVector b);
int dim_matcher(const std::vector<unsigned int> a, const std::vector<unsigned int> b);
int dim5_matcher(const std::vector<unsigned int> a, const std::vector<unsigned int> b);


template <typename T>
std::string number_to_string (T number);

// Turn an FLPar (straight from R) into FLQuant
FLQuant FLPar_to_FLQuant(SEXP flp); 

//------------ Non-member arithmetic methods with mixed types --------------

/* Canonical form: Type operator*(const Type &lhs, const Type &rhs); 
* double gets swallowed up by whatever is multiplying it.
* This means that the operations involving a double need to be individually specified.
* Need to be careful of ambiguities arise,
* FLQuant_base<anything>  = FLQuant_base<double> * FLQuant_base<anything>
* FLQuant = FLQuant * double
* FLQuantAdolc = FLQuantAdolc * adouble
* FLQuantAD = FLQuantAD * adouble
* FLQuant_base<> = <> * FLQuant_base
*/

// The templated functions are all instantiated at the bottom of FLQuant_base.cpp

// Multiplication
template <typename T>
FLQuant_base<T> operator * (const FLQuant_base<double>& lhs, const FLQuant_base<T>& rhs);
template <typename T>
FLQuant_base<T> operator * (const FLQuant_base<T>& lhs, const FLQuant_base<double>& rhs);
// No template - resolves ambiguity arising from double * FLQ<T> and <T> * FLQ<T> (if T is double, which one is called?)
FLQuant_base<double> operator * (const double& lhs, const FLQuant_base<double>& rhs);
// For FLQuant_base<adouble> = adouble * FLQuant_base<adouble>
template <typename T>
FLQuant_base<T> operator * (const T& lhs, const FLQuant_base<T>& rhs);
// For FLQuant_base<adouble> = double * FLQuant_base<adouble>
template <typename T>
FLQuant_base<T> operator * (const double& lhs, const FLQuant_base<T>& rhs);
template <typename T>
FLQuant_base<T> operator * (const FLQuant_base<T>& lhs, const double& rhs);
// For FLQuant_base<adouble> = FLQuant_base<double> * adouble
template <typename T>
FLQuant_base<T> operator * (const FLQuant_base<double>& lhs, const T& rhs);
template <typename T>
FLQuant_base<T> operator * (const T& lhs, const FLQuant_base<double>& rhs);

// Division
template <typename T>
FLQuant_base<T> operator / (const FLQuant_base<double>& lhs, const FLQuant_base<T>& rhs);
template <typename T>
FLQuant_base<T> operator / (const FLQuant_base<T>& lhs, const FLQuant_base<double>& rhs);
// No template - resolves ambiguity
FLQuant_base<double> operator / (const double& lhs, const FLQuant_base<double>& rhs);
// For FLQuant_base<adouble> = adouble / FLQuant_base<adouble>
template <typename T>
FLQuant_base<T> operator / (const T& lhs, const FLQuant_base<T>& rhs);
// For FLQuant_base<adouble> = double / FLQuant_base<adouble>
template <typename T>
FLQuant_base<T> operator / (const double& lhs, const FLQuant_base<T>& rhs);
template <typename T>
FLQuant_base<T> operator / (const FLQuant_base<T>& lhs, const double& rhs);
// For FLQuant_base<adouble> = FLQuant_base<double> / adouble
template <typename T>
FLQuant_base<T> operator / (const FLQuant_base<double>& lhs, const T& rhs);
template <typename T>
FLQuant_base<T> operator / (const T& lhs, const FLQuant_base<double>& rhs);

// Subtraction
template <typename T>
FLQuant_base<T> operator - (const FLQuant_base<double>& lhs, const FLQuant_base<T>& rhs);
template <typename T>
FLQuant_base<T> operator - (const FLQuant_base<T>& lhs, const FLQuant_base<double>& rhs);
// No template - resolves ambiguity
FLQuant_base<double> operator - (const double& lhs, const FLQuant_base<double>& rhs);
// For FLQuant_base<adouble> = adouble - FLQuant_base<adouble>
template <typename T>
FLQuant_base<T> operator - (const T& lhs, const FLQuant_base<T>& rhs);
// For FLQuant_base<adouble> = double - FLQuant_base<adouble>
template <typename T>
FLQuant_base<T> operator - (const double& lhs, const FLQuant_base<T>& rhs);
template <typename T>
FLQuant_base<T> operator - (const FLQuant_base<T>& lhs, const double& rhs);
// For FLQuant_base<adouble> = FLQuant_base<double> - adouble
template <typename T>
FLQuant_base<T> operator - (const FLQuant_base<double>& lhs, const T& rhs);
template <typename T>
FLQuant_base<T> operator - (const T& lhs, const FLQuant_base<double>& rhs);

// Addition
template <typename T>
FLQuant_base<T> operator + (const FLQuant_base<double>& lhs, const FLQuant_base<T>& rhs);
template <typename T>
FLQuant_base<T> operator + (const FLQuant_base<T>& lhs, const FLQuant_base<double>& rhs);
// No template - resolves ambiguity arising from double + FLQ<T> and <T> + FLQ<T> (if T is double, which one is called?)
FLQuant_base<double> operator + (const double& lhs, const FLQuant_base<double>& rhs);
// For FLQuant_base<adouble> = adouble + FLQuant_base<adouble>
template <typename T>
FLQuant_base<T> operator + (const T& lhs, const FLQuant_base<T>& rhs);
// For FLQuant_base<adouble> = double + FLQuant_base<adouble>
template <typename T>
FLQuant_base<T> operator + (const double& lhs, const FLQuant_base<T>& rhs);
template <typename T>
FLQuant_base<T> operator + (const FLQuant_base<T>& lhs, const double& rhs);
// For FLQuant_base<adouble> = FLQuant_base<double> * adouble
template <typename T>
FLQuant_base<T> operator + (const FLQuant_base<double>& lhs, const T& rhs);
template <typename T>
FLQuant_base<T> operator + (const T& lhs, const FLQuant_base<double>& rhs);

// Other arithmetic operations
template <typename T>
FLQuant_base<T> log(const FLQuant_base<T>& flq);
template <typename T>
FLQuant_base<T> exp(const FLQuant_base<T>& flq);

// Shortcut methods
template <typename T>
FLQuant_base<T> year_sum(const FLQuant_base<T>& flq);

template <typename T>
FLQuant_base<T> quant_sum(const FLQuant_base<T>& flq);

// Means over various dimensions
template <typename T>
FLQuant_base<T> quant_mean(const FLQuant_base<T>& flq);  // collapse the quant dimension


template <typename T>
FLQuant_base<T> max_quant(const FLQuant_base<T>& flq);

// This only makes sense if all the values ae positive
template <typename T>
FLQuant_base<T> scale_by_max_quant(const FLQuant_base<T>& flq);

