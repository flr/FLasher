// Change the name of the object?
/* 
 * Copyright 2013 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include <RcppCommon.h>
#include <Rcpp.h>

// fwdControl class
// no need for adoubles
// two main components:
// data.frame describing controls per timestep
// array for the iterations in a timestep
// leave these as Rcpp types for simplicity

// This class won't be modified - it is a read-only class
// All methods will b consts I guess

// The target_iters is a 3D array with fixed size of second dimension (3)


// Enumerated type for the target types
// If you add something here you have to also add it to the map in control.cpp
// Also, if it's an abundance based target, add it to operatingModel::get_target_fmult_timestep(const int target_no)
enum fwdControlTargetType {
    target_f,
    target_catch,
    target_ssb,
    target_biomass
};

// Map the target type as string to the enumerated type - typedef so we can make iterators to it later
typedef std::map<std::string, fwdControlTargetType> target_map_type;



class fwdControl {
	public:
		fwdControl();
		fwdControl(SEXP fwd_control_sexp); // Used as intrusive 'as'
        operator SEXP() const; // Used as intrusive 'wrap'
		fwdControl(const fwdControl& fwdControl_source); // copy constructor to ensure that copies (i.e. when passing to functions) are deep
		fwdControl& operator = (const fwdControl& fwdControl_source); // Assignment operator for a deep copy
        // Accessors
        Rcpp::DataFrame get_target() const;
        int get_ntarget() const;
        int get_niter() const;
        // Return index values, starting at 1 (i.e. not the character string of year and season)
        int get_target_year(const int target_no) const;
        int get_target_season(const int target_no) const;
        int get_target_rel_year(const int target_no) const;
        int get_target_rel_season(const int target_no) const;
        int get_target_fishery(const int target_no) const;
        Rcpp::IntegerVector get_age_range(const int target_no) const; // Returns the age range - literally just the values in target
        double get_target_value(const int target_no, const int col, const int iter) const; // col: 1 = min, 2 = value, 3 = max
        std::vector<double> get_target_value(const int target_no, const int col) const; // gets all iters. col: 1 = min, 2 = value, 3 = max
        std::string get_target_quantity(const int target_no) const;
        fwdControlTargetType get_target_type(const int target_no) const;
        void init_target_map();
    private:
        Rcpp::DataFrame target;
        Rcpp::NumericVector target_iters; 
        //std::map<std::string, fwdControlTargetType> target_map;
        target_map_type target_map;
};


