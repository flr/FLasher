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
// data.frame describing controls per timestep (element)
// array for the iterations in a timestep (iters)
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
    target_landings,
    target_discards,
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

        // For mapping target string to type
        void init_target_map();

        // Accessors
        Rcpp::DataFrame get_target() const;
        unsigned int get_ntimestep() const;
        unsigned int get_ntarget() const;
        unsigned int get_niter() const;
        unsigned int get_nsim_target(unsigned int target_no) const;
        unsigned int get_target_row(unsigned int target_no, unsigned int sim_target_no) const;
        std::vector<unsigned int> get_target_row(unsigned int target_no) const;
        // Rcpp::IntegerVector to ensure that NA is properly handled (std::vector does not work properly with Rcpp::IntegerVector::is_na())
        Rcpp::IntegerVector get_target_int_col(const int target_no, const std::string col) const;
        unsigned int get_target_int_col(const int target_no, const int sim_target_no, const std::string col) const;
        // Rcpp::NumericVector to ensure that NA is properly handled (std::vector does not work properly with Rcpp::NumericVector::is_na())
        Rcpp::NumericVector get_target_num_col(const int target_no, const std::string col) const;
        double get_target_num_col(const int target_no, const int sim_target_no, const std::string col) const;

        std::vector<double> get_target_value(const int target_no, const int col) const; // gets all iters for all simultaneous targets. col: 1 = min, 2 = value, 3 = max
        std::vector<double> get_target_value(const int target_no, const int sim_target_no, const int col) const; // gets all iters for one simultaneous target. col: 1 = min, 2 = value, 3 = max

        std::string get_target_quantity(const int target_no, const int sim_target_no) const;
        fwdControlTargetType get_target_type(const int target_no, const int sim_target_no) const;

        unsigned int get_target_effort_timestep(unsigned int target_no, unsigned int sim_target_no) const;
        
        
        std::vector<unsigned int> get_age_range(const int target_no, const int sim_target_no) const; // Returns the age range - literally just the values in target

        // FCB accessors
        Rcpp::IntegerMatrix get_FC(const int biol_no) const;
        std::vector<int> get_B(const int fishery_no, const int catch_no) const;
    private:
        // Not bothering with the R structure of fwdControl@target@iters and @element
        Rcpp::DataFrame target;
        Rcpp::NumericVector target_iters; 
        //std::map<std::string, fwdControlTargetType> target_map;
        target_map_type target_map;
        Rcpp::IntegerMatrix FCB; // an (n x 3) matrix with columns F, C and B
        // Add more abundance target types if necessary
        std::vector<fwdControlTargetType> abundance_targets {target_ssb, target_biomass};
};


