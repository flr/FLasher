/* 
 * Copyright 2013 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include <RcppCommon.h>
#include <Rcpp.h>

// fwdControl class
// Three main components:
// data.frame describing controls per timestep (element)
// array for the iterations in a timestep (iters)
// matrix to describe what fishery / catch catches a biol (FCB)
// a map (to transfer the names of the target quantity to the target type)
// implemented as Rcpp types where possible for simplicity

// Enumerated type for the target types
// If you add something here you have to also add it to the map in control.cpp
// Also, if it's an abundance based target, add it to operatingModel::get_target_fmult_timestep(const int target_no) and to abundance_targets below 
enum fwdControlTargetType {
    target_fbar,
    target_catch,
    target_landings,
    target_discards,
    target_srp,
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
        fwdControlTargetType get_target_type(const std::string quantity) const;
        unsigned int get_target_effort_timestep(unsigned int target_no, unsigned int sim_target_no) const;
        std::vector<unsigned int> get_age_range(const unsigned int target_no, const unsigned int sim_target_no) const; // Returns the age range - just the values in target no calculation
        // FCB accessors
        Rcpp::IntegerMatrix get_FCB() const;
        Rcpp::IntegerMatrix get_FC(const int biol_no) const;
        std::vector<int> get_B(const int fishery_no, const int catch_no) const;
        unsigned int get_FCB_nrow() const;
        unsigned int get_FCB_row_no(const unsigned int fishery_no, const unsigned int catch_no, const unsigned int biol_no) const;

    private:
        Rcpp::DataFrame target;
        Rcpp::NumericVector target_iters; 
        target_map_type target_map;
        Rcpp::IntegerMatrix FCB; // an (n x 3) matrix with columns F, C and B
        // Add more abundance target types if necessary - different timing of F in the operating model
        std::vector<fwdControlTargetType> abundance_targets {target_srp, target_biomass};
};

