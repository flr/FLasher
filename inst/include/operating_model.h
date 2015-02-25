/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */
/**
 * @file   operating_model.h
 * @Author Finlay Scott (drfinlayscott@gmail.com)
 * @brief  The operating model class.
 */
#ifndef _FLFishery_
#define _FLFishery_
#include "FLFishery.h"
#endif 

#ifndef _fwdBiol_
#define _fwdBiol_
#include "fwdBiol.h"
#endif 

#ifndef _fwdSR_
#define _fwdSR_
#include "fwdSR.h"
#endif 

#ifndef _FLQuant_multidim_
#define _FLQuant_multidim_
#include "FLQuant_multidim.h"
#endif

// Need to fix control object
#include "fwdControl.h"


// Converting timestep to year and season and vice versa
// Several options
template <typename T>
void year_season_to_timestep(const int year, const int season, const FLQuant_base<T>& flq, int& timestep);

template <typename T>
void timestep_to_year_season(const int timestep, const FLQuant_base<T>& flq, int& year, int& season);

void year_season_to_timestep(const int year, const int season, const int nseason, int& timestep);
void timestep_to_year_season(const int timestep, const int nseason, int& year, int& season);

//double euclid_norm(double* x, const int size_x);
double euclid_norm(std::vector<double> x);

// A Newton Raphson solver for a function that has already been taped.
// Pass in the independent variables, tape no. and control parameters
// int newton_raphson(std::vector<double>& indep, const int adolc_tape, const int max_iters= 50, const double max_limit = 100, const double tolerance = 1e-12);
int newton_raphson(std::vector<double>& indep, CppAD::ADFun<double>& fun, const int niter, const int nsim_targets, const int max_iters= 50, const double max_limit = 100, const double tolerance = 1e-12);

/* Everything Louder Than Everything Else 
 * The Operating Model Class
 */

// Not templated as this class for projecting with the control object - must have AD for solving


/*! \brief A class for building operating models
 *
 * The operatingModel class is used for running projections and solving for effort to hit various targets.
 * It is made up of the Fisheries that are in operation, and the Biols that are fished.
 * Each Fishery is made up of one or more Catches. It is the Catches that fish the Biols.
 * A Biol can be fished by multiple Catches. The limitation is that only one Catch from a Fishery may fish a Biol.
 * A Catch can fish multiple Biols (but I am not sure that this is a good idea...).
 * Who fishes on what is determined by the ctrl member.
 */
class operatingModel {
    public:
        /* Constructors */
        /* Not really possible to write an 'as' as there is no corresponding class in FLR - need to write wrapper function - see bottom of cpp script */
		operatingModel();
        operator SEXP() const; // Used as intrusive 'wrap' - returns a list of stuff
        operatingModel(const FLFisheriesAD fisheries_in, const fwdBiolsAD biols_in, const fwdControl ctrl_in);
		operatingModel(const operatingModel& operatingModel_source); // copy constructor to ensure that copy is a deep copy - used when passing FLSs into functions
		operatingModel& operator = (const operatingModel& operatingModel_source); // Assignment operator for a deep copy

        FLQuantAD catch_q(const int fishery_no, const int catch_no, const int biol_no, const int year, const int season) const;
        FLQuantAD catch_q(const int fishery_no, const int catch_no, const int biol_no) const;
        FLQuantAD catch_q_orig(const int fishery_no, const int catch_no, const int biol_no) const;
        adouble catch_q(const int fishery_no, const int catch_no, const int biol_no, const int year, const int unit, const int season, const int area, const int iter) const;
        FLQuantAD catch_q(const int fishery_no, const int catch_no, const int biol_no, const int year_min, const int year_max, const int unit_min, const int unit_max, const int season_min, const int season_max, const int area_min, const int area_max, const int iter_min, const int iter_max) const;

        FLQuantAD get_f(const int fishery_no, const int catch_no, const int biol_no) const; 
        FLQuantAD partial_f(const int fishery_no, const int catch_no, const int biol_no) const; 
        FLQuantAD total_f(const int biol_no) const;

        FLQuantAD z(const int biol_no) const;

        void run(); 

        //void run_all_iters(); 
        
        //! Project the operating model by a single timestep
        /*!
            Project the operating model by a single timestep and update the abundances using the Baranov equation.
            Fishing and natural mortality are assumed to be constant over age through the timestep.
            Catches, landings and discards in the fisheries are calculated.
            Population abundances in the biols in the following time step are calculated including recruitment.
            @param timestep the timestep to project for
        */
        void project_timestep(const int timestep);

        //void load_ad_members(const int timestep);
        //void update_from_ad_members(const int timestep);
        
        // Timestep in which to use fmult to affect the target value
        int get_target_fmult_timestep(const int target_no);
        
        // age range indices for the f based targets
        Rcpp::IntegerVector get_target_age_range_indices(const int target_no, const int biol_no) const; // Returns the indices of the age range, starts at 0

        // Given the target no, evaluate the current value in the operatingModel
        FLQuantAD eval_target(const int target_no) const;

        // The target value we are trying to hit - either directly from the control object or a min / max / rel value calculation
        std::vector<double> calc_target_value(const int target_no) const; 

        // The target value calculations
        // fbar from a catch and fishery on a stock - i.e. partial F - will need to adapt this to include multiple biols in the future
        FLQuantAD fbar(const Rcpp::IntegerVector age_range_indices, const int fishery_no, const int catch_no, const int biol_no) const;
        // Total fbar on a biol
        FLQuantAD fbar(const Rcpp::IntegerVector age_range_indices, const int biol_no) const;
        // catches from an FLCatch and fishery on a stock 
        //FLQuantAD catches(const int fishery_no, const int catch_no) const;
        // Total catches from a biol
        //FLQuantAD catches(const int biol_no) const;
        // landings from a FLCatch in a fishery on a biol
        //FLQuantAD landings(const int fishery_no, const int catch_no) const;
        // Total landings from a biol
        //FLQuantAD landings(const int biol_no) const;
        // discards from a FLCatch in a fishery on a biol
        //FLQuantAD discards(const int fishery_no, const int catch_no) const;
        // Total discards from a biol
        //FLQuantAD discards(const int biol_no) const;

        // Total biomass from a biol
        //FLQuantAD biomass(const int biol_no) const;

        // Various ways of calculating reproductive potential
        FLQuantAD ssb(const int biol_no) const;
        //FLQuantAD ssb(const int timestep, const int unit, const int area, const int biol_no) const; // all iters in a timestep, unit and area
        //adouble ssb(const int timestep, const int unit, const int area, const int iter, const int biol_no) const; // single iter in a timestep, unit and area
        //adouble ssb(const int year, const int unit, const int season, const int area, const int iter, const int biol_no) const; // single iter in a timestep, unit and area

    private:
        FLFisheriesAD fisheries;
        fwdControl ctrl;

    protected:
        fwdBiolsAD biols; // This is protected because operatingModel is a friend of fwdBiol so we can access the SRR
};



