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

#include "fwdControl.h"
#include "solver.h"

// Converting timestep to year and season and vice versa
// Several options
template <typename T>
void year_season_to_timestep(const unsigned int year, const unsigned int season, const FLQuant_base<T>& flq, unsigned int& timestep);

template <typename T>
void timestep_to_year_season(const unsigned int timestep, const FLQuant_base<T>& flq, unsigned int& year, unsigned int& season);

void year_season_to_timestep(const unsigned int year, const unsigned int season, const unsigned int nseason, unsigned int& timestep);
void timestep_to_year_season(const unsigned int timestep, const unsigned int nseason, unsigned int& year, unsigned int& season);

/* Everything Louder Than Everything Else 
 * The Operating Model Class
 */

/*! \brief A class for building operating models
 *
 * The operatingModel class is used for running projections and solving for effort to hit various targets.
 * It is made up of the Fisheries that are in operation, and the Biols that are fished.
 * Each Fishery is made up of one or more Catches. It is the Catches that fish the Biols.
 * A Biol can be fished by multiple Catches. The limitation is that only one Catch from a Fishery may fish a Biol.
 * A Catch can fish multiple Biols (but I am not sure that this is a good idea...).
 * Who fishes on what is determined by the ctrl member.
 * Not templated as this class for projecting with the control object - must have AD for solving.
 */
class operatingModel {
    public:
        /* Constructors */
        /* Not really possible to write an 'as' as there is no corresponding class in FLR - need to write wrapper function - see bottom of cpp script */
		operatingModel();
        operatingModel(const FLFisheriesAD fisheries_in, const fwdBiolsAD biols_in, const fwdControl ctrl_in);
		operatingModel(const operatingModel& operatingModel_source); // copy constructor to ensure that copy is a deep copy - used when passing FLSs into functions
		operatingModel& operator = (const operatingModel& operatingModel_source); // Assignment operator for a deep copy
        operator SEXP() const; // Used as intrusive 'wrap' - returns a list of stuff

        // Methods
        FLQuantAD srp(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuant f_prop_spwn(const int fishery_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        std::vector<adouble> calc_rec(const int biol_no, const int timestep) const;
        FLQuantAD get_f(const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuantAD get_f(const int fishery_no, const int catch_no, const int biol_no) const; 
        FLQuantAD get_f(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuantAD get_f(const int biol_no) const;
        void project_biols(const int timestep); // Uses effort in previous timestep
        void project_fisheries(const int timestep); // Uses effort in that timestep
        void run(const double effort_mult_initial, const double indep_min = 0, const double indep_max = 1e9); 

        FLQuantAD eval_om(const fwdControlTargetType target_type, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;

        // The target value we are trying to hit - either directly from the control object or a min / max calculation using the current OM
        // ADD MIN MAX BACK IN
        //std::vector<double> get_target_value(const int target_no) const; // All iters for all sim targets
        //std::vector<double> get_target_value(const int target_no, const int sim_target_no) const; // All iters for a sim target
        // Given the target no, evaluate the current value in the operatingModel
        //FLQuantAD eval_target(const unsigned int target_no, const unsigned int sim_target_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max, const bool relative_target = false) const;
        // The actual current target values in the OM - to be compared to the desired values
        //std::vector<adouble> get_target_value_hat(const int target_no) const; 
        //std::vector<adouble> get_target_value_hat(const int target_no, const int sim_target_no) const; 
        
        // Redundant methods?
        // Currently not using catch_q method - instead it is embedded in get_f()
        // It could be useful if we wanted to use different catch_q methods rather than fixing it in get_f
        //FLQuantAD catch_q(const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        //FLQuantAD catch_q(const int fishery_no, const int catch_no, const int biol_no) const;
        //adouble catch_q(const int fishery_no, const int catch_no, const int biol_no, const unsigned int year, const unsigned int unit, const unsigned int season, const unsigned int area, const unsigned int iter) const;
        //FLQuantAD partial_f(const int fishery_no, const int catch_no, const int biol_no) const; 
        //FLQuantAD partial_f(const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const; 
        //FLQuantAD z(const int biol_no) const;
        //FLQuantAD z(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;

        //void run_all_iters(); 
        
        //! Project the operating model by a single timestep
        /*!
            Project the operating model by a single timestep and update the abundances using the Baranov equation.
            Fishing and natural mortality are assumed to be constant over age through the timestep.
            Catches, landings and discards in the fisheries are calculated.
            Population abundances in the biols in the following time step are calculated including recruitment.
            @param timestep the timestep to project for
        */
        //void project_timestep(const int timestep);

        // Timestep of effort which drives the target value
        // int get_target_effort_timestep(const int target_no);
        
        // age range indices for the f based targets
        // Returns the indices of the age range, starts at 0
        //std::vector<unsigned int> get_target_age_range_indices(const unsigned int target_no, const unsigned int sim_target_no, const unsigned int biol_no) const; 



        // The target value calculations
        // Partial fbar of a single catch on a single biol
        //FLQuantAD fbar(const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        // Total fbar on a biol (possibly from multiple catches)
        //FLQuantAD fbar(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;

        // catches from an FLCatch and fishery on a stock 
        //FLQuantAD catches(const int fishery_no, const int catch_no) const;
        // Total catches / landings / discards from a biol
        FLQuantAD landings(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuantAD discards(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuantAD catches(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;

        // Total biomass from a biol
        //FLQuantAD biomass(const int biol_no) const;

        // Various ways of calculating reproductive potential
        //FLQuantAD ssb(const int biol_no) const;
        //FLQuantAD ssb(const int biol_no, &FLQuantAD total_f, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        //FLQuantAD ssb(const int timestep, const int unit, const int area, const int biol_no) const; // all iters in a timestep, unit and area
        //adouble ssb(const int timestep, const int unit, const int area, const int iter, const int biol_no) const; // single iter in a timestep, unit and area
        //adouble ssb(const int year, const int unit, const int season, const int area, const int iter, const int biol_no) const; // single iter in a timestep, unit and area

    private:
        FLFisheriesAD fisheries;
        fwdControl ctrl;

    protected:
        fwdBiolsAD biols; // This is protected because operatingModel is a friend of fwdBiol so we can access the SRR
};



