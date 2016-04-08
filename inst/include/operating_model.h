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
        unsigned int get_niter() const;
        FLQuantAD srp(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuant f_prop_spwn(const int fishery_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        std::vector<adouble> calc_rec(const unsigned int biol_no, const unsigned int unit, const unsigned int rec_timestep) const;
        FLQuantAD get_f(const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuantAD get_f(const int fishery_no, const int catch_no, const int biol_no) const; 
        FLQuantAD get_f(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuantAD get_f(const int biol_no) const;
        void project_biols(const int timestep); // Uses effort in previous timestep
        void project_fisheries(const int timestep); // Uses effort in that timestep
        void run(const double effort_mult_initial, const double indep_min, const double); 

        FLQuantAD eval_om(const fwdControlTargetType target_type, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        // The actual current target values in the OM - to be compared to the desired values
        std::vector<adouble> get_target_value_hat(const int target_no) const; 
        std::vector<adouble> get_target_value_hat(const int target_no, const int sim_target_no) const; 
        // The target value we are trying to hit - either directly from the control object or a min / max calculation using the current OM
        std::vector<adouble> get_target_value(const int target_no) const; // All iters for all sim targets
        std::vector<adouble> get_target_value(const int target_no, const int sim_target_no) const; // All iters for a sim target
        

        // Timestep of effort which drives the target value
        // int get_target_effort_timestep(const int target_no);
        
        // age range indices for age-based targets (e.g. fbar)
        // Returns the indices of the age range, starts at 0
        std::vector<unsigned int> get_target_age_range_indices(const unsigned int target_no, const unsigned int sim_target_no) const; 

        // The target value calculations
        // Partial fbar of a single catch on a single biol
        FLQuantAD fbar(const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        // Total fbar on a biol (possibly from multiple catches)
        FLQuantAD fbar(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;

        // Total catches / landings / discards from a biol
        FLQuantAD landings(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuantAD discards(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;
        FLQuantAD catches(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const;

        // Total biomass from a biol
        //FLQuantAD biomass(const int biol_no) const;


    private:
        FLFisheriesAD fisheries;
        fwdControl ctrl;

    protected:
        fwdBiolsAD biols; // This is protected because operatingModel is a friend of fwdBiol so we can access the SRR
};



