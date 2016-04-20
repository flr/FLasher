/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

// For timing functions
#include <time.h>

#include "../inst/include/operating_model.h"

/*------------------------------------------------------------*/
// operatingModel class

/*! \brief Empty constructor that creates empty members
 */
operatingModel::operatingModel(){
    biols = fwdBiolsAD();
    fisheries = FLFisheriesAD();
    ctrl = fwdControl();
}

/*! \brief Main constructor for the operatingModel
 *
 * Checks dims of the arguments and everything is OK, creates an operatingModel.
 * The dimensions of the constituent member's FLQuants must all match along dimensions 1 - 5 else it throws and error.
 *
 * \param fisheries_in The fisheries.
 * \param biols_in The biological stocks.
 * \param ctrl_in The control object that controls the projections.
 */
operatingModel::operatingModel(const FLFisheriesAD fisheries_in, const fwdBiolsAD biols_in, const fwdControl ctrl_in){
    // This requires a lot of checks...
    // Iters in all numbers and efforts must be the same (cannot be n or 1, just n)
    // Age structure and units of catches catching biols must be the same
    // Year and seasons must be the same for all objects
    // Start with the catches - compare everything to the first fishery / catch
    std::vector<unsigned int> landings_dim11 = fisheries_in(1,1).landings_n().get_dim();
    std::vector<unsigned int> discards_dim11 = fisheries_in(1,1).discards_n().get_dim();
    std::vector<unsigned int> effort_dim1 = fisheries_in(1).effort().get_dim();
    for (int fishery_no=1; fishery_no <= fisheries_in.get_nfisheries(); ++fishery_no){
        std::vector<unsigned int> effort_dim = fisheries_in(fishery_no).effort().get_dim();
        for (int catch_no=1; catch_no <= fisheries_in(fishery_no).get_ncatches(); ++catch_no){
            std::vector<unsigned int> landings_dim = fisheries_in(fishery_no,catch_no).landings_n().get_dim();
            std::vector<unsigned int> discards_dim = fisheries_in(fishery_no,catch_no).discards_n().get_dim();
            if ((landings_dim11[1] != landings_dim[1]) | (landings_dim11[3] != landings_dim[3])){
                Rcpp::stop("In operatingModel constructor. All catches in all fisheries must have the same year and season range.\n");
            }
            if ((landings_dim[5] != effort_dim1[5]) | (discards_dim[5] != effort_dim1[5]) | (effort_dim[5] != effort_dim1[5])){
                Rcpp::stop("In operatingModel constructor. All landings, discards and effort must have the same number of iterations.\n");
            }
        }
    }
    // Then the biols
    for (int biol_no=1; biol_no <= biols_in.get_nbiols();  ++biol_no){
        std::vector<unsigned int> biol_dim = biols_in(biol_no).n().get_dim();
        if ((landings_dim11[1] != biol_dim[1]) | (landings_dim11[3] != biol_dim[3])){
            Rcpp::stop("In operatingModel constructor. All biols and catches must have the same year and season range.\n");
        }
        // While we're checking biols, check the residuals are the right size
        std::vector<unsigned int> res_dim = biols_in(biol_no).get_srr().get_residuals().get_dim();
        // Loop over and check 2 - 5
        for (int dim_counter = 1; dim_counter < 5; ++dim_counter){
            if(res_dim[dim_counter] != biol_dim[dim_counter]){
                Rcpp::stop("In operatingModel constructor. Problem with biol: %i. Dimensions 2-5 of residuals must match those of biol.\n", biol_no);
            }
        }
        // Residuals iters 1 or n
        if (!((res_dim[5] == 1) | (res_dim[5] == biol_dim[5]))){
                Rcpp::stop("In operatingModel constructor. Problem with biol: %i. Iterations of residuals must be 1 or match those of biol.\n", biol_no);
        }
        // Check age structure for Biol and Catch dims = fisheries catching biols must have the same dims
        Rcpp::IntegerMatrix FC = ctrl_in.get_FC(biol_no);
        for (int FC_row = 0; FC_row < FC.nrow(); ++FC_row){
            std::vector<unsigned int> landings_dim = fisheries_in(FC(FC_row,0), FC(FC_row,1)).landings_n().get_dim();
            // Check age structure
            if((biol_dim[0] != landings_dim[0])){
                Rcpp::stop("In operatingModel constructor. Problem with biol: %i. Age structure must be the same as the catch dims\n", biol_no);
            }
            if(biol_dim[2] != landings_dim[2]){
                Rcpp::stop("In operatingModel constructor. Problem with biol: %i. Must have same number of Units as the catch\n", biol_no);
            }
        }
        // Check iterations of the abundance
        if (biol_dim[5] != effort_dim1[5]){
            Rcpp::stop("In operatingModel constructor.  Problem with biol: %i. Number of iterations in abundance must match effort\n", biol_no);
        }
    }
    // Iterations in the control object must be 1 or n
    int ctrl_iters = ctrl_in.get_niter();
    if ((ctrl_iters != effort_dim1[5]) & (ctrl_iters != 1)){
        Rcpp::stop("In operatingModel constructor. Iterations in control object must be 1 or same as effort iterations.\n");
    }
    // Everything is good - set the members
    biols = biols_in;
    fisheries = fisheries_in;
    ctrl = ctrl_in;
}

/*! \brief The copy constructor
 *
 * Ensures a deep copy and prevents members being pointed at by multiple instances
 * \param operatingModel_source The object to be copied.
 */
operatingModel::operatingModel(const operatingModel& operatingModel_source){
    biols = operatingModel_source.biols;
    fisheries = operatingModel_source.fisheries;
    ctrl = operatingModel_source.ctrl;
}

/*! \brief The assignment operator 
 *
 * Ensures a deep copy and prevents members being pointed at by multiple instances
 * \param operatingModel_source The object to be copied.
 */
operatingModel& operatingModel::operator = (const operatingModel& operatingModel_source){
	if (this != &operatingModel_source){
        biols = operatingModel_source.biols;
        fisheries = operatingModel_source.fisheries;
        ctrl = operatingModel_source.ctrl;
	}
	return *this;
}

/*! \brief Intrusive wrap
 *
 * Allows a direct return of an operatingModel to R.
 * It returns an R list of components.
 */
operatingModel::operator SEXP() const{
    //Rprintf("Wrapping operatingModel.\n");
    return Rcpp::List::create(
                            Rcpp::Named("biols", biols),
                            Rcpp::Named("fisheries", fisheries),
                            Rcpp::Named("ctrl", ctrl));
}

/*! \brief Returns the number of iterations
 * 
 * Number of iterations is equal to the number of iterations in the effort of the first fleet 
 * (must be the same for all fleets and for all *_n members).
 */
unsigned int operatingModel::get_niter() const{
    return fisheries(1).effort().get_niter();
}

/*! \brief Calculates the spawning recruitment potential of a biol at the time of spawning
 *
 * Calculated as SSB: N*mat*wt*exp(-Fprespwn - m*spwn) summed over age dimension
 * where the natural mortality m is assumed to be constant over the timestep
 * and Fprespwn is how much fishing mortality happened before spawning
 * \param biol_no The position of the biol in the biols list (starting at 1)
 * \param indices_min The minimum indices: year, unit etc (length 5)
 * \param indices_max The maximum indices: year, unit etc (length 5)
 */
FLQuantAD operatingModel::srp(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
    //Rprintf("Getting SRP\n");
    // Check indices_min and indices_max are of length 5
    if (indices_min.size() != 5 | indices_max.size() != 5){
        Rcpp::stop("In operatingModel::srp subsetter. Indices not of length 5\n");
    }
    // Add age range to input indices
    std::vector<unsigned int> qindices_min = indices_min;
    qindices_min.insert(qindices_min.begin(), 1);
    std::vector<unsigned int> dim = biols(biol_no).n().get_dim();
    std::vector<unsigned int> qindices_max = indices_max;
    qindices_max.insert(qindices_max.begin(), dim[0]);
    // Make dim of the Fprespwn object: max - min + 1
    std::vector<unsigned int> qdim(6,0.0);
    std::transform(qindices_max.begin(), qindices_max.end(), qindices_min.begin(), qdim.begin(), [](unsigned int x, unsigned int y){return x-y+1;});
    FLQuantAD f_pre_spwn(qdim);
    // Get Fprespawn = F * Fpropspawn for each F fishing the biol
    // 1. ID which F are fishing that B (timing is determined by hperiod and spwn)
    // 2. Get F(F,C,B)
    // 3. Get Fpropspawn(F,C,B)
    // 4. sum (F * Fpropspawn)
    const Rcpp::IntegerMatrix FC =  ctrl.get_FC(biol_no);
    //Rprintf("Getting f_pre_spwn\n");
    for (unsigned int f_counter=0; f_counter < FC.nrow(); ++f_counter){
        FLQuantAD tempf = get_f(FC(f_counter,0), FC(f_counter,1), biol_no, qindices_min, qindices_max); 
        FLQuant temp_prop_spwn = f_prop_spwn(FC(f_counter,0), biol_no, indices_min, indices_max);
        FLQuantAD temp_propf = sweep_mult(tempf, temp_prop_spwn);
        f_pre_spwn = f_pre_spwn + temp_propf;
    }
    // Get m pre spwn - need to adjust subsetter for the first dimension
    std::vector<unsigned int> spwn_indices_max = qindices_max;
    spwn_indices_max[0] = 1;
    FLQuant m_pre_spwn = sweep_mult(biols(biol_no).m(qindices_min, qindices_max), biols(biol_no).spwn(qindices_min, spwn_indices_max));
    // Get srp: N*mat*wt*exp(-Fprespwn - m*spwn) summed over age dimension
    FLQuantAD srp = quant_sum(
        biols(biol_no).n(qindices_min, qindices_max) *
        biols(biol_no).wt(qindices_min, qindices_max) *
        biols(biol_no).mat(qindices_min, qindices_max) * exp((-1.0 * f_pre_spwn) - m_pre_spwn));
    //Rprintf("Got SRP\n");
    return srp;
}

/*! \brief Calculates the proportion of fishing mortality that occurs before the stock spawns
 *
 * Given by the start and stop time of fishing (hperiod member of FLFishery) and time of spawning (spwn member of fwdBiol)
 * \param fishery_no The position of the fishery in the fisheries list (starting at 1)
 * \param biol_no The position of the biol in the biols list (starting at 1)
 * \param indices_min The minimum indices: year, unit etc (length 5)
 * \param indices_max The maximum indices: year, unit etc (length 5)
 */
FLQuant operatingModel::f_prop_spwn(const int fishery_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
    //Rprintf("In f_prop_spwn\n");
    // Check indices_min and indices_max are of length 5
    if (indices_min.size() != 5 | indices_max.size() != 5){
        Rcpp::stop("In operatingModel::f_prop_spwn subsetter. Indices not of length 5\n");
    }
    // Make an object to dump the result into
    FLQuant propf_out(1, indices_max[0]-indices_min[0]+1, indices_max[1]-indices_min[1]+1, indices_max[2]-indices_min[2]+1, indices_max[3]-indices_min[3]+1, indices_max[4]-indices_min[4]+1);
    // Need to calculate element by element as timing can change over years etc.
    double propf = 0.0;
    // To speed up and not get whole FLQuant each time - indicates problem with way we are accessing FLQuant members
    FLQuant hperiod = fisheries(fishery_no).hperiod();
    FLQuant spwn_all = biols(biol_no).spwn();
    for (unsigned int year_count=indices_min[0]; year_count <= indices_max[0]; ++year_count){
        for (unsigned int unit_count=indices_min[1]; unit_count <= indices_max[1]; ++unit_count){
            for (unsigned int season_count=indices_min[2]; season_count <= indices_max[2]; ++season_count){
                for (unsigned int area_count=indices_min[3]; area_count <= indices_max[3]; ++area_count){
                    for (unsigned int iter_count=indices_min[4]; iter_count <= indices_max[4]; ++iter_count){
                        // Fix this depending on representation of fperiod
                        //Rprintf("year_count: %i, unit_count: %i, season_count: %i, area_count: %i, iter_count: %i\n", year_count, unit_count, season_count, area_count, iter_count);
                        // hperiod has length 1 in the unit dimension
                        double fstart = hperiod(1,year_count, 1, season_count, area_count, iter_count);
                        double fend = hperiod(2,year_count, 1, season_count, area_count, iter_count);
                        double spwn = spwn_all(1,year_count, unit_count, season_count, area_count, iter_count);
                        if (fend < spwn){
                            propf = 1.0;
                        }
                        else if (fstart > spwn){
                            propf = 0.0;
                        }
                        else {
                            propf = (spwn - fstart) / (fend - fstart);
                        }
            // Dump it in the right place - ugliness abounds
            propf_out(1, year_count - indices_min[0] +1, unit_count - indices_min[1] +1, season_count - indices_min[2] +1, area_count - indices_min[3] +1, iter_count - indices_min[4] +1) = propf; 
    }}}}}
    //Rprintf("Leaving f_prop_spwn\n");
    return propf_out;
}


/*! \brief Calculates the recruitment for each iteration for a particular unit of a particular fwdBiol at a particular timestep
 *
 * Each unit of a biol can recruit at a different time so we need to specify the unit we want the recruitment for.
 * However, all units have the same timelag between spawning and recruitment.
 * The length of the returned vector is the number of iterations in the biol.
 * \param biol_no The position of the biol in the biols list (starting at 1).
 * \param unit The unit of the biol we are dealing with.
 * \param timestep The timestep of the recruitment (not the SSB that yields the recruitment).
 */
std::vector<adouble> operatingModel::calc_rec(const unsigned int biol_no, const unsigned int unit, const unsigned int rec_timestep) const{
    unsigned int srp_timestep = rec_timestep - biols(biol_no).srp_timelag();
    if (srp_timestep < 1){
        Rcpp::stop("In operatingModel::calc_rec. srp_timestep outside of range");
    }
    unsigned int srp_year = 0;
    unsigned int srp_season = 0;
    std::vector<unsigned int> biol_dim = biols(biol_no).n().get_dim(); 
    timestep_to_year_season(srp_timestep, biol_dim[3], srp_year, srp_season);
    // Area not dealt with yet - set to 1
    unsigned int area = 1;
    unsigned int niter = get_niter();
    // Get SRP for the single unit, year and season, i.e. just iters
    std::vector<unsigned int> srp_indices_min{srp_year, unit, srp_season, area, 1};
    std::vector<unsigned int> srp_indices_max{srp_year, unit, srp_season, area, niter};
    FLQuantAD srpq = srp(biol_no, srp_indices_min, srp_indices_max);
    // Initial indices of the SR params are those of the recruitment timestep for the Biol
    // SR params are in step with the Recruitment not the SRP
    unsigned int initial_params_year = 0;
    unsigned int initial_params_season = 0;
    timestep_to_year_season(rec_timestep, biol_dim[3], initial_params_year, initial_params_season);
    std::vector<unsigned int> initial_params_indices{initial_params_year, unit, initial_params_season, area, 1};
    // Same for the residuals
    std::vector<unsigned int> initial_residuals_indices_min{initial_params_year, unit, initial_params_season, area, 1};
    FLQuantAD rec = biols(biol_no).srr.predict_recruitment(srpq, initial_params_indices);
    return rec.get_data();
}

/*! \name get_f
 * Calculate the instantaneous fishing mortality 
 * This method is the workhorse fishing mortality method that is called by other fishing mortality methods that do make checks.
 * F = effort * selectivity * catchability.
 * F = effort * selectivity * alpha * biomass ^ -beta
 * Selectivity and biomass are disaggragated across all the FLQ dimensions (biomass has length 1 in the quant dimension), i.e. they will have seasons and units.
 * Effort always has length 1 in the unit and quant dimension and is disaggregated across other dimensions.
 * The catchability parameters can be disaggreated across dimensions 2 to 6. If not, they are recycled.
 */
//@{
/*! \brief Calculate the instantaneous fishing mortality of a single biol from a single fishery / catch over a subset of dimensions.
 * It is assumed that the fishery / catch actually fishes the biol (no check is made).
 * \param fishery_no the position of the fishery within the fisheries (starting at 1).
 * \param catch_no the position of the catch within the fishery (starting at 1).
 * \param biol_no the position of the biol within the biols (starting at 1).
 * \param indices_min The minimum indices quant, year, unit etc (length 6)
 * \param indices_max The maximum indices quant, year, unit etc (length 6)
*/
FLQuantAD operatingModel::get_f(const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if (indices_min.size() != 6 | indices_max.size() != 6){
        Rcpp::stop("In operatingModel get_f subsetter. Indices not of length 6\n");
    }
    // Lop off the first value from the indices to get indices without quant - needed for effort and catch_q
    std::vector<unsigned int> indices_min5(indices_min.begin()+1, indices_min.end());
    std::vector<unsigned int> indices_max5(indices_max.begin()+1, indices_max.end());
    FLQuantAD sel = fisheries(fishery_no, catch_no).catch_sel()(indices_min, indices_max);
    FLQuantAD biomass = biols(biol_no).biomass(indices_min5, indices_max5);
    // Need special subsetter for effort as always length 1 in the unit dimension
    std::vector<unsigned int> effort_indices_min5{indices_min5[0], 1, indices_min5[2], indices_min5[3], indices_min5[4]};  
    std::vector<unsigned int> effort_indices_max5{indices_max5[0], 1, indices_max5[2], indices_max5[3], indices_max5[4]};  
    FLQuantAD effort = fisheries(fishery_no).effort(effort_indices_min5, effort_indices_max5); // Will always have 1 in the unit dimension
    // Get q params as a whole FLQuant - just first 2 'ages' (params)
    std::vector<unsigned int> qparams_indices_min = indices_min5;
    qparams_indices_min.insert(qparams_indices_min.begin(), 1); 
    std::vector<unsigned int> qparams_indices_max = indices_max5;
    qparams_indices_max.insert(qparams_indices_max.begin(), 2); 
    FLQuant qparams = fisheries(fishery_no, catch_no).catch_q_params(qparams_indices_min, qparams_indices_max);
    // Subset qparams to get FLQ of the indiv params - i.e. seperating into alpha and beta - really faffy
    qparams_indices_min = {1,1,1,1,1,1};
    qparams_indices_max = qparams.get_dim();
    qparams_indices_max[0] = 1;
    FLQuant qparams1 = qparams(qparams_indices_min, qparams_indices_max);
    qparams_indices_min[0] = 2;
    qparams_indices_max[0] = 2;
    FLQuant qparams2 = qparams(qparams_indices_min, qparams_indices_max);
    std::transform(biomass.begin(), biomass.end(), qparams2.begin(), biomass.begin(),
        [](adouble x, double y) { return pow(x, -1.0 * y); } );
    biomass = sweep_mult(biomass * qparams1, effort); // Use sweep_mult a effort always has length 1 in unit while biomass and qparams may have more
    FLQuantAD fout = sweep_mult(biomass, sel);
    return fout;
}
/*! \brief Calculate the instantaneous fishing mortality of a single biol from a single fishery / catch over all dimensions.
 * It is assumed that the fishery / catch actually fishes the biol (no check is made).
 * \param fishery_no the position of the fishery within the fisheries (starting at 1).
 * \param catch_no the position of the catch within the fishery (starting at 1).
 * \param biol_no the position of the biol within the biols (starting at 1).
 */
FLQuantAD operatingModel::get_f(const int fishery_no, const int catch_no, const int biol_no) const {
    // Just call the subset method with full indices
    std::vector<unsigned int> indices_max = biols(biol_no).n().get_dim();
    std::vector<unsigned int> indices_min(6,1);
    FLQuantAD f = get_f(fishery_no, catch_no, biol_no, indices_min, indices_max);
    return f;
}
/*! \brief Total instantaneous fishing mortality on a biol over a subset of dimensions
 * \param biol_no the position of the biol within the biols (starting at 1).
 * \param indices_min minimum indices for subsetting (quant - iter, vector of length 6)
 * \param indices_max maximum indices for subsetting (quant - iter, vector of length 6)
 */
FLQuantAD operatingModel::get_f(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
    unsigned int fishery_no;
    unsigned int catch_no;
    if (biol_no > biols.get_nbiols()){
        Rcpp::stop("In OM get_f biol. biol_no is greater than number of biols\n");
    }
    // We need to know the Fishery / Catches that catch the biol
    const Rcpp::IntegerMatrix FC =  ctrl.get_FC(biol_no);
    // What happens if no-one is fishing that biol? FC.nrow() == 0 so loop never triggers
    FLQuantAD total_f(indices_max[0] - indices_min[0] + 1, indices_max[1] - indices_min[1] + 1, indices_max[2] - indices_min[2] + 1, indices_max[3] - indices_min[3] + 1, indices_max[4] - indices_min[4] + 1, indices_max[5] - indices_min[5] + 1); 
    total_f.fill(0.0);
    for (unsigned int f_counter=0; f_counter < FC.nrow(); ++f_counter){
        total_f = total_f + get_f(FC(f_counter,0), FC(f_counter,1), biol_no, indices_min, indices_max);
    }
    return total_f;
}
/*! \brief Total instantaneous fishing mortality on a biol over all dimensions
 *
 * \param biol_no the position of the biol within the biols (starting at 1).
 */
FLQuantAD operatingModel::get_f(const int biol_no) const {
    std::vector<unsigned int> indices_max = biols(biol_no).n().get_dim();
    std::vector<unsigned int> indices_min(6,1);
    FLQuantAD f = get_f(biol_no, indices_min, indices_max);
    return f;
}
//@}


/*! \brief Get the total mortality on a Biol, collapsed over the unit dimension.
 * This is a special case for Biols with multiple units where you want the total mortality combined over units (i.e. the unit dimension is collapsed).
 * It is used to help get the fishing mortality combined over units.
 * It does not use the current effort to calculate and F and assumes that the operatingModel is updated for the indices requested.
 * This is calculated as follows :
 * (notation: Nut is the abundance in unit u at time t)
 * N12 = N11 exp(-Z1) # survivors for unit 1
 * N22 = N21 exp(-Z2) # survivors for unit 2
 * Ntotal1 = N11 + N21 # combine the units
 * Ntotal2 = N12 + N22
 * Ntotal2 = Ntotal1 exp(-Ztotal) # survivors of combined units
 * Rearrange to give:
 * Zt = -log ((N11 exp(-Z1) + N21 exp(-Z2) + ...) / (N11 + N21 + ...))
 * \param biol_no the position of the biol within the biols (starting at 1).
 * \param indices_min minimum indices for subsetting (quant - iter, vector of length 6)
 * \param indices_max maximum indices for subsetting (quant - iter, vector of length 6)
 */
FLQuantAD operatingModel::get_unit_z(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if (indices_min.size() != 6 | indices_max.size() != 6){
        Rcpp::stop("In operatingModel get_unit_z subsetter. Indices not of length 6\n");
    }
    FLQuantAD n = biols(biol_no).n(indices_min, indices_max);
    FLQuantAD survivors = n * exp(-1.0 * (get_f(biol_no, indices_min, indices_max) + biols(biol_no).m(indices_min, indices_max)));
    FLQuantAD unit_z = -1.0 * log(unit_sum(survivors) / unit_sum(n));
    return unit_z; 
}


/*! \name get_unit_f
 * Get the fishing mortality collapsed over the unit dimension.
 * This is a special case for Biols and Catches with multiple units where you want the fishing mortality combined over units (i.e. the unit dimension is collapsed).
 * This is calculated by extracting the current total catch from the FLCatches. The catch is not calculated 'live' using the current effort.
 * It therefore assumes that the catch and biol abundance have been correctly updated to reflect the fishing mortality.
 * It does not use the current effort to calculate the catch and F and assumes that the operatingModel is updated for the indices requested.
 * Given the standard Baranov catch equation:
 * C = (F / Z) * (1 - exp(-Z)) * N
 * Rearranging to give:
 * F = C Z / ((1 - exp(-Z))*N)
 * Z is the total mortality on the Biol from all FLCatches and fishing mortality
 * C is the catch from either an individual Catch or the Catch on the Biol
 * C, Z and N have been collapsed over the unit dimension.
 */
//@{
/*! \brief Get the fishing mortality on a Biol, collapsed over the unit dimension.
 * \param biol_no the position of the biol within the biols (starting at 1).
 * \param indices_min minimum indices for subsetting (quant - iter, vector of length 6)
 * \param indices_max maximum indices for subsetting (quant - iter, vector of length 6)
 */
FLQuantAD operatingModel::get_unit_f(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if (indices_min.size() != 6 | indices_max.size() != 6){
        Rcpp::stop("In operatingModel get_unit_f subsetter. Indices not of length 6\n");
    }
    FLQuantAD unit_catch =  unit_sum(catch_n(biol_no, indices_min, indices_max));
    FLQuantAD unit_z = get_unit_z(biol_no, indices_min, indices_max);
    FLQuantAD n = unit_sum(biols(biol_no).n(indices_min, indices_max));
    FLQuantAD unit_f = (unit_catch * unit_z) / ((1.0-exp(-1.0 * unit_z)) * n);
    return unit_f;
}
/*! \brief Get the fishing mortality from an individual Catch on a Biol, collapsed over the unit dimension.
 * \param fishery_no the position of the Fishery within the Fisheries (starting at 1).
 * \param catch_no the position of the Catch within the Catches (starting at 1).
 * \param biol_no the position of the Biol within the Biols (starting at 1).
 * \param indices_min minimum indices for subsetting (quant - iter, vector of length 6)
 * \param indices_max maximum indices for subsetting (quant - iter, vector of length 6)
 */
FLQuantAD operatingModel::get_unit_f(const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if (indices_min.size() != 6 | indices_max.size() != 6){
        Rcpp::stop("In operatingModel get_unit_f subsetter. Indices not of length 6\n");
    }
    FLQuantAD unit_catch = unit_sum(fisheries(fishery_no, catch_no).catch_n(indices_min, indices_max));
    FLQuantAD unit_z = get_unit_z(biol_no, indices_min, indices_max);
    FLQuantAD n = unit_sum(biols(biol_no).n(indices_min, indices_max));
    FLQuantAD unit_f = (unit_catch * unit_z) / ((1.0-exp(-1.0 * unit_z)) * n);
    return unit_f;
}
//@}

/*! \brief Project the Biols in the operatingModel by a single timestep
 *   Projects the Biols in the operatingModel by a single timestep.
 *   All abundances in the Biols are updated in the timestep based on the fishing effort and population abundance in the previous timestep.
 *   Calculation is based on the the Baranov equation: N2 = N1 * (exp -Z)
 *   This assumes that the instantaneous rate of fishing and natural mortalities are constant over the timestep and occur simultaneously.
 *   If a Biol is caught by multiple Catches, the fishing mortalities happen at the same time (and at the same time as the natural mortality) in the timestep.
 *   \param timestep The time step for the projection.
 */
void operatingModel::project_biols(const int timestep){
    //Rprintf("In operatingModel::project_biols\n");
    if (timestep < 2){
        Rcpp::stop("In operatingModel::project_biols. Uses effort in previous timestep so timestep must be at least 2.");
    }
    unsigned int year = 0;
    unsigned int season = 0;
    unsigned int prev_year = 0;
    unsigned int prev_season = 0;
    std::vector<unsigned int> biol_dim = biols(1).n().get_dim(); // All biols have same year and season range, so pick first one
    timestep_to_year_season(timestep, biol_dim[3], year, season);
    timestep_to_year_season(timestep-1, biol_dim[3], prev_year, prev_season);
    // timestep checks
    if ((year > biol_dim[1]) | (season > biol_dim[3])){
        Rcpp::stop("In operatingModel::project_biols. Timestep outside of range");
    }
    unsigned int niter = get_niter();
    // Not yet set up for areas so just set to 1 for now
    unsigned int area = 1;
    // Loop over and update each biol
    for (unsigned int biol_counter=1; biol_counter <= biols.get_nbiols(); ++biol_counter){
        // Indices for subsetting the previous timestep
        std::vector<unsigned int> biol_dim = biols(biol_counter).n().get_dim();
        std::vector<unsigned int> prev_indices_min{1, prev_year, 1, prev_season, area, 1};
        std::vector<unsigned int> prev_indices_max{biol_dim[0], prev_year, biol_dim[2], prev_season, area, niter};
        // Get survivors from last timestep 
        FLQuantAD z_temp = get_f(biol_counter, prev_indices_min, prev_indices_max) + biols(biol_counter).m(prev_indices_min, prev_indices_max);
        //FLQuantAD z_temp(prev_indices_max[0] - prev_indices_min[0]+1, prev_indices_max[1] - prev_indices_min[1]+1, prev_indices_max[2] - prev_indices_min[2]+1, prev_indices_max[3] - prev_indices_min[3]+1, prev_indices_max[4] - prev_indices_min[4]+1, prev_indices_max[5] - prev_indices_min[5]+1, 0.0);
        FLQuantAD survivors = biols(biol_counter).n(prev_indices_min, prev_indices_max) * exp(-1.0 * z_temp);
        // Update biol in timestep by putting survivors into correct age slots
        // Process by unit (which are effectively distinct)
        for (unsigned int ucount = 1; ucount <= biol_dim[2]; ++ucount){
            // Need to know the age in which to the survivors 
            // Each unit can recruit in a different season and can only recruit in one season
            // If recruitment timestep, survivors get placed in next age i.e. it's their birthday (hooray) and they move up an age class.
            // Else, same age in next timestep
            // Determine if this is a recruitment timestep for this unit
            bool recruiting_now = biols(biol_counter).does_recruitment_happen(ucount, timestep);
            //bool recruiting_now = true;
            unsigned int age_shift = 0;
            std::vector<adouble> rec(niter, 0.0);
            if (recruiting_now){
                age_shift = 1;
                // Get the recruitment, all iters
                rec = calc_rec(biol_counter, ucount, timestep);
            }
            // Need to loop over things as objects are awkward size
            for (unsigned int icount = 1; icount <= niter; ++icount){
                // Place all but the final survivor age (careful with age shift) - final survivor age dealt with a bit later
                for (unsigned int qcount = 1; qcount <= (biol_dim[0] - 1); ++qcount){
                    biols(biol_counter).n(qcount + age_shift, year, ucount, season, area, icount) = survivors(qcount, 1, ucount, 1, 1, icount);
                }
                // If it is the recruitment season add final age of survivors into the plus group
                if (recruiting_now){
                    biols(biol_counter).n(biol_dim[0], year, ucount, season, area, icount) = biols(biol_counter).n(biol_dim[0], year, ucount, season, area, icount) + survivors(biol_dim[0], 1, ucount, 1, 1, icount);
                    // Put recruitment into first age
                    biols(biol_counter).n(1, year, ucount, season, area, icount) = rec[icount-1];
                }
                // Else the final survivor age gets put in final age
                else {
                    biols(biol_counter).n(biol_dim[0], year, ucount, season, area, icount) = survivors(biol_dim[0], 1, ucount, 1, 1, icount);
                }
            }
        }
    }
    return;
}

//! Project the Fisheries in the operatingModel by a single timestep
/*!
    Projects the Fisheries in the operatingModel by a single timestep.
    All catches, landings and discards in the Fisheries are updated for that timestep based on effort in that timestep
    The Baranov catch equation is used to calculate catches.
    C = (pF / Z) * (1 - exp(-Z)) * N
    This assumes that the instantaneous rate of fishing and natural mortalities are constant over time and age and occur simultaneously.
    \param timestep The time step for the projection.
 */
void operatingModel::project_fisheries(const int timestep){
    // C = (pF / Z) * (1 - exp(-Z)) * N
    //Rprintf("In operatingModel::project_fisheries\n");
    // What timesteps / years / seasons are we dealing with?
    unsigned int year = 0;
    unsigned int season = 0;
    std::vector<unsigned int> catch_dim = fisheries(1,1).landings_n().get_dim(); // Just used for years and seasons - same across Catches in OM
    timestep_to_year_season(timestep, catch_dim[3], year, season);
    // timestep checks
    if ((year > catch_dim[1]) | (season > catch_dim[3])){
        Rcpp::stop("In operatingModel::project_fisheries. timestep outside of range");
    }
    // CAREFUL WITH NUMBER OF ITERS
    // Number of iters for all catch_n and biol n must be the same as all derive from effort which has the number of iters
    unsigned int niter = get_niter();
    // Not yet set up for areas
    unsigned int area = 1;
    // Get the Total Z for every biol - order is order of biols in biols list (maybe different to FCB order)
    std::vector<FLQuantAD> total_z(biols.get_nbiols());
    // Fill it up with natural mortality to start with
    for (int biol_count=1; biol_count <= biols.get_nbiols(); ++biol_count){
        std::vector<unsigned int> indices_min{1, year, 1, season, area, 1};
        std::vector<unsigned int> biol_dim = biols(biol_count).n().get_dim();
        std::vector<unsigned int> indices_max{biol_dim[0], year, biol_dim[2], season, area, niter};
        total_z[biol_count - 1] = biols(biol_count).m(indices_min, indices_max);
    }
    // Get Partial F for every row in FCB table
    // Store in the same order as FCB table - order is important will be used to access them later
    //Rprintf("Getting total Z and partial F\n");
    Rcpp::IntegerMatrix FCB = ctrl.get_FCB();
    std::vector<FLQuantAD> partial_f(FCB.nrow());
    for (unsigned int FCB_counter=0; FCB_counter < FCB.nrow(); ++FCB_counter){
        //Rprintf("FCB counter %i Biol %i\n", FCB_counter, FCB(FCB_counter, 2)); 
        // Indices for subsetting the timestep
        std::vector<unsigned int> indices_min{1, year, 1, season, area, 1};
        std::vector<unsigned int> biol_dim = biols(FCB(FCB_counter, 2)).n().get_dim();
        std::vector<unsigned int> indices_max{biol_dim[0], year, biol_dim[2], season, area, niter};
        partial_f[FCB_counter] = get_f(FCB(FCB_counter, 0), FCB(FCB_counter, 1), FCB(FCB_counter, 2), indices_min, indices_max);
        // Add the partial f to the total z list
        total_z[FCB(FCB_counter, 2)-1] = total_z[FCB(FCB_counter, 2)-1] + partial_f[FCB_counter];
    }
    //Rprintf("Got total Z and partial F for all Biols\n");
    // Now we have the partial F of each FC on B, and the total Z of each biol, we can get the catch
    FLQuantAD landings;
    FLQuantAD discards;
    FLQuantAD discards_ratio_temp;
    for (int fishery_count=1; fishery_count <= fisheries.get_nfisheries(); ++fishery_count){
        for (int catch_count=1; catch_count <= fisheries(fishery_count).get_ncatches(); ++catch_count){
            //Rprintf("fishery_count: %i catch_count: %i\n", fishery_count, catch_count);
            // Indices for subsetting the timestep
            std::vector<unsigned int> catch_dim = fisheries(fishery_count, catch_count).landings_n().get_dim();
            std::vector<unsigned int> indices_min{1, year, 1, season, area, 1};
            std::vector<unsigned int> indices_max{catch_dim[0], year, catch_dim[2], season, area, niter};
            // Make temporary catch of right size fillled with 0s
            std::vector<unsigned int> catch_temp_dims(6); // Could just use catch_dim from above but that may have multiple areas and units in the future
            std::transform(indices_max.begin(), indices_max.end(), indices_min.begin(), catch_temp_dims.begin(), [] (unsigned int x, unsigned int y) {return x-y+1;});
            FLQuantAD catch_temp(catch_temp_dims, 0.0);
            // Loop over each biol that the FC fishes - a catch can fish more than one biol
            std::vector<int> biols_fished = ctrl.get_B(fishery_count, catch_count);
            for (int biol_count=0; biol_count < biols_fished.size(); ++biol_count){
                // Index of total Z
                unsigned int biol_no = biols_fished[biol_count];
                // Index of partial f
                unsigned int partial_f_element = ctrl.get_FCB_row_no(fishery_count, catch_count, biol_no); 
                // C = (pF / Z) * (1 - exp(-Z)) * N
                catch_temp = catch_temp + ((partial_f[partial_f_element] / total_z[biol_no-1]) * (1.0 - exp(-1.0 * total_z[biol_no-1])) * biols(biol_no).n(indices_min, indices_max));
            }
            discards_ratio_temp = fisheries(fishery_count, catch_count).discards_ratio(indices_min, indices_max); 
            landings = catch_temp * (1.0 - discards_ratio_temp);
            discards = catch_temp * discards_ratio_temp;
            // Stick the new landings and discards into the catch
            fisheries(fishery_count, catch_count).landings_n().insert(landings, indices_min, indices_max);
            fisheries(fishery_count, catch_count).discards_n().insert(discards, indices_min, indices_max);
    }}
    //Rprintf("Leaving project_fisheries\n");
    return;
}


/*! \brief Runs the projection according to the control object.
 *
 * Finds the effort multipliers for each timestep of the projection to hit the desired targets.
 *
 * \param indep_min The minimum value of effort multipliers
 * \param indep_max The maximum value of effort multipliers
 */
Rcpp::IntegerMatrix operatingModel::run(const double effort_mult_initial, const double indep_min, const double indep_max, const unsigned int nr_iters){
    Rprintf("Running\n");
    // Housekeeping
    auto niter = get_niter(); // number of iters taken from effort of first fishery
    Rprintf("Number of iterations to solve - niter: %i\n", niter);
    // Effort multiplier is the independent value. There is one independent values for each effort, i.e. for each fishery
    auto neffort = fisheries.get_nfisheries();
    Rprintf("Number of fisheries to solve effort for: %i\n", neffort);

    // Update N at start of projection (minimum timestep of the first target - not effort timestep)
    // For example, if you have a catch target in timestep y, you need to make sure that there is biol abundance in timestep y
    // Is this always necessary? What if first target is biol in timestep y? Check this
    // The target timesteps are contiguous.
    // Update the biology in the first target timestep.
    // This ensures that we have abundance numbers in the first timestep of the projection.
    // However, this will overwrite existing abundances - do we want this?
    // Assumes that first target has a target number of 1
    int first_target_number = 1; // Or could write routine to calculate
    Rcpp::IntegerVector target_timestep = ctrl.get_target_int_col(first_target_number, "timestep");
    // Get the min, as same target can have sim targets in different timesteps e.g. with SSB targets - think this through
    auto min_target_timestep = *std::min_element(std::begin(target_timestep), std::end(target_timestep));
    Rprintf("Min target timestep: %i\n", min_target_timestep);
    Rprintf("Projecting biols for the first timestep to update abundances\n");
    project_biols(min_target_timestep);
    // Get maximum timestep of OM. If room, we update Biol in final timestep at the end
    std::vector<unsigned int> biol1_dim = biols(1).n().get_dim();
    unsigned int max_timestep = biol1_dim[1] * biol1_dim[3];
    // Each target is solved independently.
    // But a target can be made up of multiple simultaneous targets
    auto ntarget = ctrl.get_ntarget();
    Rprintf("Targets to solve: %i \n", ntarget);
    // Ntarget x iter
    Rcpp::IntegerMatrix solver_codes(ntarget,niter);
    // Loop over targets and solve all simultaneous targets in that target set
    // e.g. With 2 fisheries with 2 efforts, we can set 2 catch targets to be solved at the same time
    // Indexing of targets starts at 1
    for (auto target_count = 1; target_count <= ntarget; ++target_count){
        Rprintf("\nProcessing target: %i\n", target_count);
        auto nsim_targets = ctrl.get_nsim_target(target_count);
        Rprintf("Number of simultaneous targets: %i\n", nsim_targets);
        // Timestep in which we find effort is the same for all simultaneous targets in a target set
        // Cannot just read from the control object because the abundance (biomass, SSB etc.) is determined by effort in previous timestep - maybe
        // Get time step of first sim target and use this for all sim targets.
        int first_sim_target_number = 1; // Or could write routine to calculate
        unsigned int target_effort_timestep = ctrl.get_target_effort_timestep(target_count, first_sim_target_number);
        unsigned int target_effort_year = 0;
        unsigned int target_effort_season = 0;
        timestep_to_year_season(target_effort_timestep, biols(1).n().get_nseason(), target_effort_year, target_effort_season);
        Rprintf("target_effort_timestep: %i\n", target_effort_timestep);
        // Get the target value based on control object and current value in the OM (if Max / Min)
        // This is not part of the operation sequence so is evaluated before we turn on the tape
        std::vector<adouble> target_value = get_target_value(target_count); // values of all sim targets for the target
        // Turn tape on
        // Set up effort multipliers - do all efforts and iters at same time (keep timesteps, areas, units seperate)
        std::vector<adouble> effort_mult_ad(neffort * niter, effort_mult_initial);
        std::fill(effort_mult_ad.begin(), effort_mult_ad.end(), effort_mult_initial);
        CppAD::Independent(effort_mult_ad);
        //Rprintf("Turned on tape\n");
        //Rprintf("target_effort_year: %i\n", target_effort_year);
        //Rprintf("target_effort_season: %i\n", target_effort_season);
        // Update fisheries.effort() with effort multiplier in the effort timestep (area and unit effectively ignored)
        //Rprintf("Updating effort with multipler\n");
        //Rprintf("Before updating effort: %f\n", Value(fisheries(1).effort()(1, target_effort_year, 1, target_effort_season, 1, 1)));
        for (int fisheries_count = 1; fisheries_count <= fisheries.get_nfisheries(); ++fisheries_count){
            for (int iter_count = 1; iter_count <= niter; ++ iter_count){
                fisheries(fisheries_count).effort()(1, target_effort_year, 1, target_effort_season, 1, iter_count) = 
                    fisheries(fisheries_count).effort()(1, target_effort_year, 1, target_effort_season, 1, iter_count) * 
                    effort_mult_ad[(fisheries_count - 1) * niter + iter_count - 1];
            }
        }
        //Rprintf("After updating effort: %f\n", Value(fisheries(1).effort()(1, target_effort_year, 1, target_effort_season, 1, 1)));
        //Rprintf("Projecting\n");
        // Project fisheries in the target effort timestep
        // (landings and discards are functions of effort in the effort timestep)
        Rprintf("Projecting fisheries\n");
        project_fisheries(target_effort_timestep); 
        // Project biology in the target effort timestep plus 1
        // (biology abundances are functions of effort in the previous timestep)
        // Only update if there is room
        if ((target_effort_timestep+1) <= max_timestep){
            Rprintf("Projecting biols\n");
            project_biols(target_effort_timestep+1); 
        }
        //Rprintf("Back from projecting\n");
        // Calc error
        //Rprintf("Getting desired target values from control object\n");
        //std::vector<adouble> target_value = get_target_value(target_count); // values of all sim targets for the target
        //Rprintf("Getting current state of operating model\n");
        // Get current state of operating model
        std::vector<adouble> target_value_hat = get_target_value_hat(target_count); 
        //Rprintf("Back from target_value_hat\n");
        //Rprintf("Length of target_value_hat: %i\n", target_value.size());
        //Rprintf("Length of target_value: %i\n", target_value.size());
        // Check they are the same length? 
        std::vector<adouble> error(target_value_hat.size());
        //Rprintf("Calculating error\n");
        std::transform(target_value.begin(), target_value.end(), target_value_hat.begin(), error.begin(),
                [](adouble x, adouble y){return x - y;});
                //[](adouble x, adouble y){return (x - y) * (x - y);}); // squared error - not as effective
        //Rprintf("target 1. target_value: %f target_value_hat: %f error: %f\n", Value(target_value[0]), Value(target_value_hat[0]), Value(error[0]));
        //Rprintf("target 2. target_value: %f target_value_hat: %f error: %f\n", Value(target_value[1]), Value(target_value_hat[1]), Value(error[1]));
        // Stop recording
        CppAD::ADFun<double> fun(effort_mult_ad, error);
        //Rprintf("Turned off tape\n\n");
        // Solve the target
        // double version of effort mult used in solver
        std::vector<double> effort_mult(neffort * niter, effort_mult_initial);
        std::fill(effort_mult.begin(), effort_mult.end(), effort_mult_initial);
        std::vector<int> nr_out = newton_raphson(effort_mult, fun, niter, nsim_targets, indep_min, indep_max, nr_iters);
        // Check nr_out - if not all 1 then something has gone wrong - flag up warning
        // Each iter has a success code for all sim targets - put them into a matrix
        // Ntarget x iter
        for (auto iter_count = 0; iter_count < niter; ++iter_count){
            solver_codes(target_count - 1, iter_count) = nr_out[iter_count];
        }
        //Rprintf("effort_mult: %f\n", effort_mult[0]);
        //Rprintf("Updating effort with solved effort mult\n");
        for (int fisheries_count = 1; fisheries_count <= fisheries.get_nfisheries(); ++fisheries_count){
            for (int iter_count = 1; iter_count <= niter; ++ iter_count){
                fisheries(fisheries_count).effort()(1, target_effort_year, 1, target_effort_season, 1, iter_count) = 
                   fisheries(fisheries_count).effort()(1, target_effort_year, 1, target_effort_season, 1, iter_count) * 
                    effort_mult[(fisheries_count - 1) * niter + iter_count - 1] / effort_mult_initial;
            }
        }
        //Rprintf("Final effort: %f\n", Value(fisheries(1).effort()(1, target_effort_year, 1, target_effort_season, 1, 1)));
        //Rprintf("Projecting again\n");
        project_fisheries(target_effort_timestep); 
        // If space, update biols too
        if ((target_effort_timestep+1) <= max_timestep){
            project_biols(target_effort_timestep+1); 
        }
    }
    Rprintf("Leaving run\n");
    return solver_codes;
}

/*! \brief Evaluate the current state of the operating model. 
 *
 * It is necessary to provide the fishery, catch and biol numbers because the state being evaluated depends on them.
 * For example, catch could be the total catch of a fwdBiol (from multiple FLCatch objects), or the catch of a single FLCatch.
 * \param target_type What state do you want to evaluate (catch, F etc)
 * \param fishery_no The fishery number. 
 * \param catch_no The catch number. 
 * \param biol_no The biol number. 
 * \param indices_min The minimum range of the returned FLQuant
 * \param indices_max The maximum range of the returned FLQuant
 */
FLQuantAD operatingModel::eval_om(const fwdControlTargetType target_type, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
    // Check length of indices_min and max - should be of length 5 (no age structured quantities returned)
    if (indices_min.size() != 5 | indices_max.size() != 5){
        Rcpp::stop("In operatingModel srp subsetter. Indices not of length 5\n");
    }
    // If we have a catch_no, we must also have a fishery_no
    // But you are allowed a fishery with no catch - effort target
    if(!(Rcpp::IntegerVector::is_na(catch_no)) & Rcpp::IntegerVector::is_na(fishery_no)){
        Rcpp::stop("In operatingModel::eval_om. If you specify a catch_no, you must also specify a fishery_no.\n");
    }
    FLQuantAD out;
    switch(target_type){
        case target_effort: {
            Rprintf("target_effort\n");
            if (Rcpp::IntegerVector::is_na(fishery_no)){
                Rcpp::stop("In operatingModel::eval_om. Asking for effort target but fishery_no has been specified.\n");
            }
            else {
                out = fisheries(fishery_no).effort(indices_min, indices_max);
            }
        break;
        }

//        case target_fbar: {
//            Rprintf("target_fbar\n");
//            // Indices only 5D when passed in - needs age range
//            if (Rcpp::IntegerVector::is_na(biol_no)) {
//                Rcpp::stop("In operatingModel::eval_target. Asking for fbar when biol_na is NA. Not yet implemented.\n");
//            }
//            std::vector<unsigned int> age_range_indices = get_target_age_range_indices(target_no, sim_target_no, biol_no); // indices start at 0
//            std::vector<unsigned int> age_indices_min = indices_min;
//            std::vector<unsigned int> age_indices_max = indices_max;
//            age_indices_min.insert(age_indices_min.begin(), age_range_indices[0] + 1); // +1 because accessor starts at 1 but age indices 0 - sorry
//            age_indices_max.insert(age_indices_max.begin(), age_range_indices[1] + 1);
//            // Is it total fbar on a biol, or fbar of an FLCatch
//            if (!Rcpp::IntegerVector::is_na(biol_no) & !Rcpp::IntegerVector::is_na(catch_no)){
//                Rprintf("fbar is from FLCatch %i in FLFishery %i on biol %i\n", catch_no, fishery_no, biol_no);
//                out = fbar(fishery_no, catch_no, biol_no, age_indices_min, age_indices_max);
//            }
//            else {
//                Rprintf("catch_no is NA. fbar is total fbar from biol %i\n", biol_no);
//                Rprintf("ages_indices_min\n");
//        //        for (auto i=0; i<6; ++i){
//        //            Rprintf("age_indices_min %i\n", age_indices_min[i]);
//        //        }
//        //        Rprintf("ages_indices_max\n");
//        //        for (auto i=0; i<6; ++i){
//        //            Rprintf("age_indices_max %i\n", age_indices_max[i]);
//        //        }
//                out = fbar(biol_no, age_indices_min, age_indices_max);
//            }
//            break;
//        }
        case target_catch: {
            Rprintf("target_catch\n");
            // Is it total catch of a biol, or catch of an FLCatch
            if (Rcpp::IntegerVector::is_na(biol_no) & !Rcpp::IntegerVector::is_na(catch_no)){
                Rprintf("biol_no is NA, catch is from FLCatch %i in FLFishery %i\n", catch_no, fishery_no);
                out = unit_sum(fisheries(fishery_no, catch_no).catches(indices_min, indices_max));
            }
            else if (!Rcpp::IntegerVector::is_na(biol_no) & Rcpp::IntegerVector::is_na(catch_no)){
                Rprintf("catch is total catch from biol %i\n", biol_no);
                out =  unit_sum(catches(biol_no, indices_min, indices_max));
            }
            else if (!Rcpp::IntegerVector::is_na(fishery_no) & Rcpp::IntegerVector::is_na(catch_no)){
                Rcpp::stop("In operatingModel::eval_om. Asking for catch from a particular fishery but no catch_no has been specified.\n");
            }
            else {
                Rcpp::stop("In operatingModel::eval_om. Asking for catch from a particular catch and biol. It's a special case that is not yet implemented. Can you ask for total catch from just the biol instead and set catch no to NA?\n");
            }
            break;
        }
        case target_landings: {
            Rprintf("target_landings\n");
            if (Rcpp::IntegerVector::is_na(biol_no) & !Rcpp::IntegerVector::is_na(catch_no)){
                out = unit_sum(fisheries(fishery_no, catch_no).landings(indices_min, indices_max));
            }
            else if (!Rcpp::IntegerVector::is_na(biol_no) & Rcpp::IntegerVector::is_na(catch_no)){
                Rprintf("landings are total landings from biol %i\n", biol_no);
                out = unit_sum(landings(biol_no, indices_min, indices_max));
            }
            else if (!Rcpp::IntegerVector::is_na(fishery_no) & Rcpp::IntegerVector::is_na(catch_no)){
                Rcpp::stop("In operatingModel::eval_om. Asking for landings from a particular fishery but no catch_no has been specified.\n");
            }
            else {
                Rcpp::stop("In operatingModel::eval_om. Asking for landings from a particular catch and biol. It's a special case that is not yet implemented. Can you ask for total landings from just the biol instead?\n");
            }
            break;
        }
        case target_discards: {
            Rprintf("target_discards\n");
            if (Rcpp::IntegerVector::is_na(biol_no) & !Rcpp::IntegerVector::is_na(catch_no)){
                out = unit_sum(fisheries(fishery_no, catch_no).discards(indices_min, indices_max));
            }
            else if (!Rcpp::IntegerVector::is_na(biol_no) & Rcpp::IntegerVector::is_na(catch_no)){
                Rprintf("discards are total discards from biol %i\n", biol_no);
                out = unit_sum(discards(biol_no, indices_min, indices_max));
            }
            else if (!Rcpp::IntegerVector::is_na(fishery_no) & Rcpp::IntegerVector::is_na(catch_no)){
                Rcpp::stop("In operatingModel::eval_om. Asking for discards from a particular fishery but no catch_no has been specified.\n");
            }
            else {
                Rcpp::stop("In operatingModel::eval_om. Asking for discards from a particular catch and biol. It's a special case that is not yet implemented. Can you ask for total discards from just the biol instead?\n");
            }
            break;
        }
////        case target_srp: {
////            // Spawning reproductive potential
////            Rprintf("target_srp\n");
////            if (Rcpp::IntegerVector::is_na(biol_no)){
////                Rcpp::stop("In operatingModel eval_om. Trying to evaluate SRP target when biol no. is NA. Problem with control object?\n");
////            }
////            else {
////                out = ssb(biol_no, indices_min, indices_max);
////            }
////        break;
////    }
////        case target_biomass: {
////            Rprintf("target_biomass\n");
////            if (Rcpp::IntegerVector::is_na(biol_no)){
////                Rcpp::stop("In operatingModel eval_om. Trying to evaluate biomass target when biol no. is NA. Problem with control object?\n");
////            }
////            else {
////                out = biols(biol_no).biomass(indices_min, indices_max);
////            }
////            break;
////        }
        default:
            Rcpp::stop("target_type not found in switch statement - giving up\n");
            break;
    }
    return out;
}

/*! \name Get the current target values in the operating model
 */
//@{
/*! \brief Get the current target values in the operating model for all simultaneous targets.
 *
 * Returns a vector of the current simultaneous target values.
 * The vector is made up of vectors for each simultaneous target (each being of length niter)
 * If the target is relative, the method calculates the current relative state.
 * The values can be compared to the desired target values from get_target_value(). 
 * \param target_no References the target column in the control dataframe. Starts at 1.
 */
std::vector<adouble> operatingModel::get_target_value_hat(const int target_no) const{
    //Rprintf("In get_target_value_hat target_no\n");
    auto nsim_target = ctrl.get_nsim_target(target_no);
    std::vector<adouble> value;
    for (auto sim_target_count = 1; sim_target_count <= nsim_target; ++sim_target_count){
        auto sim_target_value = get_target_value_hat(target_no, sim_target_count);
        value.insert(value.end(), sim_target_value.begin(), sim_target_value.end());
    }
    return value;
} 

/*! \brief Get the current target values in the operating model for all simultaneous targets.
 *
 * Returns a vector of the current simultaneous target values in the operating model.
 * If the target is relative, the method calculates the current relative state.
 * The values can be compared to the desired target values from get_target_value(). 
 * At the moment, targets can not be specified for individual Units.
 * \param target_no References the target column in the control dataframe. Starts at 1.
 * \param sim_target_no References the simultaneous target in the target set. Starts at 1.
 */
std::vector<adouble> operatingModel::get_target_value_hat(const int target_no, const int sim_target_no) const{
    // Rprintf("In get_target_value_hat sim_target_no\n");
    Rprintf("sim_target_no: %i\n", sim_target_no);
    // Target information: type, fishery, catch, biol, etc
    auto niter = get_niter();
    unsigned int year = ctrl.get_target_int_col(target_no, sim_target_no, "year");
    unsigned int season = ctrl.get_target_int_col(target_no, sim_target_no, "season");
    Rprintf("year: %i, season: %i\n", year, season);
    // Could add unit here if necessary.
    fwdControlTargetType target_type = ctrl.get_target_type(target_no, sim_target_no);
    unsigned int fishery_no = ctrl.get_target_int_col(target_no, sim_target_no, "fishery");
    unsigned int catch_no = ctrl.get_target_int_col(target_no, sim_target_no, "catch");
    unsigned int biol_no = ctrl.get_target_int_col(target_no, sim_target_no, "biol");
    Rprintf("fishery no: %i, catch no: %i, biol no: %i\n", fishery_no, catch_no, biol_no);
    // Set indices for subsetting the target values
    // Need to know unit range but could be biol only (total catch), or catch only (total catch from several biols), or fishery only (econ)
    // If biol not NA, choose unit from biol
    unsigned int nunit = 1;
    if (!Rcpp::IntegerVector::is_na(biol_no)){
        nunit = biols(biol_no).n().get_nunit();
    }
    // Else if catch not NA choose unit from catch
    else if (!Rcpp::IntegerVector::is_na(catch_no)){
        nunit = fisheries(fishery_no, catch_no).landings_n().get_nunit();
    }
    //Rprintf("nunit: %i\n", nunit);
    // Else unit comes from fishery which is always 1
    std::vector<unsigned int> indices_min = {year,1,season,1,1};
    std::vector<unsigned int> indices_max = {year,nunit,season,1,niter};
    // Get the current absolute values, i.e. not relative, as FLQuant
    FLQuantAD target_value = eval_om(target_type, fishery_no, catch_no, biol_no, indices_min, indices_max);
    // Are we dealing with absolute or relative values?
    // Has to be relative in time (can be same timestep, but must still be notified) and relative in fishery
    unsigned int rel_year = ctrl.get_target_int_col(target_no, sim_target_no, "relYear"); 
    unsigned int rel_season = ctrl.get_target_int_col(target_no, sim_target_no, "relSeason");
    unsigned int rel_fishery = ctrl.get_target_int_col(target_no, sim_target_no, "relFishery");
    unsigned int rel_catch = ctrl.get_target_int_col(target_no, sim_target_no, "relCatch");
    unsigned int rel_biol = ctrl.get_target_int_col(target_no, sim_target_no, "relBiol");
    //Rprintf("rel_fishery no: %i, rel_catch no: %i, rel_biol no: %i, rel_year: %i, rel_season: %i\n", rel_fishery, rel_catch, rel_biol, rel_year, rel_season);
    bool target_rel_year_na = Rcpp::IntegerVector::is_na(rel_year);
    bool target_rel_season_na = Rcpp::IntegerVector::is_na(rel_season);
    bool target_rel_fishery_na = Rcpp::IntegerVector::is_na(rel_fishery);
    bool target_rel_catch_na = Rcpp::IntegerVector::is_na(rel_catch);
    bool target_rel_biol_na = Rcpp::IntegerVector::is_na(rel_biol);
    // If one of them is relative, then we have a relative target
    // Add check if relative NA makes sense
    // If relFishery we must have relCatch
    if (target_rel_fishery_na ^ target_rel_catch_na){
        Rcpp::stop("In operatingModel::get_target_value_hat. If relFishery specified, relCatch must also be specified (and vice versa)\n.");
    }
    // If relFishery OR relBiol, we must have relYear AND relSeason
    if (((!target_rel_fishery_na | !target_rel_biol_na) ^ (!target_rel_year_na & !target_rel_season_na))){
        Rcpp::stop("In operatingModel::get_target_value_hat. If relFishery and relCatch or relBiol specified, relYear and relSeason must also be specified (and vice versa). \n.");
    }
    if (!target_rel_year_na){
        Rprintf("Relative target target\n");
        // Set indices for subsetting the relative target values
        // Need to know unit range but could be biol only (total catch), or catch only (total catch from several biols), or fishery only (econ)
        // If biol not NA, choose unit from biol
        unsigned int rel_nunit = 1;
        if (!Rcpp::IntegerVector::is_na(rel_biol)){
            rel_nunit = biols(rel_biol).n().get_nunit();
        }
        // Else if catch not NA choose unit from catch
        else if (!Rcpp::IntegerVector::is_na(rel_catch)){
            rel_nunit = fisheries(rel_fishery, rel_catch).landings_n().get_nunit();
        }
        //Rprintf("rel_nunit: %i\n", rel_nunit);
        std::vector<unsigned int> rel_indices_min = {rel_year,1,rel_season,1,1};
        std::vector<unsigned int> rel_indices_max = {rel_year,rel_nunit,rel_season,1,niter};
        FLQuantAD rel_target_value = eval_om(target_type, rel_fishery, rel_catch, rel_biol, rel_indices_min, rel_indices_max);
        target_value = target_value / rel_target_value;
    }
    std::vector<adouble> value = target_value.get_data();
    return value;
} 
//@}

/*! \name Get the desired target values in the operating model
 */
//@{
/*! \brief Get the desired target values in the operating model for all simultaneous targets in a target set.
 * If the target values are relative, the returned values are the proportions from the control object, not the actual values.
 * If values are based on max / min some calculation is required.
 * Returns a vector of the simultaneous target values of a target set. 
 * Needs to return adouble because if min / max target, the returned value will depend on current effort value.
 * \param target_no References the target column in the control dataframe. Starts at 1.
 */
std::vector<adouble> operatingModel::get_target_value(const int target_no) const{
    //Rprintf("In get_target_value all sim targets\n");
    auto nsim_target = ctrl.get_nsim_target(target_no);
    std::vector<adouble> value;
    for (auto sim_target_count = 1; sim_target_count <= nsim_target; ++sim_target_count){
        auto sim_target_value = get_target_value(target_no, sim_target_count);
        value.insert(value.end(), sim_target_value.begin(), sim_target_value.end());
    }
    return value;
}
/*! \brief Get the desired target values in the operating model for a single simultaneous target
 * If the target values are relative, the returned values are the proportions from the control object, not the actual values.
 * If values are based on max / min some calculation is required.
 * Returns a vector of values of the simultaneous target from the target set. 
 * Needs to return adouble because if min / max target, the returned value will depend on current effort value.
 * Note that targets cannot currently be specified by Unit. 
 * \param target_no References the target column in the control dataframe. Starts at 1.
 * \param sim_target_no References the target column in the control dataframe. Starts at 1.
 */
std::vector<adouble> operatingModel::get_target_value(const int target_no, const int sim_target_no) const{
    //Rprintf("Get target value\n");
    // Pull out values for all iterations for the sim targets from the control object
    auto niter = get_niter(); // number of iters taken from effort of first fishery
    auto ctrl_niter = ctrl.get_niter();
    // iters in ctrl object may be less than iters in effort (1 or n)
    // Check if 1 or n (but ctrl niters cannot be bigger than effort iters)
    //Rprintf("ctrl iters: %i\n", ctrl_niter);
    if ((niter != ctrl_niter) & (ctrl_niter != 1)){
        Rcpp::stop("In operatingModel::get_target_value. Iterations in control object must be 1 or equal to those in fishing effort.\n");
    }
    std::vector<adouble> value(niter);
    // Are we dealing with a min / max value?
    // If so we need to get the current state of the operating model to compare with
    // This should not ask for a min / max column in target, but check the first value of min / max column in iters
    //double max_col = ctrl.get_target_num_col(target_no, sim_target_no, "max");
    //double min_col = ctrl.get_target_num_col(target_no, sim_target_no, "min");
    std::vector<double> max_col = ctrl.get_target_value(target_no, sim_target_no, 3); 
    std::vector<double> min_col = ctrl.get_target_value(target_no, sim_target_no, 1); 
    // Just check first iteration of min and max values
    bool max_na = Rcpp::NumericVector::is_na(max_col[0]);
    bool min_na = Rcpp::NumericVector::is_na(min_col[0]);
    if (!max_na | !min_na){
        // Get current value in OM to compare to the min and max
        std::vector<adouble> current_value = get_target_value_hat(target_no, sim_target_no);
        value = current_value;
        if(!max_na){
            Rprintf("Max target\n");
            std::vector<double> ctrl_value = ctrl.get_target_value(target_no, sim_target_no, 3);
            // In case of only 1 ctrl iter we need to blow up niter
            std::vector<adouble> ctrl_value_long(niter);
            if (niter > ctrl_niter){
                std::fill(ctrl_value_long.begin(), ctrl_value_long.end(), ctrl_value[0]);
            }
            else {
                ctrl_value_long.assign(ctrl_value.begin(), ctrl_value.end());
            }
            // No need to use CppAD Conditional as target value is evaluated before the operation sequence, i.e. not on the tape
            //for (auto i=0; i < value.size(); ++i){
            //    auto temp_value = value[i];
            //    Rprintf("value[%i]: %f\n", i, Value(value[i]));
            //    Rprintf("ctrl_value_long[%i]: %f\n", i, Value(ctrl_value_long[i]));
            //    value[i] = CppAD::CondExpLt(value[i], ctrl_value_long[i], value[i], ctrl_value_long[i]);
            //    Rprintf("so value[%i]: %f\n", i, Value(value[i]));
            //}
            std::transform(value.begin(), value.end(), ctrl_value_long.begin(), value.begin(), [](adouble x, adouble y) {return std::min(x, y);});
        }
        if(!min_na){
            Rprintf("Min target\n");
            std::vector<double> ctrl_value = ctrl.get_target_value(target_no, sim_target_no, 1);
            // In case of only 1 ctrl iter we need to blow up niter
            std::vector<adouble> ctrl_value_long(niter);
            if (niter > ctrl_niter){
                std::fill(ctrl_value_long.begin(), ctrl_value_long.end(), ctrl_value[0]);
            }
            else {
                ctrl_value_long.assign(ctrl_value.begin(), ctrl_value.end());
            }
            // Conditional AD max
            //for (auto i=0; i < value.size(); ++i){
            //    value[i] = CppAD::CondExpGt(value[i], ctrl_value_long[i], value[i], ctrl_value_long[i]);
            //}
            std::transform(value.begin(), value.end(), ctrl_value_long.begin(), value.begin(), [](adouble x, adouble y) {return std::max(x, y);});
        }
    }
    // If not min or max, just get the values from the control object
    else {
        std::vector<double> ctrl_value = ctrl.get_target_value(target_no, sim_target_no, 2);
        if (niter > ctrl_value.size()){
            std::fill(value.begin(), value.end(), ctrl_value[0]);
        }
        else {
            value.assign(ctrl_value.begin(), ctrl_value.end());
        }
    }
    //Rprintf("Returned value: %f\n", Value(value[0]));
    return value;
}
//@}







//// The timestep that fmult affects to calculate the target value
//// e.g. if Biomass is target, then adjust the fmult in the previous timestep
//// Add more target types to it
////int operatingModel::get_target_effort_timestep(const int target_no){
////    fwdControlTargetType target_type = ctrl.get_target_type(target_no);
////    unsigned int target_year = ctrl.get_target_year(target_no);
////    unsigned int target_season = ctrl.get_target_season(target_no);
////    unsigned int target_timestep = 0;
////    year_season_to_timestep(target_year, target_season, biols(1).n(), target_timestep);
////    // Biomass timesteps
////    if((target_type == target_ssb) ||
////       (target_type == target_biomass)){
////        target_timestep = target_timestep - 1;
////    }
////    return target_timestep;
////}
//
//*/



/*! \brief Returns the indices of the age range, starting at 0
 *
 * Returns the indices of the age dimension. These are only the same as the minAge and maxAge columns in the control object if the first age is 0.
 * \param target_no References the target column in the control dataframe. Starts at 1.
 * \param sim_target_no References the target column in the control dataframe. Starts at 1.
 */
std::vector<unsigned int> operatingModel::get_target_age_range_indices(const unsigned int target_no, const unsigned int sim_target_no) const {
    std::vector<std::string> age_names;
    // Get age names from biol in target. 
    int biol_no = ctrl.get_target_int_col(target_no, sim_target_no, "biol"); 
    if (!Rcpp::IntegerVector::is_na(biol_no)){
        age_names = Rcpp::as<std::vector<std::string> >(biols(biol_no).n().get_dimnames()[0]);
    }
    // If no biol target, get age names from catch.
    else {
        int fishery_no = ctrl.get_target_int_col(target_no, sim_target_no, "fishery"); 
        int catch_no = ctrl.get_target_int_col(target_no, sim_target_no, "catch"); 
        if (!Rcpp::IntegerVector::is_na(catch_no)){
            age_names = Rcpp::as<std::vector<std::string> >(fisheries(fishery_no, catch_no).landings_n().get_dimnames()[0]);
        }
        //  If no biol or catch target, give up.
        else {
            Rcpp::stop("In operatingModel::get_target_age_range_indices. biol_no and catch_no are NA. Unable to get age range.\n");
        }
    }
    std::vector<unsigned int> age_range = ctrl.get_age_range(target_no, sim_target_no); // Just values in the control, not the age indices
    std::vector<unsigned int> age_range_indices(2);
    // Use find() to match names - precheck in R that they exist - if not find, returns the last
    std::vector<std::string>::iterator age_min_iterator = find(age_names.begin(), age_names.end(), std::to_string(age_range[0]));
    if(age_min_iterator != age_names.end()){
        age_range_indices[0] = std::distance(age_names.begin(), age_min_iterator);
    }
    else {
        Rcpp::stop("minAge in control not found in dimnames of target object\n");
    }
    std::vector<std::string>::iterator age_max_iterator = find(age_names.begin(), age_names.end(), std::to_string(age_range[1]));
    if(age_max_iterator != age_names.end()){
        age_range_indices[1] = std::distance(age_names.begin(), age_max_iterator);
    }
    else {
        Rcpp::stop("maxAge in control not found in dimnames of target object\n");
    }
    return age_range_indices;
}

//---------------Target methods ----------------------------

/*! \name fbar
 * Calculate the mean instantaneous fishing mortality over the specified age range
 * Note that the indices are not the names of the ages, but the positions, starting at 1
 */
//@{
/*! \brief The mean instantaneous fishing mortality over the specified age range of a single biol from a single fishery / catch, subset over dimensions 2-6.
 * It is assumed that the fishery / catch actually fishes the biol (no check is made).
 * \param fishery_no the position of the fishery within the fisheries (starting at 1).
 * \param catch_no the position of the catch within the fishery (starting at 1).
 * \param biol_no the position of the biol within the biols (starting at 1).
 * \param indices_min The minimum indices quant, year, unit etc (length 6)
 * \param indices_max The maximum indices quant, year, unit etc (length 6)
*/
FLQuantAD operatingModel::fbar(const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    // Get the F, then mean over the first dimension
    FLQuantAD f = get_f(fishery_no, catch_no, biol_no, indices_min, indices_max); 
    FLQuantAD fbar = quant_mean(f);
    return fbar;
}

/*! \brief The total mean instantaneous fishing mortality over the specified age range of a single biol, subset over dimensions 2-6.
 * \param biol_no the position of the biol within the biols (starting at 1).
 * \param indices_min The minimum indices quant, year, unit etc (length 6)
 * \param indices_max The maximum indices quant, year, unit etc (length 6)
 */
FLQuantAD operatingModel::fbar(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    // Get the F, then mean over the first dimension
    FLQuantAD f = get_f(biol_no, indices_min, indices_max); 
    FLQuantAD fbar = quant_mean(f);
    return fbar;
}
//@}


/*! \brief The total landings mass from a single biol 
 * Sums up the current landings from each of the FLCatch objects that fish the biol
 * However, this only works if none of the FLCatch objects also fish on another biol (because then the total catches in the FLCatch come from a mix of biols).
 * In this case, we need to split the catches between the biols.
 * This is not yet implemented. 
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 5)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 5)
 */
FLQuantAD operatingModel::landings(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if(indices_min.size() != 5 | indices_max.size() != 5){
        Rcpp::stop("In operatingModel landings on a biol subset method. indices_min and max must be of length 5\n");
    }
    // Indices for the full FLQ, i.e. including first dimension
    std::vector<unsigned int> dim = biols(biol_no).n().get_dim();
    std::vector<unsigned int> quant_indices_min = indices_min;
    std::vector<unsigned int> quant_indices_max = indices_max;
    quant_indices_min.insert(quant_indices_min.begin(), 1);
    quant_indices_max.insert(quant_indices_max.begin(), dim[0]);
    // Empty quant for storage
    FLQuantAD total_landings(1, indices_max[0] - indices_min[0] + 1, indices_max[1] - indices_min[1] + 1, indices_max[2] - indices_min[2] + 1, indices_max[3] - indices_min[3] + 1, indices_max[4] - indices_min[4] + 1); 
    total_landings.fill(0.0);
    // Get the Fishery / Catches that catch the biol
    const Rcpp::IntegerMatrix FC =  ctrl.get_FC(biol_no);
    // Loop over the FCs that catch from that biol 
    for (int FC_counter = 0; FC_counter < FC.nrow(); ++FC_counter){
        // What biols are also fished by that FC
       std::vector<int> biols_fished = ctrl.get_B(FC(FC_counter, 0), FC(FC_counter, 1)); 
        // Do any of these FCs catch another biol - if so STOP and return error
        if (biols_fished.size() > 1){
            Rcpp::stop("In om::landings. Trying to get landings from a biol that is fished by an FLCatch that also fishes another biol. Not yet implemented.\n");
        }
        total_landings = total_landings + fisheries(FC(FC_counter,0), FC(FC_counter,1)).landings(indices_min, indices_max);
    }
    return total_landings;
}

/*! \brief The total discards from a single biol 
 * Sums up the current discards from each of the FLCatch objects that fish the biol
 * However, this only works if none of the FLCatch objects also fish on another biol (because then the total catches in the FLCatch come from a mix of biols).
 * In this case, we need to split the catches between the biols.
 * This is not yet implemented. 
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 5)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 5)
 */
FLQuantAD operatingModel::discards(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if(indices_min.size() != 5 | indices_max.size() != 5){
        Rcpp::stop("In operatingModel discards on a biol subset method. indices_min and max must be of length 5\n");
    }
    // Indices for the full FLQ, i.e. including first dimension
    std::vector<unsigned int> dim = biols(biol_no).n().get_dim();
    std::vector<unsigned int> quant_indices_min = indices_min;
    std::vector<unsigned int> quant_indices_max = indices_max;
    quant_indices_min.insert(quant_indices_min.begin(), 1);
    quant_indices_max.insert(quant_indices_max.begin(), dim[0]);
    // Empty quant for storage
    FLQuantAD total_discards(1, indices_max[0] - indices_min[0] + 1, indices_max[1] - indices_min[1] + 1, indices_max[2] - indices_min[2] + 1, indices_max[3] - indices_min[3] + 1, indices_max[4] - indices_min[4] + 1); 
    total_discards.fill(0.0);
    // Get the Fishery / Catches that catch the biol
    const Rcpp::IntegerMatrix FC =  ctrl.get_FC(biol_no);
    // Loop over the FCs that catch from that biol 
    for (int FC_counter = 0; FC_counter < FC.nrow(); ++FC_counter){
        // What biols are also fished by that FC
       std::vector<int> biols_fished = ctrl.get_B(FC(FC_counter, 0), FC(FC_counter, 1)); 
        // Do any of these FCs catch another biol - if so STOP and return error
        if (biols_fished.size() > 1){
            Rcpp::stop("In om::discards. Trying to get discards from a biol that is fished by an FLCatch that also fishes another biol. Not yet implemented.\n");
        }
        total_discards = total_discards + fisheries(FC(FC_counter,0), FC(FC_counter,1)).discards(indices_min, indices_max);
    }
    return total_discards;
}

/*! \brief Subset the total catches from a single biol 
 * If the FLCatch that catches the biol only catches from that biol, the catches are taken directly from the catches in the FLCatch object, i.e. they are not recalculated. Therefore, if effort or something has changed, this method will not reflect those changes.
 * If a biol is fished by a catch that also catches from another biol (i.e. a catch fishing on two sub stocks)
 * the catches from each biol have to be recalculated as there is no direct way of splitting the total catch
 * into the catches from each biol.
 * This means that the sum of the (recalculated) catches may not equal the total catch already stored in the object.
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 5)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 5)
 */
FLQuantAD operatingModel::catches(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if(indices_min.size() != 5 | indices_max.size() != 5){
        Rcpp::stop("In operatingModel catches on a biol subset method. indices_min and max must be of length 5\n");
    }
    FLQuantAD total_catches = landings(biol_no, indices_min, indices_max) + discards(biol_no, indices_min, indices_max);
    return total_catches;
}

/*! \brief The total landings abundance by age from a single biol 
 * Sums up the current landings from each of the FLCatch objects that fish the biol
 * However, this only works if none of the FLCatch objects also fish on another biol (because then the total catches in the FLCatch come from a mix of biols).
 * In this case, we need to split the catches between the biols.
 * This is not yet implemented. 
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 6)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 6)
 */
FLQuantAD operatingModel::landings_n(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if(indices_min.size() != 6 | indices_max.size() != 6){
        Rcpp::stop("In operatingModel landings_n on a biol subset method. indices_min and max must be of length 6\n");
    }
    // Empty quant for storage
    FLQuantAD total_landings_n(indices_max[0] - indices_min[0] + 1, indices_max[1] - indices_min[1] + 1, indices_max[2] - indices_min[2] + 1, indices_max[3] - indices_min[3] + 1, indices_max[4] - indices_min[4] + 1, indices_max[5] - indices_min[5] + 1); 
    total_landings_n.fill(0.0);
    // Get the Fishery / Catches that catch the biol
    const Rcpp::IntegerMatrix FC =  ctrl.get_FC(biol_no);
    // Loop over the FCs that catch from that biol 
    for (int FC_counter = 0; FC_counter < FC.nrow(); ++FC_counter){
        // What biols are also fished by that FC
       std::vector<int> biols_fished = ctrl.get_B(FC(FC_counter, 0), FC(FC_counter, 1)); 
        // Do any of these FCs catch another biol - if so STOP and return error
        if (biols_fished.size() > 1){
            Rcpp::stop("In om::landings_n. Trying to get landings from a biol that is fished by an FLCatch that also fishes another biol. Not yet implemented.\n");
        }
        total_landings_n = total_landings_n + fisheries(FC(FC_counter,0), FC(FC_counter,1)).landings_n(indices_min, indices_max);
    }
    return total_landings_n;
}

/*! \brief The total discards abundance by age from a single biol 
 * Sums up the current discards from each of the FLCatch objects that fish the biol
 * However, this only works if none of the FLCatch objects also fish on another biol (because then the total catches in the FLCatch come from a mix of biols).
 * In this case, we need to split the catches between the biols.
 * This is not yet implemented. 
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 6)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 6)
 */
FLQuantAD operatingModel::discards_n(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if(indices_min.size() != 6 | indices_max.size() != 6){
        Rcpp::stop("In operatingModel discards_n on a biol subset method. indices_min and max must be of length 6\n");
    }
    // Empty quant for storage
    FLQuantAD total_discards_n(indices_max[0] - indices_min[0] + 1, indices_max[1] - indices_min[1] + 1, indices_max[2] - indices_min[2] + 1, indices_max[3] - indices_min[3] + 1, indices_max[4] - indices_min[4] + 1, indices_max[5] - indices_min[5] + 1); 
    total_discards_n.fill(0.0);
    // Get the Fishery / Catches that catch the biol
    const Rcpp::IntegerMatrix FC =  ctrl.get_FC(biol_no);
    // Loop over the FCs that catch from that biol 
    for (int FC_counter = 0; FC_counter < FC.nrow(); ++FC_counter){
        // What biols are also fished by that FC
       std::vector<int> biols_fished = ctrl.get_B(FC(FC_counter, 0), FC(FC_counter, 1)); 
        // Do any of these FCs catch another biol - if so STOP and return error
        if (biols_fished.size() > 1){
            Rcpp::stop("In om::discards_n. Trying to get discards from a biol that is fished by an FLCatch that also fishes another biol. Not yet implemented.\n");
        }
        total_discards_n = total_discards_n + fisheries(FC(FC_counter,0), FC(FC_counter,1)).discards_n(indices_min, indices_max);
    }
    return total_discards_n;
}

/*! \brief Subset the total catch numbers from a single biol 
 * The catches are taken directly from the catches in the FLCatch object, i.e. they are not recalculated. Therefore, if effort or something has changed, this method will not reflect those changes.
 * If a biol is fished by a catch that also catches from another biol (i.e. a catch fishing on two sub stocks)
 * the catches from each biol have to be recalculated as there is no direct way of splitting the total catch
 * into the catches from each biol. This is not yet implemented.
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 6)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 6)
 */
FLQuantAD operatingModel::catch_n(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
    if(indices_min.size() != 6 | indices_max.size() != 6){
        Rcpp::stop("In operatingModel catch_n on a biol subset method. indices_min and max must be of length 5\n");
    }
    FLQuantAD total_catch_n = landings_n(biol_no, indices_min, indices_max) + discards_n(biol_no, indices_min, indices_max);
    return total_catch_n;
}

////-------------------------------------------------
//
//
////// Define some pointers to function 
////typedef std::vector<adouble> (*funcPtrAD)(const std::vector<adouble>& x);
////typedef std::vector<double> (*funcPtr)(const std::vector<double>& x);
////
////
////// Returning a whole ADFun might be expensive - return as pointer?
////CppAD::ADFun<double> tape_my_func(std::vector<double>& xin, funcPtrAD fun){
////    // How to easily make an adouble vector?
////    std::vector<adouble> x(xin.size());
////    std::copy(xin.begin(), xin.end(), x.begin());
////    // Tape on
////    CppAD::Independent(x);
////    std::vector<adouble> y = fun(x);
////    CppAD::ADFun<double> f(x, y);
////    return f;
////}
////
////// [[Rcpp::export]]
////Rcpp::List solve_my_func(std::vector<double> xin, Rcpp::XPtr<funcPtrAD> xptr){
////    funcPtrAD func = *xptr; // the typedef
////    CppAD::ADFun<double> fun = tape_my_func(xin, func);
////    std::vector<int> success = newton_raphson(xin, fun, 1, 2, -1e9, 1e9, 50, 1e-12);
////    return Rcpp::List::create(Rcpp::Named("indep") = xin,
////                              Rcpp::Named("success") = success);
////}
////
