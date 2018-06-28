/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

// For timing functions
//#include <chrono>

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
  for (unsigned int fishery_no=1; fishery_no <= fisheries_in.get_nfisheries(); ++fishery_no){
    std::vector<unsigned int> effort_dim = fisheries_in(fishery_no).effort().get_dim();
    for (unsigned int catch_no=1; catch_no <= fisheries_in(fishery_no).get_ncatches(); ++catch_no){
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
  for (unsigned int biol_no=1; biol_no <= biols_in.get_nbiols();  ++biol_no){
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
  unsigned int ctrl_iters = ctrl_in.get_niter();
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

/*! \brief Does spawning occuring before fishing
 *
 * Any range of indices can be used. If ANY of them has spwn <= hperiod[1,] then true is returned.
 * False is only returned if ALL of the indices has spwn > hperiod[1,].
 * This is best used for a single timestep.
 * If spawn is NA, then it is still False.
 *
 * \param biol_no The position of the biol in the biols list (starting at 1)
 * \param indices_min The minimum indices: year, unit, season, area, iter (length 5).
 * \param indices_max The maximum indices: year, unit, season, area, iter (length 5).
 */
bool operatingModel::spawn_before_fishing(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
  // Check indices_min and indices_max are of length 6
  if ((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel::spawn_before_fishing subsetter. Indices not of length 5\n");
  }
  auto sbf = false;
  // Loop over all Fs that catch B
  // Loop over indices
  // if spwn <= hperiod[1,] then return true
  FLQuant spwn = biols(biol_no).spwn();
  auto Fs =  ctrl.get_F(biol_no); // unique Fs fishing that B
  for (unsigned int f_counter=0; f_counter < Fs.size(); ++f_counter){
    FLQuant hperiod = fisheries(Fs[f_counter]).hperiod();
    for (auto year_count = indices_min[0]; year_count <= indices_max[0]; ++year_count){
      for (auto unit_count = indices_min[1]; unit_count <= indices_max[1]; ++unit_count){
        for (auto season_count = indices_min[2]; season_count <= indices_max[2]; ++season_count){
          for (auto area_count = indices_min[3]; area_count <= indices_max[3]; ++area_count){
            for (auto iter_count = indices_min[4]; iter_count <= indices_max[4]; ++iter_count){
              if (spwn(1,year_count, unit_count, season_count, area_count, iter_count) <= hperiod(1,year_count, unit_count, season_count, area_count, iter_count)){
                sbf = true;
              }
    }}}}}
  }
  return sbf;
}

/*! \brief Does fishing happen before spawning
 *
 * Any range of indices can be used. If ANY of them has spwn > hperiod[1,] then true is returned.
 * False is only returned if ALL of the indices has hperiod[1,] > spwn.
 * This is best used for a single timestep.
 * If spwn is NA, then it is False
 *
 * \param biol_no The position of the biol in the biols list (starting at 1)
 * \param indices_min The minimum indices: year, unit, season, area, iter (length 5).
 * \param indices_max The maximum indices: year, unit, season, area, iter (length 5).
 */
bool operatingModel::fishing_before_spawn(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
  // Check indices_min and indices_max are of length 6
  if ((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel::spawn_before_fishing subsetter. Indices not of length 5\n");
  }
  auto fbs = false;
  // Loop over all Fs that catch B
  // Loop over indices
  // if spwn <= hperiod[1,] then return true
  FLQuant spwn = biols(biol_no).spwn();
  auto Fs =  ctrl.get_F(biol_no); // unique Fs fishing that B
  for (unsigned int f_counter=0; f_counter < Fs.size(); ++f_counter){
    FLQuant hperiod = fisheries(Fs[f_counter]).hperiod();
    for (auto year_count = indices_min[0]; year_count <= indices_max[0]; ++year_count){
      for (auto unit_count = indices_min[1]; unit_count <= indices_max[1]; ++unit_count){
        for (auto season_count = indices_min[2]; season_count <= indices_max[2]; ++season_count){
          for (auto area_count = indices_min[3]; area_count <= indices_max[3]; ++area_count){
            for (auto iter_count = indices_min[4]; iter_count <= indices_max[4]; ++iter_count){
              if (spwn(1,year_count, unit_count, season_count, area_count, iter_count) > hperiod(1,year_count, unit_count, season_count, area_count, iter_count)){
                fbs = true;
              }
    }}}}}
  }
  return fbs;
}

/*! \brief Calculates the exponent of negative total mortality before spawning
 *
 * Calculated as exp(-Fprespwn + m*spwn) where the natural mortality, m, is assumed to be constant over the timestep
 * and Fprespwn is how much fishing mortality happened before spawning.
 * Note that if the spwn member of the FLBiol is NA (no spawning), then 0.0 is returned.
 * This method is used for SSB / SRP calculation.
 * SSB should be 0 when spwn is NA, hence returning 0.0 when spwn is NA
 *
 * \param biol_no The position of the biol in the biols list (starting at 1)
 * \param indices_min The minimum indices: year, unit, season, area, iter (length 5).
 * \param indices_max The maximum indices: year, unit, season, area, iter (length 5).
 */
FLQuantAD operatingModel::get_exp_z_pre_spwn(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
  // Check indices_min and indices_max are of length 6
  if ((indices_min.size() != 6) | (indices_max.size() != 6)){
    Rcpp::stop("In operatingModel::get_z_pre_spwn subsetter. Indices not of length 6\n");
  }
  // Make dim of the Fprespwn object: max - min + 1
  std::vector<unsigned int> qdim(6,0.0);
  std::transform(indices_max.begin(), indices_max.end(), indices_min.begin(), qdim.begin(), [](unsigned int x, unsigned int y){return x-y+1;});
  FLQuantAD f_pre_spwn(qdim);
  // Get Fprespawn = F * Fpropspawn for each F fishing the biol
  // 1. ID which F are fishing that B (timing is determined by hperiod and spwn)
  // 2. Get F(F,C,B)
  // 3. Get Fpropspawn(F,C,B)
  // 4. sum (F * Fpropspawn)
  const Rcpp::IntegerMatrix FC =  ctrl.get_FC(biol_no);
  std::vector<unsigned int> indices_min5 = {indices_min[1], indices_min[2], indices_min[3], indices_min[4], indices_min[5]}; 
  std::vector<unsigned int> indices_max5 = {indices_max[1], indices_max[2], indices_max[3], indices_max[4], indices_max[5]}; 
  for (int f_counter=0; f_counter < FC.nrow(); ++f_counter){
    FLQuantAD tempf = get_f(FC(f_counter,0), FC(f_counter,1), biol_no, indices_min, indices_max); 
    FLQuant temp_prop_spwn = f_prop_spwn(FC(f_counter,0), biol_no, indices_min5, indices_max5);
    FLQuantAD temp_propf = sweep_mult(tempf, temp_prop_spwn);
    f_pre_spwn = f_pre_spwn + temp_propf;
  }
  // Get m pre spwn - need to adjust subsetter for the first dimension
  std::vector<unsigned int> spwn_indices_max = indices_max;
  spwn_indices_max[0] = 1;
  FLQuant m_pre_spwn = sweep_mult(biols(biol_no).m(indices_min, indices_max), biols(biol_no).spwn(indices_min, spwn_indices_max));
  FLQuantAD z_pre_spwn = f_pre_spwn + m_pre_spwn;
  FLQuantAD exp_z_pre_spwn = exp(-1.0 * z_pre_spwn);
  // exp_z_pre_spwn needs to be 0 when spwn is 0
  // If spwn in biol is NA, then no spawning has occured. Therefore the amount of F and M before spawning is 0
  // A spwn of NA will result in the exp_z_pre_spwn of NA
  // So we find NA in spwn and replace in exp_z_pre_spwn with 0.0
  FLQuant spwn_temp = biols(biol_no).spwn(indices_min, spwn_indices_max);
  // Hacky because exp_z is age structured and spwn is not
  for (unsigned int iter_count = 1; iter_count <= qdim[5]; ++iter_count){
    for (unsigned int area_count = 1; area_count <= qdim[4]; ++area_count){
      for (unsigned int season_count = 1; season_count <= qdim[3]; ++season_count){
        for (unsigned int unit_count = 1; unit_count <= qdim[2]; ++unit_count){
          for (unsigned int year_count = 1; year_count <= qdim[1]; ++year_count){
            // If spwn is NA set all ages of pre_spwn to 0.0
            if (Rcpp::NumericVector::is_na(spwn_temp(1,year_count, unit_count, season_count, area_count, iter_count))){
              for (unsigned int age_count = 1; age_count <= qdim[0]; ++age_count){
                exp_z_pre_spwn(age_count, year_count, unit_count, season_count, area_count, iter_count) = 0.0;
              }
            }
  }}}}}
  return exp_z_pre_spwn;
}

/*! \brief Calculates the total spawning recruitment potential of a biol at the time of spawning
 *
 * Calculated as SSB: N*mat*wt*exp(-Fprespwn - m*spwn) summed over age dimension
 * where the natural mortality m is assumed to be constant over the timestep
 * and Fprespwn is how much fishing mortality happened before spawning.
 * The SRP is summed over all units 
 * \param biol_no The position of the biol in the biols list (starting at 1)
 * \param indices_min The minimum indices: year, unit, season, area, iter (length 5).
 * \param indices_max The maximum indices: year, unit, season, area, iter (length 5).
 */
FLQuantAD operatingModel::total_srp(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
  if ((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel::total_srp subsetter. Indices not of length 5 (no age index)\n");
  }
  FLQuantAD usrp = ssf(biol_no, indices_min, indices_max);
  // FLQuantAD usrp = srp(biol_no, indices_min, indices_max);
  // Sum over units
  FLQuantAD total_srp = unit_sum(usrp);
  return total_srp;
}

/*! \brief Calculates the spawning recruitment potential of each unit a biol at the time of spawning
 *
 * Calculated as SSB: N*mat*wt*exp(-Fprespwn - m*spwn) summed over age dimension
 * where the natural mortality m is assumed to be constant over the timestep
 * and Fprespwn is how much fishing mortality happened before spawning.
 * Each unit is kept separate.
 * \param biol_no The position of the biol in the biols list (starting at 1)
 * \param indices_min The minimum indices: year, unit etc (length 5)
 * \param indices_max The maximum indices: year, unit etc (length 5)
 */
FLQuantAD operatingModel::srp(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
  // Check indices_min and indices_max are of length 5
  if ((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel::srp subsetter. Indices not of length 5\n");
  }
  // Add age range to input indices
  std::vector<unsigned int> qindices_min = indices_min;
  qindices_min.insert(qindices_min.begin(), 1);
  std::vector<unsigned int> dim = biols(biol_no).n().get_dim();
  std::vector<unsigned int> qindices_max = indices_max;
  qindices_max.insert(qindices_max.begin(), dim[0]);
  FLQuantAD exp_z_pre_spwn = get_exp_z_pre_spwn(biol_no, qindices_min, qindices_max);

  // Get srp: N*mat*wt*exp(-Fprespwn - m*spwn) summed over age dimension
  FLQuantAD srp = quant_sum(
    biols(biol_no).n(qindices_min, qindices_max) *
    biols(biol_no).wt(qindices_min, qindices_max) *
    biols(biol_no).mat(qindices_min, qindices_max) * exp_z_pre_spwn);
  return srp;
}

/*! \brief Calculates the spawning stock fecundity of each unit a biol at the time of spawning
 *
 * Calculated as SSF: N*fec*mat*exp(-Fprespwn - m*spwn) summed over age dimension
 * where the natural mortality m is assumed to be constant over the timestep
 * and Fprespwn is how much fishing mortality happened before spawning.
 * Each unit is kept separate.
 * \param biol_no The position of the biol in the biols list (starting at 1)
 * \param indices_min The minimum indices: year, unit etc (length 5)
 * \param indices_max The maximum indices: year, unit etc (length 5)
 */
FLQuantAD operatingModel::ssf(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
  // Check indices_min and indices_max are of length 5
  if ((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel::ssf subsetter. Indices not of length 5\n");
  }
  // Add age range to input indices
  std::vector<unsigned int> qindices_min = indices_min;
  qindices_min.insert(qindices_min.begin(), 1);
  std::vector<unsigned int> dim = biols(biol_no).n().get_dim();
  std::vector<unsigned int> qindices_max = indices_max;
  qindices_max.insert(qindices_max.begin(), dim[0]);
  FLQuantAD exp_z_pre_spwn = get_exp_z_pre_spwn(biol_no, qindices_min, qindices_max);

  // Get srp: N*mat*wt*exp(-Fprespwn - m*spwn) summed over age dimension
  FLQuantAD ssf = quant_sum(
    biols(biol_no).n(qindices_min, qindices_max) *
    biols(biol_no).fec(qindices_min, qindices_max) *
    biols(biol_no).mat(qindices_min, qindices_max) * exp_z_pre_spwn);
  return ssf;
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
  // Check indices_min and indices_max are of length 5
  if ((indices_min.size() != 5) | (indices_max.size() != 5)){
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
            // hperiod has length 1 in the unit dimension
            double fstart = hperiod(1,year_count, 1, season_count, area_count, iter_count);
            double fend = hperiod(2,year_count, 1, season_count, area_count, iter_count);
            double spwn = spwn_all(1,year_count, unit_count, season_count, area_count, iter_count);
            // If fstart is NA then there is no fishing in that timestep. propf is then 0 
            if (Rcpp::NumericVector::is_na(fstart)){
              propf = 0.0;
            }
            else {
              if (fend < spwn){
                propf = 1.0;
              }
              else if (fstart > spwn){
                propf = 0.0;
              }
              else {
                propf = (spwn - fstart) / (fend - fstart);
              }
            }
      // Dump it in the right place - ugliness abounds
      propf_out(1, year_count - indices_min[0] +1, unit_count - indices_min[1] +1, season_count - indices_min[2] +1, area_count - indices_min[3] +1, iter_count - indices_min[4] +1) = propf; 
  }}}}}
  return propf_out;
}

/*! \brief Calculates the recruitment for each iteration for a particular unit of a particular fwdBiol at a particular timestep
 *
 * Each unit of a biol can recruit at a different time so we need to specify the unit we want the recruitment for.
 * However, all units have the same timelag between spawning and recruitment.
 * Recruitment is based on the total SRP of all units.
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
  // SRP for all units
  std::vector<unsigned int> srp_indices_min{srp_year, 1, srp_season, area, 1};
  std::vector<unsigned int> srp_indices_max{srp_year, biol_dim[2], srp_season, area, niter};
  FLQuantAD srpq = total_srp(biol_no, srp_indices_min, srp_indices_max);
  // Initial indices of the SR params are those of the recruitment timestep for the Biol
  // SR params are in step with the Recruitment not the SRP
  unsigned int initial_params_year = 0;
  unsigned int initial_params_season = 0;
  timestep_to_year_season(rec_timestep, biol_dim[3], initial_params_year, initial_params_season);
  std::vector<unsigned int> initial_params_indices{initial_params_year, unit, initial_params_season, area, 1};
  FLQuantAD rec = biols(biol_no).predict_recruitment(srpq, initial_params_indices);
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
  bool verbose = false;
  if(verbose){Rprintf("In operatingModel::get_f FCB subsetter\n");}
  if ((indices_min.size() != 6) | (indices_max.size() != 6)){
    Rcpp::stop("In operatingModel get_f subsetter. Indices not of length 6\n");
  }
  // Lop off the first value from the indices to get indices without quant - needed for effort and catch_q
  std::vector<unsigned int> indices_min5(indices_min.begin()+1, indices_min.end());
  std::vector<unsigned int> indices_max5(indices_max.begin()+1, indices_max.end());
  if(verbose){Rprintf("Getting biomass\n");}
  FLQuantAD biomass = biols(biol_no).biomass(indices_min5, indices_max5);
  if(verbose){Rprintf("Got biomass\n");}
  FLQuantAD sel = fisheries(fishery_no, catch_no).catch_sel()(indices_min, indices_max);
  // Need special subsetter for effort as always length 1 in the unit dimension
  std::vector<unsigned int> effort_indices_min5{indices_min5[0], 1, indices_min5[2], indices_min5[3], indices_min5[4]};  
  std::vector<unsigned int> effort_indices_max5{indices_max5[0], 1, indices_max5[2], indices_max5[3], indices_max5[4]};  
  if(verbose){Rprintf("Getting effort\n");}
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
FLQuantAD operatingModel::get_f(const unsigned int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
  bool verbose = false;
  if(verbose){Rprintf("In operatingModel::get_f biol subsetter\n");}
  if (biol_no > biols.get_nbiols()){
    Rcpp::stop("In OM get_f biol. biol_no is greater than number of biols\n");
  }
  // We need to know the Fishery / Catches that catch the biol
  const Rcpp::IntegerMatrix FC =  ctrl.get_FC(biol_no);
  // What happens if no-one is fishing that biol? FC.nrow() == 0 so loop never triggers
  FLQuantAD total_f(indices_max[0] - indices_min[0] + 1, indices_max[1] - indices_min[1] + 1, indices_max[2] - indices_min[2] + 1, indices_max[3] - indices_min[3] + 1, indices_max[4] - indices_min[4] + 1, indices_max[5] - indices_min[5] + 1); 
  total_f.fill(0.0);
  if(verbose){Rprintf("About to loop over FCs\n");}
  for (int f_counter=0; f_counter < FC.nrow(); ++f_counter){
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
FLQuantAD operatingModel::get_nunit_z(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
  if ((indices_min.size() != 6) | (indices_max.size() != 6)){
    Rcpp::stop("In operatingModel get_unit_z subsetter. Indices not of length 6\n");
  }
  FLQuantAD n = biols(biol_no).n(indices_min, indices_max);
  FLQuantAD surv = survivors(biol_no, indices_min, indices_max);
  FLQuantAD nunit_z = -1.0 * log(unit_sum(surv) / unit_sum(n));
  return nunit_z; 
}

/*! \name get_nunit_f
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
 * Alternatively:
 * F = C log (N / survivors) / (N - survivors)
 *
 * It is not possible to calculate the unit combined F when the Biol is fished by a Catch that also fishes on other Biols.
 * This is because it is not possible to get the portion of catches of that Catch that come from a particular Biol.
 * A check is made and an error thrown if tried.
 *
 */
//@{
/*! \brief Get the fishing mortality on a Biol, collapsed over the unit dimension.
 * \param biol_no the position of the biol within the biols (starting at 1).
 * \param indices_min minimum indices for subsetting (quant - iter, vector of length 6)
 * \param indices_max maximum indices for subsetting (quant - iter, vector of length 6)
 */
FLQuantAD operatingModel::get_nunit_f(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
  if ((indices_min.size() != 6) | (indices_max.size() != 6)){
    Rcpp::stop("In operatingModel get_nunit_f B subsetter. Indices not of length 6\n");
  }
  // Test for shared catch
  if (ctrl.shared_catch(biol_no)){
    Rcpp::stop("In operatingModel get_nunit_f B. Not possible to get unit combined F of a Biol that is fished by a Catch that also fishes on other Biols. This is because it is not possible to get the portion of catches of that Catch that come from a particular Biol.\n");
  }
  FLQuantAD unit_catch =  unit_sum(catch_n(biol_no, indices_min, indices_max));
  FLQuantAD unit_surv = unit_sum(survivors(biol_no, indices_min, indices_max));
  FLQuantAD unit_n = unit_sum(biols(biol_no).n(indices_min, indices_max));
  FLQuantAD nunit_f = (unit_catch * log(unit_n / unit_surv)) / (unit_n - unit_surv);
  return nunit_f;
}
/*! \brief Get the fishing mortality from an individual Catch on a Biol, collapsed over the unit dimension.
 * \param fishery_no the position of the Fishery within the Fisheries (starting at 1).
 * \param catch_no the position of the Catch within the Catches (starting at 1).
 * \param biol_no the position of the Biol within the Biols (starting at 1).
 * \param indices_min minimum indices for subsetting (quant - iter, vector of length 6)
 * \param indices_max maximum indices for subsetting (quant - iter, vector of length 6)
 */
FLQuantAD operatingModel::get_nunit_f(const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
  if ((indices_min.size() != 6) | (indices_max.size() != 6)){
    Rcpp::stop("In operatingModel get_nunit_f FCB subsetter. Indices not of length 6\n");
  }
  // Test for shared catch
  if (ctrl.shared_catch(biol_no)){
    Rcpp::stop("In operatingModel get_nunit_f FCB. Not possible to get unit combined F of a Biol that is fished by a Catch that also fishes on other Biols. This is because it is not possible to get the portion of catches of that Catch that come from a particular Biol.\n");
  }
  FLQuantAD unit_catch = unit_sum(fisheries(fishery_no, catch_no).catch_n(indices_min, indices_max));
  FLQuantAD nunit_z = get_nunit_z(biol_no, indices_min, indices_max);
  FLQuantAD unit_n = unit_sum(biols(biol_no).n(indices_min, indices_max));
  FLQuantAD nunit_f = (unit_catch * nunit_z) / ((1.0-exp(-1.0 * nunit_z)) * unit_n);
  return nunit_f;
}
//@}

/*! \brief Calculate the survivors at the end of a timestep.
 *   Survivors are based on the fishing effort and population abundance at the end of a timestep
 *   Calculation is based on the the Baranov equation: N2 = N1 * (exp -Z)
 *   This assumes that the instantaneous rate of fishing and natural mortalities are constant over the timestep and occur simultaneously.
 * \param biol_no Position of the fwdBiol in the fwdBiols list.
 * \param indices_min minimum indices for subsetting (quant - iter, vector of length 6)
 * \param indices_max maximum indices for subsetting (quant - iter, vector of length 6)
 */
FLQuantAD operatingModel::survivors(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
  bool verbose = false;
  if(verbose){Rprintf("In operatingModel::survivors\n");}
  if ((indices_min.size() != 6) | (indices_max.size() != 6)){
    Rcpp::stop("In operatingModel survivors subsetter. Indices not of length 6\n");
  }
  if(verbose){Rprintf("About to get F\n");}
  FLQuantAD z_temp = get_f(biol_no, indices_min, indices_max) + biols(biol_no).m(indices_min, indices_max);
  if(verbose){Rprintf("Got F\n");}
  FLQuantAD survivors = biols(biol_no).n(indices_min, indices_max) * exp(-1.0 * z_temp);
  return survivors;
}

/*! \brief Project the Biols in the operatingModel by a single timestep
 *   Projects the Biols in the operatingModel by a single timestep.
 *   All abundances in the Biols are updated in the timestep based on the fishing effort and population abundance in the previous timestep.
 *   Calculation is based on the the Baranov equation: N2 = N1 * (exp -Z)
 *   This assumes that the instantaneous rate of fishing and natural mortalities are constant over the timestep and occur simultaneously.
 *   If a Biol is caught by multiple Catches, the fishing mortalities happen at the same time (and at the same time as the natural mortality) in the timestep.
 *   \param timestep The time step for the projection.
 */
void operatingModel::project_biols(const int timestep){
  bool verbose = false;
  if(verbose){Rprintf("In operatingModel::project_biols\n");}
  if (timestep < 2){
    Rcpp::stop("In operatingModel::project_biols. Uses effort in previous timestep so timestep must be at least 2.");
  }
  for (unsigned int biol_counter=1; biol_counter <= biols.get_nbiols(); ++biol_counter){
    if(verbose){Rprintf("Projecting biol: %i\n", biol_counter);}
    std::vector<unsigned int> biol_dim = biols(biol_counter).n().get_dim();
    unsigned int year = 1;
    unsigned int season = 1;
    timestep_to_year_season(timestep, biol_dim[3], year, season);
    // timestep checks
    if ((year > biol_dim[1]) | (season > biol_dim[3])){
      Rcpp::stop("In operatingModel::project_biols. Timestep outside of range");
    }
    unsigned int niter = get_niter();
    unsigned int area = 1;
    // Make indices for previous timestep
    unsigned int prev_year = 1;
    unsigned int prev_season = 1;
    timestep_to_year_season(timestep-1, biol_dim[3], prev_year, prev_season);
    std::vector<unsigned int> prev_indices_min{1, prev_year, 1, prev_season, area, 1};
    std::vector<unsigned int> prev_indices_max{biol_dim[0], prev_year, biol_dim[2], prev_season, area, niter};
    // Get abundance at end of preceding timestep
    if(verbose){Rprintf("About to get survivors\n");}
    FLQuantAD surv = survivors(biol_counter, prev_indices_min, prev_indices_max);
    FLQuantAD new_abundance = surv;
    if(verbose){Rprintf("Starting to loop over units\n");}
    for (unsigned int ucount = 1; ucount <= biol_dim[2]; ++ucount){
      bool recruiting_now = biols(biol_counter).does_recruitment_happen(ucount, year, season);
      // If recruiting shift survivor ages, fix plus group. Recruitment calculated later after updating abundances.
      if (recruiting_now){
        for (unsigned int icount = 1; icount <= niter; ++icount){
          for (unsigned int qcount = 2; qcount <= biol_dim[0]; ++qcount){
            new_abundance(qcount,1,ucount,1,area,icount) = surv(qcount-1,1,ucount,1,area,icount);
          }
          // plus group
          new_abundance(biol_dim[0], 1, ucount, 1, area, icount) = new_abundance(biol_dim[0], 1, ucount, 1, area, icount) + surv(biol_dim[0], 1, ucount, 1, 1, icount);
        }
      }
      // Update biol with survivors
      for (unsigned int icount = 1; icount <= niter; ++icount){
        for (unsigned int qcount = 1; qcount <= biol_dim[0]; ++qcount){
          biols(biol_counter).n(qcount, year, ucount, season, area, icount) = new_abundance(qcount, 1, ucount, 1, 1, icount);
        }
      }
      // Insert recruitment - can only do after abundance has been updated in timestep as SRP might be from this timestep
      if (recruiting_now){
        std::vector<adouble> rec = calc_rec(biol_counter, ucount, timestep);
        for (unsigned int icount = 1; icount <= niter; ++icount){
          biols(biol_counter).n(1, year, ucount, season, area, icount) = rec[icount-1];
        }
      }
      // If abundances are too small, set at minimum - avoid numerical issues in solver
      adouble min_abundance = 1e-6;
      for (unsigned int icount = 1; icount <= niter; ++icount){
        for (unsigned int qcount = 1; qcount <= biol_dim[0]; ++qcount){
          biols(biol_counter).n(qcount, year, ucount, season, area, icount) = CppAD::CondExpLt(biols(biol_counter).n(qcount, year, ucount, season, area, icount), min_abundance, min_abundance, biols(biol_counter).n(qcount, year, ucount, season, area, icount));
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
  // Number of iters for all catch_n and biol n must be the same as all derive from effort which has the number of iters
  unsigned int niter = get_niter();
  // Not yet set up for areas
  unsigned int area = 1;
  // Get the Total Z for every biol - order is order of biols in biols list (maybe different to FCB order)
  std::vector<FLQuantAD> total_z(biols.get_nbiols());
  // Fill it up with natural mortality to start with
  for (unsigned int biol_count=1; biol_count <= biols.get_nbiols(); ++biol_count){
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
  for (int FCB_counter=0; FCB_counter < FCB.nrow(); ++FCB_counter){
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
  for (unsigned int fishery_count=1; fishery_count <= fisheries.get_nfisheries(); ++fishery_count){
    for (unsigned int catch_count=1; catch_count <= fisheries(fishery_count).get_ncatches(); ++catch_count){
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
      auto biols_fished = ctrl.get_B(fishery_count, catch_count);
      for (unsigned int biol_count=0; biol_count < biols_fished.size(); ++biol_count){
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
 * \param effort_max The maximum total value of effort
 */
Rcpp::IntegerMatrix operatingModel::run(const double effort_mult_initial, std::vector<double> effort_max, const double indep_min, const double indep_max, const unsigned int nr_iters){
  //auto tstartrun = std::chrono::high_resolution_clock::now();
  bool verbose = false;
  if(verbose){Rprintf("\nIn run()\n");}
  // Housekeeping
  auto niter = get_niter(); // number of iters taken from effort of first fishery
  if(verbose){Rprintf("Number of iterations to solve - niter: %i\n", niter);}
  // Effort multiplier is the independent value. There is one independent values for each effort, i.e. for each fishery
  auto neffort = fisheries.get_nfisheries();
  if(verbose){Rprintf("Number of fisheries to solve effort for: %i\n", neffort);}
  // The target timesteps are contiguous.
  // Update the biology in the first target timestep.
  // This ensures that we have abundance numbers in the first timestep of the projection.
  // Is this always necessary? What if first target is biol in timestep y? Check this
  // However, this will overwrite existing abundances - do we want this?
  // Assumes that first target has a target number of 1
  // Assume that the first sim target of the first target is in the first timestep of the projection
  unsigned int first_target_year = ctrl.get_target_int_col(1,1, "year");
  unsigned int first_target_season = ctrl.get_target_int_col(1,1, "season");
  unsigned int min_target_timestep = 0;
  year_season_to_timestep(first_target_year, first_target_season, biols(1).n().get_nseason(), min_target_timestep);
  if(verbose){Rprintf("Min target timestep: %i\n", min_target_timestep);}
  if(verbose){Rprintf("Projecting biols for the first timestep to update abundances\n"); }
  project_biols(min_target_timestep);
  // Get maximum timestep of OM. If room, we update Biol in final timestep at the end
  if(verbose){Rprintf("Back from projecting biols\n");}
  std::vector<unsigned int> biol1_dim = biols(1).n().get_dim();
  unsigned int max_timestep = biol1_dim[1] * biol1_dim[3];
  // Each target is solved independently (but a target can be made up of multiple simultaneous targets)
  auto ntarget = ctrl.get_ntarget();
  if(verbose){Rprintf("Targets to solve: %i \n", ntarget);}
  // Place to store the codes from the solver routine. One code per target per iter. Ntarget x iter
  Rcpp::IntegerMatrix solver_codes(ntarget,niter);
  // Loop over targets and solve all simultaneous targets in that target set
  // e.g. With 2 fisheries with 2 efforts, we can set 2 catch targets to be solved at the same time
  // Indexing of targets starts at 1
  for (unsigned int target_count = 1; target_count <= ntarget; ++target_count){
    if(verbose){Rprintf("\nProcessing target: %i\n", target_count);}
    auto nsim_targets = ctrl.get_nsim_target(target_count);
    if(verbose){Rprintf("Number of simultaneous targets: %i\n", nsim_targets);}
    // Timestep in which we find effort is the same for all simultaneous targets in a target set
    // Get time step of first sim target and use this for all sim targets.
    // Get Y/S from control - convert to timestep
    unsigned int target_effort_timestep = 0;
    unsigned int target_effort_year = ctrl.get_target_int_col(target_count, 1, "year");
    unsigned int target_effort_season = ctrl.get_target_int_col(target_count, 1, "season");
    year_season_to_timestep(target_effort_year, target_effort_season, biols(1).n().get_nseason(), target_effort_timestep);
    // Get the target value based on control object and current value in the OM (if Max / Min)
    // This is not part of the operation sequence so is evaluated before we turn on the tape
    if(verbose){Rprintf("Getting desired target values from control object\n");}
    std::vector<double> target_value = get_target_value(target_count); // values of all sim targets for the target
    // Set up effort multipliers - do all efforts and iters at same time (keep timesteps, areas seperate)
    std::vector<adouble> effort_mult_ad(neffort * niter, effort_mult_initial);
    std::fill(effort_mult_ad.begin(), effort_mult_ad.end(), effort_mult_initial);

    // Turn tape on
    CppAD::Independent(effort_mult_ad);
    if(verbose){Rprintf("Turned on tape\n");}
    if(verbose){Rprintf("target_effort_year: %i\n", target_effort_year);}
    if(verbose){Rprintf("target_effort_season: %i\n", target_effort_season);}
    // Update fisheries.effort() with effort multiplier in the effort timestep (area and unit effectively ignored)
    if(verbose){Rprintf("Updating effort with multipler\n");}
    if(verbose){Rprintf("Effort before updating: %f\n", Value(fisheries(1).effort()(1, target_effort_year, 1, target_effort_season, 1, 1)));}
    for (unsigned int fisheries_count = 1; fisheries_count <= fisheries.get_nfisheries(); ++fisheries_count){
      for (unsigned int iter_count = 1; iter_count <= niter; ++ iter_count){
        fisheries(fisheries_count).effort()(1, target_effort_year, 1, target_effort_season, 1, iter_count) = 
          fisheries(fisheries_count).effort()(1, target_effort_year, 1, target_effort_season, 1, iter_count) * 
          effort_mult_ad[(fisheries_count - 1) * niter + iter_count - 1];
      }
    }
    if(verbose){Rprintf("Effort after updating : %f\n", Value(fisheries(1).effort()(1, target_effort_year, 1, target_effort_season, 1, 1)));}
    //auto tpreproject = std::chrono::high_resolution_clock::now();
    if(verbose){Rprintf("Projecting\n");}
    // Project fisheries in the target effort timestep
    // (landings and discards are functions of effort in the effort timestep)
    if(verbose){Rprintf("Projecting fisheries\n");}
    project_fisheries(target_effort_timestep); 
    // Project biology in the target effort timestep plus 1
    // (biology abundances are functions of effort in the previous timestep)
    // Only update if there is room
    if ((target_effort_timestep+1) <= max_timestep){
      if(verbose){Rprintf("Projecting biols on tape\n");}
      project_biols(target_effort_timestep+1); 
    }
    if(verbose){Rprintf("Back from projecting\n");}
    // Calc error
    if(verbose){Rprintf("Getting current state of operating model\n");}
    // Get current state of operating model
    std::vector<adouble> target_value_hat = get_target_value_hat(target_count); 
    if(verbose){
      Rprintf("target_value_hat [0]: %f\n", Value(target_value_hat[0]));
      //Rprintf("target_value_hat [1]: %f\n", Value(target_value_hat[1]));
    }
    // Check they are the same length? 
    if (target_value_hat.size() != target_value.size()){
      Rcpp::stop("In operatingModel run. target_value_hat and target_value are not the same size. Something has gone wrong.\n");
    }
    std::vector<adouble> error(target_value_hat.size());
    if(verbose){Rprintf("Calculating error\n");}
    std::transform(target_value.begin(), target_value.end(), target_value_hat.begin(), error.begin(),
        //[](double x, adouble y){return x - y;});
        [](double x, adouble y){return y - x;});
        //[](adouble x, adouble y){return (x - y) * (x - y);}); // squared error - not as effective
    if(verbose){
      Rprintf("target 1. target_value: %f target_value_hat: %f error: %f\n", target_value[0], Value(target_value_hat[0]), Value(error[0]));
      //Rprintf("target 2. target_value: %f target_value_hat: %f error: %f\n", target_value[1], Value(target_value_hat[1]), Value(error[1]));
    }
    //if(verbose){Rprintf("target 2. target_value: %f target_value_hat: %f error: %f\n", target_value[1], Value(target_value_hat[1]), Value(error[1]));}
    // Stop recording
    // auto tpretapeoff = std::chrono::high_resolution_clock::now();
    // Use of optimize takes a long time. But solver is slightly faster
    CppAD::ADFun<double> fun(effort_mult_ad, error);
    // fun.optimize();
    // Use Dependent? Makes no difference
    // CppAD::ADFun<double> fun;
    // fun.Dependent(effort_mult_ad, error);
    // fun.optimize();
    // auto taftertapeoff = std::chrono::high_resolution_clock::now();

    if(verbose){Rprintf("Turned off tape\n");}
    // Solve the target
    // double version of effort mult used in solver
    std::vector<double> effort_mult(neffort * niter, effort_mult_initial);
    std::fill(effort_mult.begin(), effort_mult.end(), effort_mult_initial);
    if(verbose){Rprintf("Solving\n");}
    //auto tpresolve = std::chrono::high_resolution_clock::now();
    std::vector<int> nr_out = newton_raphson(effort_mult, fun, niter, nsim_targets, indep_min, indep_max, nr_iters);
    if(verbose){Rprintf("Finished solving\n");}
    if(verbose){Rprintf("nr_out: %i\n", nr_out[0]);}
    //auto taftersolve = std::chrono::high_resolution_clock::now();

    // Check nr_out - if not all 1 then something has gone wrong - flag up warning
    // Each iter has a success code for all sim targets - put them into a matrix
    // Ntarget x iter
    for (unsigned int iter_count = 0; iter_count < niter; ++iter_count){
      solver_codes(target_count - 1, iter_count) = nr_out[iter_count];
    }
    if(verbose){Rprintf("effort_mult: %f\n", effort_mult[0]);}
    if(verbose){Rprintf("Updating effort with solved effort mult\n");}
    // Update effort in fisheries based on the solved effort multiplier
    for (unsigned int fisheries_count = 1; fisheries_count <= fisheries.get_nfisheries(); ++fisheries_count){
      for (unsigned int iter_count = 1; iter_count <= niter; ++ iter_count){
        fisheries(fisheries_count).effort()(1, target_effort_year, 1, target_effort_season, 1, iter_count) = 
           fisheries(fisheries_count).effort()(1, target_effort_year, 1, target_effort_season, 1, iter_count) * 
          effort_mult[(fisheries_count - 1) * niter + iter_count - 1] / effort_mult_initial;
      }
    }
    // *** Check if effort is > effort max. If too big, limit it. ****
    // effort_max must be same length as number of fisheries
    for (unsigned int fisheries_count = 1; fisheries_count <= fisheries.get_nfisheries(); ++fisheries_count){
      for (unsigned int iter_count = 1; iter_count <= niter; ++ iter_count){
        // Final effort, all iters
        adouble current_effort = fisheries(fisheries_count).effort()(1, target_effort_year, 1, target_effort_season, 1, iter_count);
        // Compare to this
        // Normal comparison should be OK as we have finished taping
        if (current_effort > effort_max[fisheries_count-1]){
          fisheries(fisheries_count).effort()(1, target_effort_year, 1, target_effort_season, 1, iter_count) = effort_max[fisheries_count-1];
        }
      }}
    // ***** end of new effort bit
    if(verbose){Rprintf("Final effort: %f\n", Value(fisheries(1).effort()(1, target_effort_year, 1, target_effort_season, 1, 1)));}
    if(verbose){Rprintf("Projecting again\n");}
    project_fisheries(target_effort_timestep); 
    // If space, update biols too
    if ((target_effort_timestep+1) <= max_timestep){
      project_biols(target_effort_timestep+1); 
    }
    //std::chrono::duration<double, std::milli> proj_time = tpretapeoff - tpreproject;
    //std::chrono::duration<double, std::milli> tape_time = taftertapeoff - tpretapeoff;
    //std::chrono::duration<double, std::milli> solv_time = taftersolve - tpresolve;
    //Rprintf("proj_time: %f \n", proj_time.count());
    //Rprintf("tape_time: %f \n", tape_time.count());
    //Rprintf("solv_time: %f \n", solv_time.count());
  }
  if(verbose){Rprintf("Leaving run\n\n");}
  //auto tendrun = std::chrono::high_resolution_clock::now();
  //std::chrono::duration<double, std::milli> run_time = tendrun - tstartrun;
  //Rprintf("run_time: %f \n", run_time.count());
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
 * \param indices_min The minimum range of the returned FLQuant. Length should be appropriate for the target type (e.g. 6 for Fbar, 5 for SSB).
 * \param indices_max The maximum range of the returned FLQuant. Length should be appropriate for the target type (e.g. 6 for Fbar, 5 for SSB).
 */
FLQuantAD operatingModel::eval_om(const fwdControlTargetType target_type, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) {
  bool verbose = false;
  if(verbose){Rprintf("\nInside eval_om\n");}
  if(verbose){Rprintf("Fishery: %i, Catch: %i, Biol: %i\n", fishery_no, catch_no, biol_no);}
  bool biol_na = Rcpp::IntegerVector::is_na(biol_no);
  bool catch_na = Rcpp::IntegerVector::is_na(catch_no);
  bool fishery_na = Rcpp::IntegerVector::is_na(fishery_no);

  // Checks
  // Must have at least one of FBC
  if (biol_na & fishery_na & catch_na){
    Rcpp::stop("In operatingModel::eval_om. No Fishery, Catch or Biol specified.\n");
  }
  // If we have a Catch, we must also have a Fishery
  if(!catch_na & fishery_na){
    Rcpp::stop("In operatingModel::eval_om. If you specify a Catch, you must also specify a Fishery.\n");
  }
  // Cannot have Fishery and Biol but no Catch
  if(!biol_na & !fishery_na & catch_na){
    Rcpp::stop("In operatingModel::eval_om. Only Fishery and Biol specified. Must also specify Catch.\n");
  }

  FLQuantAD out;
  switch(target_type){
    case target_revenue: {
      // Revenue is not age structured - sums over all ages
      if(verbose){Rprintf("target_revenue\n");}
      // Total revenue of a Fishery (no Biol allowed)
      if (!fishery_na & catch_na & biol_na){
        out = unit_sum(fisheries(fishery_no).revenue(indices_min, indices_max));
      }
      // Revenue from a Catch (no Biol allowed)
      else if (!fishery_na & !catch_na & biol_na){
        out = unit_sum(fisheries(fishery_no, catch_no).revenue(indices_min, indices_max));
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for revenue target but Fishery, Catch and Biol not specified correctly. Only Fishery (for total revenue) , or Fishery and Catch (for revenue of a Catch) allowed.\n");
      }
    break;
    }
    case target_effort: {
      if(verbose){Rprintf("target_effort\n");}
      // Effort only has one unit
      std::vector<unsigned int> effort_indices_min = indices_min;
      std::vector<unsigned int> effort_indices_max = indices_max;
      effort_indices_min[2] = 1;
      effort_indices_max[2] = 1;
      // Effort of fishery only
      if (!fishery_na & catch_na & biol_na){
        out = fisheries(fishery_no).effort(indices_min, indices_max);
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for effort target but Fishery, Catch and Biol not specified correctly. Only Fishery allowed.\n");
      }
    break;
    }
    case target_fbar: {
      if(verbose){Rprintf("target_fbar\n");}
      // Total Fbar on a biol
      if (!biol_na & fishery_na & catch_na){
        out = fbar(biol_no, indices_min, indices_max);
      }
      // Partial Fbar from a Fishery and Catch on a Biol
      else if (!biol_na & !fishery_na & !catch_na){
        out = fbar(fishery_no, catch_no, biol_no, indices_min, indices_max);
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for Fbar target but Fishery, Catch and Biol not specified correctly. Either specify only a Biol (for the total Fbar on a biol) or specify the Fishery, Catch and Biol (for the partial Fbar).\n");
      }
      break;
    }
    case target_catch: {
      if(verbose){Rprintf("target_catch\n");}
      // Calculate catch of a Biol
      if (!biol_na & fishery_na & catch_na){
        if(verbose){Rprintf("catch is total catch from biol %i\n", biol_no);}
        out =  unit_sum(catches(biol_no, indices_min, indices_max));
        if(verbose){Rprintf("Total catch iter 1: %f\n", Value(out(1,1,1,1,1,1)));}
      }
      // Catch taken from the Fishery and Catch objects
      else if (biol_na & !fishery_na & !catch_na){
        if(verbose){Rprintf("biol_no is NA, catch is from FLCatch %i in FLFishery %i\n", catch_no, fishery_no);}
        out = unit_sum(fisheries(fishery_no, catch_no).catches(indices_min, indices_max));
      }
      else if (!fishery_na & !catch_na & !biol_na) {
        Rcpp::stop("In operatingModel::eval_om. Asking for catch from a specific Fishery / Catch and Biol. It's a special case that is not yet implemented. Can you ask for total catch from just the Biol instead and set Fishery / Catch to NA?\n");
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for catch target but Fishery, Catch and Biol not specified correctly. You must only specify the Fishery and Catch (for the catch of a Catch) or the Biol (for the total catch from a Biol).\n");
      }
      break;
    }
    case target_landings: {
      if(verbose){Rprintf("target_landings\n");}
      // Calculate landings of a Biol
      if (!biol_na & fishery_na & catch_na){
        if(verbose){Rprintf("landings is total landings from biol %i\n", biol_no);}
        out =  unit_sum(landings(biol_no, indices_min, indices_max));
      }
      // Landings taken from the Fishery and Catch objects
      else if (biol_na & !fishery_na & !catch_na){
        if(verbose){Rprintf("Biol is NA, landings is from Catch %i in Fishery %i\n", catch_no, fishery_no);}
        out = unit_sum(fisheries(fishery_no, catch_no).landings(indices_min, indices_max));
      }
      else if (!fishery_na & !catch_na & !biol_na) {
        Rcpp::stop("In operatingModel::eval_om. Asking for landings from a specific Fishery / Catch and Biol. It's a special case that is not yet implemented. Can you ask for total landings from just the Biol instead and set Fishery / Catch to NA?\n");
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for landings target but Fishery, Catch and Biol not specified correctly. You must only specify the Fishery and Catch (for the landings of a Catch) or the Biol (for the total landings from a Biol).\n");
      }
      break;
    }
    case target_discards: {
      if(verbose){Rprintf("target_discards\n");}
      // Calculate discards of a Biol
      if (!biol_na & fishery_na & catch_na){
        if(verbose){Rprintf("discards is total discards from biol %i\n", biol_no);}
        out =  unit_sum(discards(biol_no, indices_min, indices_max));
      }
      // Landings taken from the Fishery and Catch objects
      else if (biol_na & !fishery_na & !catch_na){
        if(verbose){Rprintf("Biol is NA, discards is from Catch %i in Fishery %i\n", catch_no, fishery_no);}
        out = unit_sum(fisheries(fishery_no, catch_no).discards(indices_min, indices_max));
      }
      else if (!fishery_na & !catch_na & !biol_na) {
        Rcpp::stop("In operatingModel::eval_om. Asking for discards from a specific Fishery / Catch and Biol. It's a special case that is not yet implemented. Can you ask for total discards from just the Biol instead and set Fishery / Catch to NA?\n");
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for discards target but Fishery, Catch and Biol not specified correctly. You must only specify the Fishery and Catch (for the discards of a Catch) or the Biol (for the total discards from a Biol).\n");
      }
      break;
    }
    case target_srp: {
      // SRP - *at the time of spawning*
      if (!biol_na & fishery_na & catch_na){
        // fbs is FALSE if spwn before fishing, OR if spwn is NA
        bool fbs = fishing_before_spawn(biol_no, indices_min, indices_max); 
        if (!fbs){
          Rcpp::warning("In operatingModel eval_om, srp target. Either spawning happens before fishing (so fishing effort has no impact on SRP), or no spawning in timestep. Cannot solve. Stopping\n");
        }
        out = total_srp(biol_no, indices_min, indices_max); 
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for SRP target but Fishery, Catch and Biol not specified correctly. You must only specify the Biol (not a Fishery or a Catch).\n");
      }
      break;
     }
// Makes no sense to have SSB at start - cannot affect it with F in current time step
//    case target_ssb_start: {
//      if(verbose){Rprintf("target_ssb_start\n");}
//      if (!biol_na & fishery_na & catch_na){
//        out = unit_sum(ssb_start(biol_no, indices_min, indices_max)); 
//      }
//      else {
//        Rcpp::stop("In operatingModel::eval_om. Asking for SSB_start target but Fishery, Catch and Biol not specified correctly. You must only specify the Biol (not a Fishery or a Catch).\n");
//      }
//      break;
//    }
//    case target_biomass_start: {
//      if(verbose){Rprintf("target_biomass_start\n");}
//      if (!biol_na & fishery_na & catch_na){
//        out = unit_sum(biomass_start(biol_no, indices_min, indices_max)); 
//      }
//      else {
//        Rcpp::stop("In operatingModel::eval_om. Asking for biomass_start target but Fishery, Catch and Biol not specified correctly. You must only specify the Biol (not a Fishery or a Catch).\n");
//      }
//      break;
//    }
    case target_ssb_end: {
      if(verbose){Rprintf("target_ssb_end\n");}
      if (!biol_na & fishery_na & catch_na){
        out = unit_sum(ssb_end(biol_no, indices_min, indices_max)); 
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for SSB_end target but Fishery, Catch and Biol not specified correctly. You must only specify the Biol (not a Fishery or a Catch).\n");
      }
      break;
    }
    case target_biomass_end: {
      if(verbose){Rprintf("target_biomass_end\n");}
      if (!biol_na & fishery_na & catch_na){
        out = unit_sum(biomass_end(biol_no, indices_min, indices_max));
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for biomass_end target but Fishery, Catch and Biol not specified correctly. You must only specify the Biol (not a Fishery or a Catch).\n");
      }
      break;
    }
    case target_ssb_spawn: {
      if(verbose){Rprintf("target_ssb_spawn\n");}
      if (!biol_na & fishery_na & catch_na){
        // fbs is FALSE if spwn before fishing, OR if spwn is NA
        bool fbs = fishing_before_spawn(biol_no, indices_min, indices_max); 
        if (!fbs){
          Rcpp::warning("In operatingModel eval_om, ssb_spawn target. Either spawning happens before fishing (so fishing effort has no impact on SRP), or no spawning in timestep. Cannot solve.\n");
        }
        out = unit_sum(ssb_spawn(biol_no, indices_min, indices_max));
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for SSB_spawn target but Fishery, Catch and Biol not specified correctly. You must only specify the Biol (not a Fishery or a Catch).\n");
      }
      break;
    }
    case target_biomass_spawn: {
      if(verbose){Rprintf("target_biomass_spawn\n");}
      if (!biol_na & fishery_na & catch_na){
        // fbs is FALSE if spwn before fishing, OR if spwn is NA
        bool fbs = fishing_before_spawn(biol_no, indices_min, indices_max); 
        if (!fbs){
          Rcpp::warning("In operatingModel eval_om, biomass_spawn target. Either spawning happens before fishing (so fishing effort has no impact on SRP), or no spawning in timestep. Cannot solve.\n");
        }
        out = unit_sum(biomass_spawn(biol_no, indices_min, indices_max));
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for biomass_spawn target but Fishery, Catch and Biol not specified correctly. You must only specify the Biol (not a Fishery or a Catch).\n");
      }
      break;
    }
    case target_ssb_flash: {
      if(verbose){Rprintf("target_ssb_flash\n");}
      if (!biol_na & fishery_na & catch_na){
        out = unit_sum(ssb_flash(biol_no, indices_min, indices_max));
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for SSB_flash target but Fishery, Catch and Biol not specified correctly. You must only specify the Biol (not a Fishery or a Catch).\n");
      }
      break;
    }
    case target_biomass_flash: {
      if(verbose){Rprintf("target_biomass_flash\n");}
      if (!biol_na & fishery_na & catch_na){
        out = unit_sum(biomass_flash(biol_no, indices_min, indices_max));
      }
      else {
        Rcpp::stop("In operatingModel::eval_om. Asking for biomass_flash target but Fishery, Catch and Biol not specified correctly. You must only specify the Biol (not a Fishery or a Catch).\n");
      }
      break;
    }
    default:
      Rcpp::stop("target_type not found in switch statement - giving up\n");
      break;
  }
  if(verbose){Rprintf("Leaving eval_om\n\n");}
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
std::vector<adouble> operatingModel::get_target_value_hat(const int target_no) {
  auto nsim_target = ctrl.get_nsim_target(target_no);
  std::vector<adouble> value;
  for (unsigned int sim_target_count = 1; sim_target_count <= nsim_target; ++sim_target_count){
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
std::vector<adouble> operatingModel::get_target_value_hat(const int target_no, const int sim_target_no) {
  bool verbose = false;
  if(verbose){Rprintf("\nIn get_target_value_hat\n");}
  if(verbose){Rprintf("sim_target_no: %i\n", sim_target_no);}
  std::vector<unsigned int> indices_min(1,6);
  std::vector<unsigned int> indices_max(1,6);
  // A target may have multiple target components from multiple biols - e.g. joint TACs
  // Loop over ntarget_components (start at 0)
  // Get the indices range using get_target_hat_indices(indices_min, indices_max, target_no, sim_target_no, target_component, false);
  // Get the Fishery, Catch, Biol[target_component] no
  // store result of evalOM
  // Sum results of evalOM
  //auto fishery_no = ctrl.get_target_int_col(target_no, sim_target_no, "fishery");
  //auto catch_no = ctrl.get_target_int_col(target_no, sim_target_no, "catch");
  auto Fnos = ctrl.get_target_list_int_col(target_no, sim_target_no, "fishery");
  auto Cnos = ctrl.get_target_list_int_col(target_no, sim_target_no, "catch");
  auto Bnos = ctrl.get_target_list_int_col(target_no, sim_target_no, "biol");
  // maximum length of these three
  auto no_target_components = std::max(Fnos.size(), std::max(Bnos.size(), Cnos.size()));
  if(verbose){Rprintf("Fnos length: %i\n", Fnos.size());}
  if(verbose){Rprintf("Cnos length: %i\n", Cnos.size());}
  if(verbose){Rprintf("Bnos length: %i\n", Bnos.size());}
  if(verbose){Rprintf("no_target_components: %i\n", no_target_components);}
  auto niters = get_niter();
  FLQuantAD target_value(1,1,1,1,1,niters); // target values are not structured by age, time or unit - only by iter
  fwdControlTargetType target_type;
  for (long target_component=1; target_component <= no_target_components; ++target_component){ // long to get min to work...
    auto Bno = std::min(target_component, (long int) Bnos.size()) - 1; // target_component or max no of biols in that target
    auto Cno = std::min(target_component, (long int) Cnos.size()) - 1;
    auto Fno = std::min(target_component, (long int) Fnos.size()) - 1;
    if(verbose){Rprintf("Fno: %i\n", Fno);}
    if(verbose){Rprintf("Cno: %i\n", Cno);}
    if(verbose){Rprintf("Bno: %i\n", Bno);}
    //Rprintf("target_component: %i\n", target_component);
    // Indices of target
    get_target_hat_indices(indices_min, indices_max, target_no, sim_target_no, target_component, false);
    // Target type
    target_type = ctrl.get_target_type(target_no, sim_target_no, false);
    // Evaluate the OM
    target_value = target_value + eval_om(target_type, Fnos[Fno], Cnos[Cno], Bnos[Bno], indices_min, indices_max);
  }
  if(verbose){Rprintf("Absolute target: %f\n", Value(target_value(1,1,1,1,1,1)));}

  // Do we have a relative target? Only check year and season
  // No check to see if the control object makes sense is OK - handled elsewhere
  unsigned int rel_year = ctrl.get_target_int_col(target_no, sim_target_no, "relYear"); 
  unsigned int rel_season = ctrl.get_target_int_col(target_no, sim_target_no, "relSeason");
  bool rel_year_na = Rcpp::IntegerVector::is_na(rel_year);
  bool rel_season_na = Rcpp::IntegerVector::is_na(rel_season);
  // Quick check: if relative biol, catch or fishery, is not NA but the year and season are, something has gone wrong
  auto rel_Cnos = ctrl.get_target_list_int_col(target_no, sim_target_no, "relCatch");
  auto rel_Fnos = ctrl.get_target_list_int_col(target_no, sim_target_no, "relFishery");
  auto rel_Bnos = ctrl.get_target_list_int_col(target_no, sim_target_no, "relBiol");
  bool rel_biol_na = true;
  for (auto rel_biol : rel_Bnos){
     rel_biol_na = rel_biol_na & Rcpp::IntegerVector::is_na(rel_biol);
  }
  bool rel_catch_na = true;
  for (auto rel_catch : rel_Cnos){
     rel_catch_na = rel_catch_na & Rcpp::IntegerVector::is_na(rel_catch);
  }
  bool rel_fishery_na = true;
  for (auto rel_fishery : rel_Fnos){
     rel_fishery_na = rel_fishery_na & Rcpp::IntegerVector::is_na(rel_fishery);
  }
  if ((!rel_biol_na | !rel_catch_na | !rel_fishery_na) & (!(!rel_year_na & !rel_season_na))){
    Rcpp::stop("In operatingModel::get_target_value_hat. You have specifed a relative Fishery, Catch or Biol, but not relative Year and Season\n");
  }
  // If relative year, must also be relative season
  if (rel_year_na != rel_season_na){
    Rcpp::stop("In operatingModel::get_target_value_hat. If you specify a relative year you must also specify a relative season and vice versa.\n");

  }
  // Relative target given only by year and season - not F, C or B
  if (!rel_year_na | !rel_season_na){
    if(verbose){Rprintf("Relative target in control\n");}
    //auto rel_fishery_no = ctrl.get_target_int_col(target_no, sim_target_no, "relFishery");
    //auto rel_catch_no = ctrl.get_target_int_col(target_no, sim_target_no, "relCatch");
    //auto rel_Fnos = ctrl.get_target_list_int_col(target_no, sim_target_no, "relFishery");
    //auto rel_Cnos = ctrl.get_target_list_int_col(target_no, sim_target_no, "relCatch");
    //auto rel_Bnos = ctrl.get_target_list_int_col(target_no, sim_target_no, "relBiol");
    auto no_target_components = std::max(rel_Fnos.size(), std::max(rel_Bnos.size(), rel_Cnos.size()));
    FLQuantAD rel_target_value(1,1,1,1,1,niters); // target values are not structured by age, time or unit - only by iter
    for (long target_component=1; target_component <= no_target_components; ++target_component){
      auto Bno = std::min(target_component, (long int) rel_Bnos.size()) - 1; // see above
      auto Cno = std::min(target_component, (long int) rel_Cnos.size()) - 1;
      auto Fno = std::min(target_component, (long int) rel_Fnos.size()) - 1;
      // Indices of rel target
      get_target_hat_indices(indices_min, indices_max, target_no, sim_target_no, target_component, true);
      // Evaluate the OM
      rel_target_value = rel_target_value + eval_om(target_type, rel_Fnos[Fno], rel_Cnos[Cno], rel_Bnos[Bno], indices_min, indices_max);
    }
    target_value = target_value / rel_target_value;
    if(verbose){Rprintf("Relative to absolute target: %f\n", Value(rel_target_value(1,1,1,1,1,1)));}
    if(verbose){Rprintf("Relative target: %f\n", Value(target_value(1,1,1,1,1,1)));}
  }
  std::vector<adouble> value = target_value.get_data();
  if(verbose){Rprintf("Leaving get_target_value_hat\n\n");}
  return value;
} 
//@}

/*! \brief Get the indices of the desired target
 *
 * Gets the range of indices of the desired target.
 * Indices start at 1.
 * The indices have length 5 (year, unit, season, area, iter) unless minAge and maxAge are present in the control object.
 * In that case, the indices are length 6 with age structure added.
 * 
 * \param indices_min Vector of the minimum indices.
 * \param indices_max Vector of the maximum indices.
 * \param target_no References the target column in the control dataframe. Starts at 1.
 * \param sim_target_no References the target column in the control dataframe. Starts at 1.
 * \param target_component_no If we have multiple biols in a target (stored as a list), which element of that list are we dealing with. Indexing starts at 0.
 * \param relative Are we getting the relative target indices (so that the correct columns of control are accessed).
 */
void operatingModel::get_target_hat_indices(std::vector<unsigned int>& indices_min, std::vector<unsigned int>& indices_max, const int target_no, const int sim_target_no, const long target_component_no,  const bool relative) {
  // Get the key information
  unsigned int year, season, fishery_no, catch_no, biol_no, min_age, max_age;
  // Get and check FCB nos
  std::vector<unsigned int> FCB_nos;
  Rcpp::IntegerVector biol_nos, fishery_nos, catch_nos;
  // Get the year, season and age range, depending if we have a relative target or not
  if (relative){
    year = ctrl.get_target_int_col(target_no, sim_target_no, "relYear"); 
    season = ctrl.get_target_int_col(target_no, sim_target_no, "relSeason");
    min_age = ctrl.get_target_int_col(target_no, sim_target_no, "relMinAge");
    max_age = ctrl.get_target_int_col(target_no, sim_target_no, "relMaxAge");
    fishery_nos = ctrl.get_target_list_int_col(target_no, sim_target_no, "relFishery");
    catch_nos = ctrl.get_target_list_int_col(target_no, sim_target_no, "relCatch");
    biol_nos = ctrl.get_target_list_int_col(target_no, sim_target_no, "relBiol");
  }
  if (!relative){
    year = ctrl.get_target_int_col(target_no, sim_target_no, "year"); 
    season = ctrl.get_target_int_col(target_no, sim_target_no, "season");
    min_age = ctrl.get_target_int_col(target_no, sim_target_no, "minAge");
    max_age = ctrl.get_target_int_col(target_no, sim_target_no, "maxAge");
    fishery_nos = ctrl.get_target_list_int_col(target_no, sim_target_no, "fishery");
    catch_nos = ctrl.get_target_list_int_col(target_no, sim_target_no, "catch");
    biol_nos = ctrl.get_target_list_int_col(target_no, sim_target_no, "biol");
  }
  fishery_no = fishery_nos[std::min(target_component_no, (long int) fishery_nos.size()) - 1];
  catch_no = catch_nos[std::min(target_component_no, (long int) catch_nos.size()) - 1];
  biol_no = biol_nos[std::min(target_component_no, (long int) biol_nos.size()) - 1];
  // Are these NAs?
  bool year_na = Rcpp::IntegerVector::is_na(year);
  bool season_na = Rcpp::IntegerVector::is_na(season);
  bool catch_na = Rcpp::IntegerVector::is_na(catch_no);
  bool fishery_na = Rcpp::IntegerVector::is_na(fishery_no);
  bool biol_na = Rcpp::IntegerVector::is_na(biol_no);
  bool min_age_na = Rcpp::IntegerVector::is_na(min_age);
  bool max_age_na = Rcpp::IntegerVector::is_na(max_age);
  // Check if we have the required information
  // Do we have year and season of the target
  if (year_na | season_na){
    Rcpp::stop("In OM get_target_hat_indices. We need both Year and Season in control (including for relative targets if necessary).\n");
  }
  // Need to know unit range of target.
  // If just fishery, nunit = 1 (no slots in a fishery have units)
  unsigned int nunit = 1;
  // If biol not NA, choose unit from biol
  if (!biol_na){
    nunit = biols(biol_no).n().get_nunit();
  }
  // Else if catch not NA choose unit from catch
  else if (!catch_na){
    if(fishery_na){
      Rcpp::stop("In OM get_target_hat_indices. Trying to get unit range from Catch but Fishery is not specified.\n");
    }
    nunit = fisheries(fishery_no, catch_no).landings_n().get_nunit();
  }
  // Get information for building indices
  unsigned int min_year = year;
  unsigned int max_year = year; // May change if additional info in control object
  unsigned int min_season = season;
  unsigned int max_season = season; // May change if additional info in control object
  unsigned int niter = get_niter();

  // If we have min or max age, get age indices
  if (!min_age_na | !max_age_na){
    if (min_age_na | max_age_na){
      Rcpp::stop("In operatingModel::get_target_hat_indices. Must supply both minAge and maxAge, not just one of them.\n");
    }
    // Make initial indices
    indices_min = {1, min_year, 1, min_season, 1, 1};
    indices_max = {1, max_year, nunit, max_season, 1, niter};
    // Need age names - get from biol or catch (only they have age structure - fishery does not)
    std::vector<std::string> age_names;
    if(!biol_na){
      age_names = Rcpp::as<std::vector<std::string> >(biols(biol_no).n().get_dimnames()[0]);
    }
    else if (!catch_na){
      age_names = Rcpp::as<std::vector<std::string> >(fisheries(fishery_no, catch_no).landings_n().get_dimnames()[0]);
    }
    else {
      Rcpp::stop("In operatingModel::get_target_hat_indices. Unable to get age range as biol_no and catch_no are NA.\n");
    }
    // Use find() to match names - precheck in R that they exist
    std::vector<std::string>::iterator age_min_iterator = find(age_names.begin(), age_names.end(), std::to_string(min_age));
    if(age_min_iterator != age_names.end()){
      indices_min[0] = std::distance(age_names.begin(), age_min_iterator)+1;
    }
    else {
      Rcpp::stop("minAge in control not found in dimnames of target object\n");
    }
    std::vector<std::string>::iterator age_max_iterator = find(age_names.begin(), age_names.end(), std::to_string(max_age));
    if(age_max_iterator != age_names.end()){
      indices_max[0] = std::distance(age_names.begin(), age_max_iterator)+1;
    }
    else {
      Rcpp::stop("maxAge in control not found in dimnames of target object\n");
    }
  }
  else {
    // No min / max age then we assume no age structure in the target 
    indices_min = {min_year, 1, min_season, 1, 1};
    indices_max = {max_year, nunit, max_season, 1, niter};
  }
  return;
}

/*! \name Get the desired target values 
 */
//@{
/*! \brief Get the desired target values for all simultaneous targets in a target set.
 * If the target values are relative, the returned values are the proportions from the control object, not the actual values.
 * If values are based on max / min some calculation is required.
 * Returns a vector of the simultaneous target values of a target set. 
 * Needs to return adouble because if min / max target, the returned value will depend on current effort value.
 * \param target_no References the target column in the control dataframe. Starts at 1.
 */
std::vector<double> operatingModel::get_target_value(const int target_no) {
  auto nsim_target = ctrl.get_nsim_target(target_no);
  std::vector<double> value;
  for (unsigned int sim_target_count = 1; sim_target_count <= nsim_target; ++sim_target_count){
    auto sim_target_value = get_target_value(target_no, sim_target_count);
    value.insert(value.end(), sim_target_value.begin(), sim_target_value.end());
  }
  return value;
}
/*! \brief Get the desired target values for a single simultaneous target
 * If the target values are relative, the returned values are the proportions from the control object, not the actual values.
 * If values are based on max / min some calculation is required.
 * Returns a vector of values of the simultaneous target from the target set. 
 * Needs to return adouble because if min / max target, the returned value will depend on current effort value.
 * Note that targets cannot currently be specified by Unit. 
 * \param target_no References the target column in the control dataframe. Starts at 1.
 * \param sim_target_no References the target column in the control dataframe. Starts at 1.
 */
std::vector<double> operatingModel::get_target_value(const int target_no, const int sim_target_no) {
  // Check iters in control and OM
  // Iters in ctrl object may be less than iters in OM (1 or n)
  auto niter = get_niter(); // number of iters in OM taken from effort of first fishery
  auto ctrl_niter = ctrl.get_niter(); // iters in control object
  if ((niter != ctrl_niter) & (ctrl_niter != 1)){
    Rcpp::stop("In operatingModel::get_target_value. Iterations in control object must be 1 or equal to those in fishing effort.\n");
  }
  // Are we dealing with a min / max value?
  // If so we need to get the current state of the operating model to compare with
  std::vector<double> max_col = ctrl.get_target_value(target_no, sim_target_no, 3); 
  std::vector<double> min_col = ctrl.get_target_value(target_no, sim_target_no, 1); 
  // Just check first iteration of min and max values
  bool max_na = Rcpp::NumericVector::is_na(max_col[0]);
  bool min_na = Rcpp::NumericVector::is_na(min_col[0]);
  std::vector<double> value(niter);
  if (!max_na | !min_na){
    // Get current value in OM, force to double, to compare to the min and max
    std::vector<adouble> value_ad = get_target_value_hat(target_no, sim_target_no);
    std::transform(value_ad.begin(), value_ad.end(), value.begin(), [](adouble x) {return Value(x);});
    if(!max_na){
      // In case of only 1 ctrl iter we need to blow up niter
      if (niter > ctrl_niter){
        max_col.resize(niter);
        std::fill(max_col.begin(), max_col.end(), max_col[0]);
      }
      // No need to use CppAD Conditional as target value is evaluated before the operation sequence, i.e. not on the tape
      std::transform(value.begin(), value.end(), max_col.begin(), value.begin(), [](double x, double y) {return std::min(x, y);});
    }
    if(!min_na){
      // In case of only 1 ctrl iter we need to blow up niter
      if (niter > ctrl_niter){
        min_col.resize(niter);
        std::fill(min_col.begin(), min_col.end(), min_col[0]);
      }
      // No need to use CppAD Conditional as target value is evaluated before the operation sequence, i.e. not on the tape
      std::transform(value.begin(), value.end(), min_col.begin(), value.begin(), [](double x, double y) {return std::max(x, y);});
    }
  }
  // If not min or max, just get the values from the control object
  else {
    value = ctrl.get_target_value(target_no, sim_target_no, 2);
    if (niter > value.size()){
      value.resize(niter);
      std::fill(value.begin(), value.end(), value[0]);
    }
  }
  return value;
}
//@}

//---------------Target methods ----------------------------

/*! \name fbar
 * Calculate the mean instantaneous fishing mortality over the specified age range.
 * This collapses the unit dimension using the get_nunit_f() method which extracts the current catches in the OM.
 * It is therefore assumed that the catches in the OM for the specified indices have been updated and reflect the current effort and abundance.
 * If this is not the case, you will need to run project_fisheries() for the desired timesteps.
 * Note that the indices are not the names of the ages, but the positions, starting at 1
 * No check is made to see if the Fishery and Catch are fishing on the Biol.
 * Note that this method requires that ALL of the catch in the FLCatch must come from the Biol, i.e. this method will not correctly if the Fishery and Catch are fising on multiple biols.
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
  if((indices_min.size() != 6) | (indices_max.size() != 6)){
    Rcpp::stop("In operatingModel fbar FCB method. indices_min and max must be of length 6\n");
  }
  FLQuantAD fbar;
  // If a single unit is asked for, get F using get_f method
  if (indices_min[2] == indices_max[2]){
    FLQuantAD f = get_f(fishery_no, catch_no, biol_no, indices_min, indices_max); 
    fbar = quant_mean(f);
  }
  // Else we have to fanny about with multiple units
  else {
    FLQuantAD f = get_nunit_f(fishery_no, catch_no, biol_no, indices_min, indices_max); 
    fbar = quant_mean(f);
  }
  return fbar;
}

/*! \brief The total mean instantaneous fishing mortality over the specified age range of a single biol, subset over dimensions 2-6.
 * \param biol_no the position of the biol within the biols (starting at 1).
 * \param indices_min The minimum indices quant, year, unit etc (length 6)
 * \param indices_max The maximum indices quant, year, unit etc (length 6)
 */
FLQuantAD operatingModel::fbar(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
  if((indices_min.size() != 6) | (indices_max.size() != 6)){
    Rcpp::stop("In operatingModel fbar B method. indices_min and max must be of length 6\n");
  }
  FLQuantAD fbar;
  // If a single unit is asked for, get F using get_f method
  if (indices_min[2] == indices_max[2]){
    FLQuantAD f = get_f(biol_no, indices_min, indices_max); 
    fbar = quant_mean(f);
  }
  // Else we have to fanny about with multiple units
  else {
    FLQuantAD f = get_nunit_f(biol_no, indices_min, indices_max); 
    fbar = quant_mean(f);
  }
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
  if((indices_min.size() != 5) | (indices_max.size() != 5)){
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
     auto biols_fished = ctrl.get_B(FC(FC_counter, 0), FC(FC_counter, 1)); 
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
  if((indices_min.size() != 5) | (indices_max.size() != 5)){
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
     auto biols_fished = ctrl.get_B(FC(FC_counter, 0), FC(FC_counter, 1)); 
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
  if((indices_min.size() != 5) | (indices_max.size() != 5)){
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
  if((indices_min.size() != 6) | (indices_max.size() != 6)){
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
     auto biols_fished = ctrl.get_B(FC(FC_counter, 0), FC(FC_counter, 1)); 
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
  if((indices_min.size() != 6) | (indices_max.size() != 6)){
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
     auto biols_fished = ctrl.get_B(FC(FC_counter, 0), FC(FC_counter, 1)); 
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
 * \param indices_min minimum indices for subsetting (age - iter, integer vector of length 6)
 * \param indices_max maximum indices for subsetting (age - iter, integer vector of length 6)
 */
FLQuantAD operatingModel::catch_n(const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
  if((indices_min.size() != 6) | (indices_max.size() != 6)){
    Rcpp::stop("In operatingModel catch_n on a biol subset method. indices_min and max must be of length 6\n");
  }
  FLQuantAD total_catch_n = landings_n(biol_no, indices_min, indices_max) + discards_n(biol_no, indices_min, indices_max);
  return total_catch_n;
}

/*! \brief Subset the SSB at the start of the timestep
 *
 * Calculates the SSB at the start of the timestep, not at the time of spawning.
 * It is used for the target calculation.
 * SSB = sum (N * wt * mat)
 * Where N is the abundance at the start of the timestep.
 * Units are not collapsed.
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 5)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 5)
 */
FLQuantAD operatingModel::ssb_start(const int biol_no,  const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
  if((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel ssb_start. indices_min and max must be of length 6\n");
  }
  // Need bigger indices
  std::vector<unsigned int> dim = biols(biol_no).n().get_dim();
  std::vector<unsigned int> qindices_min = indices_min;
  qindices_min.insert(qindices_min.begin(), 1);
  std::vector<unsigned int> qindices_max = indices_max;
  qindices_max.insert(qindices_max.begin(), dim[0]);
  // SSB = n * wt * mat
  // Calc SSB - without unit sum - done in eval_om
  FLQuantAD ssb = quant_sum(biols(biol_no).n(qindices_min, qindices_max) * biols(biol_no).wt(qindices_min, qindices_max) * biols(biol_no).mat(qindices_min, qindices_max));
  return ssb;
}

/*! \brief Subset the biomass at the start of the timestep
 *
 * Calculates the biomass at the start of the timestep, not at the time of spawning.
 * It is used for the target calculation.
 * SSB = sum (N * wt)
 * Where N is the abundance at the start of the timestep.
 * Units are not collapsed.
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 5)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 5)
 */
FLQuantAD operatingModel::biomass_start(const int biol_no,  const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
  if((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel ssb_start. indices_min and max must be of length 6\n");
  }
  // Need bigger indices
  std::vector<unsigned int> dim = biols(biol_no).n().get_dim();
  std::vector<unsigned int> qindices_min = indices_min;
  qindices_min.insert(qindices_min.begin(), 1);
  std::vector<unsigned int> qindices_max = indices_max;
  qindices_max.insert(qindices_max.begin(), dim[0]);
  // biomass = n * wt
  // Calc biomass - without unit sum - done in eval_om
  FLQuantAD biomass = quant_sum(biols(biol_no).n(qindices_min, qindices_max) * biols(biol_no).wt(qindices_min, qindices_max));
  return biomass;
}

/*! \brief Subset the SSB at the end of the timestep
 *
 * Calculates the SSB at the end of the timestep, not at the time of spawning.
 * It is used for the target calculation.
 * SSB = sum (N * wt * mat)
 * Where N is the abundance at the end of the timestep.
 * Units not collapsed.
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 5)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 5)
 */
FLQuantAD operatingModel::ssb_end(const int biol_no,  const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
  if((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel ssb_end. indices_min and max must be of length 6\n");
  }
  // Need bigger indices
  std::vector<unsigned int> dim = biols(biol_no).n().get_dim();
  std::vector<unsigned int> qindices_min = indices_min;
  qindices_min.insert(qindices_min.begin(), 1);
  std::vector<unsigned int> qindices_max = indices_max;
  qindices_max.insert(qindices_max.begin(), dim[0]);
  FLQuantAD surv = survivors(biol_no, qindices_min, qindices_max);
  // SSB = survivors * wt * mat
  // Calc SSB - without unit sum - done in eval_om
  FLQuantAD ssb = quant_sum(surv * biols(biol_no).wt(qindices_min, qindices_max) * biols(biol_no).mat(qindices_min, qindices_max));
  return ssb;
}

/*! \brief Subset the biomass at the end of the timestep
 *
 * Calculates the biomass at the end of the timestep, not at the time of spawning.
 * It is used for the target calculation.
 * Biomass = sum (N * wt)
 * Where N is the abundance at the end of the timestep.
 * The indices range arguments must be for a single timestep only.
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 5)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 5)
 */
FLQuantAD operatingModel::biomass_end(const int biol_no,  const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const {
  if((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel biomass_end. indices_min and max must be of length 6\n");
  }
  // Check that only one timestep is asked for
  if ((indices_min[0] != indices_max[0]) | (indices_min[2] != indices_max[2])){
    Rcpp::stop("In operatingModel biomass_end. Year and season of indices_min and indices_max must be the same. Only one timestep allowed.\n");
  }
  std::vector<unsigned int> dim = biols(biol_no).n().get_dim();
  // Calc biomass - without unit sum - done in eval_om
  // Need bigger indices
  std::vector<unsigned int> qindices_min = indices_min;
  qindices_min.insert(qindices_min.begin(), 1);
  std::vector<unsigned int> qindices_max = indices_max;
  qindices_max.insert(qindices_max.begin(), dim[0]);
  // Biomass = survivors * wt
  FLQuantAD surv = survivors(biol_no, qindices_min, qindices_max);
  FLQuantAD biomass = quant_sum(surv * biols(biol_no).wt(qindices_min, qindices_max));
  return biomass;
}

/*! \brief Calculate the SSB at the time of spawning
 *
 * Calculates the SSB at the time of spawning.
 * sum N * wt * mat * exp(-Zspwn)
 * If any value of the spwn member in the indices range is NA an error is thrown (as it implies no spawning for that timestep / unit etc).
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 5)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 5)
 */
FLQuantAD operatingModel::ssb_spawn(const int biol_no,  const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
  // Check indices_min and indices_max are of length 5
  if ((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel::ssb_spawn subsetter. Indices not of length 5\n");
  }
  // Check for NA in spwn - if any value of spwn across indices range is NA - stop
  std::vector<unsigned int> qindices_min = indices_min;
  qindices_min.insert(qindices_min.begin(), 1);
  std::vector<unsigned int> qindices_max = indices_max;
  qindices_max.insert(qindices_max.begin(), 1);
  FLQuant spwn = biols(biol_no).spwn(qindices_min, qindices_max);
  for (const auto& it : spwn){
    if (Rcpp::NumericVector::is_na(it)){
      Rcpp::stop("In operatingModel::ssb_spawn subsetter. spwn member has NA value in the indices range. Implies no spawning so cannot calculate SSB at time of spawning. Stopping\n");
    }
  }
  // Add age range to input indices
  std::vector<unsigned int> dim = biols(biol_no).n().get_dim();
  qindices_max[0] = dim[0];
  FLQuantAD exp_z_pre_spwn = get_exp_z_pre_spwn(biol_no, qindices_min, qindices_max);
  // Get srp: N*mat*wt*exp(-Fprespwn - m*spwn) summed over age dimension
  FLQuantAD ssb = quant_sum(
    biols(biol_no).n(qindices_min, qindices_max) *
    biols(biol_no).wt(qindices_min, qindices_max) *
    biols(biol_no).mat(qindices_min, qindices_max) * exp_z_pre_spwn);
  return ssb;
}

/*! \brief Calculate the biomass at the time of spawning
 *
 * Calculates the SSB at the time of spawning.
 * sum N * wt * exp(-Zspwn)
 *
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 5)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 5)
 */
FLQuantAD operatingModel::biomass_spawn(const int biol_no,  const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) const{
  // Check indices_min and indices_max are of length 5
  if ((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel::biomass_spawn subsetter. Indices not of length 5\n");
  }
  // Check for NA in spwn - if any value of spwn across indices range is NA - stop
  std::vector<unsigned int> qindices_min = indices_min;
  qindices_min.insert(qindices_min.begin(), 1);
  std::vector<unsigned int> qindices_max = indices_max;
  qindices_max.insert(qindices_max.begin(), 1);
  FLQuant spwn = biols(biol_no).spwn(qindices_min, qindices_max);
  for (const auto& it : spwn){
    if (Rcpp::NumericVector::is_na(it)){
      Rcpp::stop("In operatingModel::biomass_spawn subsetter. spwn member has NA value in the indices range. Implies no spawning so cannot calculate SSB at time of spawning. Stopping\n");
    }
  }
  // Add age range to input indices
  std::vector<unsigned int> dim = biols(biol_no).n().get_dim();
  qindices_max[0] = dim[0];
  FLQuantAD exp_z_pre_spwn = get_exp_z_pre_spwn(biol_no, qindices_min, qindices_max);
  // Get srp: N*wt*exp(-Fprespwn - m*spwn) summed over age dimension
  FLQuantAD biomass = quant_sum(
    biols(biol_no).n(qindices_min, qindices_max) *
    biols(biol_no).wt(qindices_min, qindices_max) * exp_z_pre_spwn);
  return biomass;
}

/*! \brief Calculate the SSB old-school FLash style
 *
 * If fishing takes place after spawning it has no impact on SSB at time of spawning. Therefore, get SSB at time of spawning in next time step (if there is no spawning in the next timestep, an error is thrown).
 * This is really dodgy because if spawning in next timestep happens after fishing in next timestep, then SSB at spawning is a function of fishing effort in current AND next timestep.
 * It assumes that fishing and spawning take place at same time in each timestep. For a seasonal model, this is really dodgy.
 * If fishing takes place before spawning, get SSB at time of spawning in current time step.
 * 
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 5)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 5)
 */
FLQuantAD operatingModel::ssb_flash(const int biol_no,  const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) {
  // Check indices_min and indices_max are of length 5
  if ((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel::ssb_flash subsetter. Indices not of length 5\n");
  }
  // Check a single timestep
  if((indices_min[0] != indices_max[0]) | (indices_min[2] != indices_max[2])){
    Rcpp::stop("In operatingModel::ssb_flash subsetter. Only one timestep allowed in subsetting.\n");
  }
  FLQuantAD out;
  // If fishing affects SSB in current timestep, get SSB in current timestep at time of spawning
  if (fishing_before_spawn(biol_no, indices_min, indices_max)){
    out = ssb_spawn(biol_no, indices_min, indices_max);
  }
  // Otherwise get SSB at time of spawning in next timestep
  // What if there is no next time step? Do nothing
  else {
    // SSB at time of spawning in next timestep
    // Update population in next timestep
    auto biol_dim = biols(biol_no).n().get_dim(); 
    auto max_timestep = biol_dim[1] * biol_dim[3];
    // Set up indices for next timestep - really faffy!
    unsigned int timestep = 0;
    year_season_to_timestep(indices_min[0], indices_min[2], biol_dim[3], timestep);
    timestep++;
    if (timestep > max_timestep){
      Rcpp::warning("In operatingModel::ssb_flash. Unable to get SSB in following timestep as no room\n");
      out = ssb_spawn(biol_no, indices_min, indices_max);
    }
    else {
      project_biols(timestep);
      std::vector<unsigned int> next_indices_min = indices_min;
      std::vector<unsigned int> next_indices_max = indices_max;
      timestep_to_year_season(timestep, biol_dim[3], next_indices_min[0], next_indices_min[2]);
      next_indices_max[0] = next_indices_min[0];
      next_indices_max[2] = next_indices_min[2];
      // Check for NA in spwn in next time step - if any value of spwn across indices range is NA - stop
      std::vector<unsigned int> qindices_min = next_indices_min;
      qindices_min.insert(qindices_min.begin(), 1);
      std::vector<unsigned int> qindices_max = next_indices_max;
      qindices_max.insert(qindices_max.begin(), 1);
      FLQuant spwn = biols(biol_no).spwn(qindices_min, qindices_max);
      for (const auto& it : spwn){
        if (Rcpp::NumericVector::is_na(it)){
          Rcpp::stop("In operatingModel::ssb_flash. spwn member in following time step has NA value in the indices range. Implies no spawning so cannot calculate SSB at time of spawning. Stopping\n");
        }
      }
      out = ssb_spawn(biol_no, next_indices_min, next_indices_max);
    }
  }
  return out;
}

/*! \brief Calculate the biomass old-school FLash style
 *
 * If fishing takes place after spawning it has no impact on SSB at time of spawning. Therefore, get biomass at time of spawning in next time step (if there is no spawning in the next timestep, an error is thrown).
 * If fishing takes place before spawning, get biomass at time of spawning in current time step.
 * This is really dodgy because if spawning in next timestep happens after fishing in next timestep, then SSB at spawning is a function of fishing effort in current AND next timestep.
 * It assumes that fishing and spawning take place at same time in each timestep. For a seasonal model, this is really dodgy.
 * 
 * \param biol_no Position of the chosen biol in the biols list
 * \param indices_min minimum indices for subsetting (year - iter, integer vector of length 5)
 * \param indices_max maximum indices for subsetting (year - iter, integer vector of length 5)
 */
FLQuantAD operatingModel::biomass_flash(const int biol_no,  const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max) {
  // Check indices_min and indices_max are of length 5
  if ((indices_min.size() != 5) | (indices_max.size() != 5)){
    Rcpp::stop("In operatingModel::biomass_flash subsetter. Indices not of length 5\n");
  }
  // Check a single timestep
  if((indices_min[0] != indices_max[0]) | (indices_min[2] != indices_max[2])){
    Rcpp::stop("In operatingModel::biomass_flash subsetter. Only one timestep allowed in subsetting.\n");
  }
  FLQuantAD out;
  // If fishing affects SSB in current timestep, get SSB in current timestep at time of spawning
  if (fishing_before_spawn(biol_no, indices_min, indices_max)){
    out = biomass_spawn(biol_no, indices_min, indices_max);
  }
  // Otherwise get SSB at time of spawning in next timestep
  else {
    // SSB at time of spawning in next timestep
    // Update population in next timestep
    auto biol_dim = biols(biol_no).n().get_dim(); 
    auto max_timestep = biol_dim[1] * biol_dim[3];
    // Set up indices for next timestep - really faffy!
    unsigned int timestep = 0;
    year_season_to_timestep(indices_min[0], indices_min[2], biol_dim[3], timestep);
    timestep++;
    if (timestep > max_timestep){
      Rcpp::warning("In operatingModel::ssb_flash. Unable to get SSB in following timestep as no room\n");
      out = ssb_spawn(biol_no, indices_min, indices_max);
    }
    else {
      project_biols(timestep);
      std::vector<unsigned int> next_indices_min = indices_min;
      std::vector<unsigned int> next_indices_max = indices_max;
      timestep_to_year_season(timestep, biol_dim[3], next_indices_min[0], next_indices_min[2]);
      next_indices_max[0] = next_indices_min[0];
      next_indices_max[2] = next_indices_min[2];
      // Check for NA in spwn in next time step - if any value of spwn across indices range is NA - stop
      std::vector<unsigned int> qindices_min = next_indices_min;
      qindices_min.insert(qindices_min.begin(), 1);
      std::vector<unsigned int> qindices_max = next_indices_max;
      qindices_max.insert(qindices_max.begin(), 1);
      FLQuant spwn = biols(biol_no).spwn(qindices_min, qindices_max);
      for (const auto& it : spwn){
        if (Rcpp::NumericVector::is_na(it)){
          Rcpp::stop("In operatingModel::biomass_flash. spwn member in following time step has NA value in the indices range. Implies no spawning so cannot calculate SSB at time of spawning. Stopping\n");
        }
      }
      out = biomass_spawn(biol_no, next_indices_min, next_indices_max);
    }
  }
  return out;
}

//-------------------------------------------------
//
//
////// Define some pointers to function 
////typedef std::vector<adouble> (*funcPtrAD)(const std::vector<adouble>& x);
////typedef std::vector<double> (*funcPtr)(const std::vector<double>& x);
////
////
////// Returning a whole ADFun might be expensive - return as pointer?
////CppAD::ADFun<double> tape_my_func(std::vector<double>& xin, funcPtrAD fun){
////  // How to easily make an adouble vector?
////  std::vector<adouble> x(xin.size());
////  std::copy(xin.begin(), xin.end(), x.begin());
////  // Tape on
////  CppAD::Independent(x);
////  std::vector<adouble> y = fun(x);
////  CppAD::ADFun<double> f(x, y);
////  return f;
////}
////
////// [[Rcpp::export]]
////Rcpp::List solve_my_func(std::vector<double> xin, Rcpp::XPtr<funcPtrAD> xptr){
////  funcPtrAD func = *xptr; // the typedef
////  CppAD::ADFun<double> fun = tape_my_func(xin, func);
////  std::vector<int> success = newton_raphson(xin, fun, 1, 2, -1e9, 1e9, 50, 1e-12);
////  return Rcpp::List::create(Rcpp::Named("indep") = xin,
////                Rcpp::Named("success") = success);
////}
////


//'@title Call the CPP operatingModel run method
//'@description Call the CPP operatingModel run method
//'@name operatingModelRun
//
//' Used to run the projections.
//' Internal use only.
//'@param flfs FLFisheries.
//'@param biols List of the Biol bits.
//'@param ctrl fwdControl.
//'@param effort_mult_initial Initial effort multiplier.
//'@param effort_max Maximum yearly rate of change in effort for each fishery.
//'@param indep_min Minimum independent solver value.
//'@param indep_max Maximum independent solver value.
//'@param nr_iters Maximum number of iterations for solver.
//'@rdname operatingModelRun
// [[Rcpp::export]]
Rcpp::List operatingModelRun(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, std::vector<double> effort_max, const double effort_mult_initial, const double indep_min, const double indep_max, const int nr_iters = 50){
  //auto tstartrun = std::chrono::high_resolution_clock::now();
  operatingModel om(flfs, biols, ctrl);
  Rcpp::IntegerMatrix solver_codes = om.run(effort_mult_initial, effort_max, indep_min, indep_max, nr_iters);
  //auto tendrun = std::chrono::high_resolution_clock::now();
  //std::chrono::duration<double, std::milli> run_time = tendrun - tstartrun;
  //Rprintf("OM run_time: %f \n", run_time.count());
	return Rcpp::List::create(Rcpp::Named("om", om),
    Rcpp::Named("solver_codes",solver_codes));
}
