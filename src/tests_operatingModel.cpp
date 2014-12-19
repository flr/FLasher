/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/operating_model.h"

/* timestep convertors */

// [[Rcpp::export]]
int test_year_season_to_timestep_FLQuant_double(FLQuant flq, const int year, const int season){
    int timestep = 0;
    year_season_to_timestep(year, season, flq, timestep);
    return timestep;
}

// [[Rcpp::export]]
int test_year_season_to_timestep_FLQuant_adouble(FLQuantAD flqad, const int year, const int season){
    int timestep = 0;
    year_season_to_timestep(year, season, flqad, timestep);
    return timestep;
}

// [[Rcpp::export]]
int test_year_season_to_timestep(FLQuant flq, const int year, const int season){
    int timestep = 0;
    year_season_to_timestep(year, season, flq.get_nseason(), timestep);
    return timestep;
}

// [[Rcpp::export]]
Rcpp::IntegerVector test_timestep_to_year_season_FLQuant_double(FLQuant flq, const int timestep){
    int year = 0;
    int season = 0;
    timestep_to_year_season(timestep, flq, year, season);
    Rcpp::IntegerVector out(2);
    out[0] = year;
    out[1] = season;
    return out;
}

// [[Rcpp::export]]
Rcpp::IntegerVector test_timestep_to_year_season_FLQuant_adouble(FLQuantAD flqad, const int timestep){
    int year = 0;
    int season = 0;
    timestep_to_year_season(timestep, flqad, year, season);
    Rcpp::IntegerVector out(2);
    out[0] = year;
    out[1] = season;
    return out;
}

// [[Rcpp::export]]
Rcpp::IntegerVector test_timestep_to_year_season(FLQuant flq, const int timestep){
    int year = 0;
    int season = 0;
    timestep_to_year_season(timestep, flq.get_nseason(), year, season);
    Rcpp::IntegerVector out(2);
    out[0] = year;
    out[1] = season;
    return out;
}

/* -------------- constructors and wrappers ----------------- */

// [[Rcpp::export]]
void test_operatingModel_empty_constructor(){
    operatingModel om;
    return;
}

// [[Rcpp::export]]
operatingModel test_operatingModel_full_constructor(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl){
    // Make the fwdBiols
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om;
}

/*----------- F methods --------------*/


// f_spwn()

// catch_q()
// [[Rcpp::export]]
double test_operatingModel_catch_q_adouble(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    adouble qad = om.catch_q(fishery_no, catch_no, biol_no, indices[0], indices[1], indices[2], indices[3], indices[4]); 
    double q = Value(qad);
    return q;
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_catch_q_FLQuantAD(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD qad = om.catch_q(fishery_no, catch_no, biol_no);
    return qad;
}

// f()
// [[Rcpp::export]]
FLQuantAD test_operatingModel_F_FCB(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no){
    Rprintf("Making biols\n");
    fwdBiolsAD biols(flbs_list_sexp);
    Rprintf("Making om\n");
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD total_f = om.get_f(fishery_no, catch_no, biol_no);
    return total_f;
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_F_B(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD total_f = om.total_f(biol_no);
    return total_f;
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_partial_f(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD pf = om.partial_f(fishery_no, catch_no, biol_no);
    Rprintf("Returning pf\n");
    return pf;
}

// [[Rcpp::export]]
operatingModel test_operatingModel_project_timestep(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int timestep){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    om.project_timestep(timestep);
    return om;
}

/*----------- Project timestep --------------*/

/*
// [[Rcpp::export]]
operatingModel test_operatingModel_project_timestep(const FLFisheriesAD fisheries, SEXP FLBiolSEXP, const std::string srr_model_name, const FLQuant srr_params, const int srr_timelag, const FLQuant srr_residuals, const bool srr_residuals_mult, FLQuant7AD f, FLQuant7 f_spwn, fwdControl ctrl, const std::vector<int> timesteps){
    // Make the fwdBiol from the FLBiol and SRR bits
    fwdBiolAD biol(FLBiolSEXP, srr_model_name, srr_params, srr_timelag, srr_residuals, TRUE); 
    // Make the OM
    operatingModel om(fisheries, biol, f, f_spwn, ctrl);
    //om.load_ad_members(timestep);
    for (int timestep = timesteps[0]; timestep <= timesteps[timesteps.size()-1]; ++timestep){
        om.project_timestep(timestep);
    }

    return om;

}
*/

/*----------- SSB calculations--------------*/

/*
// [[Rcpp::export]]
FLQuantAD test_operatingModel_SSB_FLQ(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const fwdControl ctrl){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    const int biol_no = 1;
    return om.ssb(biol_no);
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_SSB_iters(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const int timestep, const int unit, const int area, const fwdControl ctrl){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    const int biol_no = 1;
    FLQuantAD out = om.ssb(timestep, unit, area, biol_no);
    return out;
}

// [[Rcpp::export]]
double test_operatingModel_SSB_single_iter(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const int timestep, const int unit, const int area, const int iter, const fwdControl ctrl){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    const int biol_no = 1;
    adouble out = om.ssb(timestep, unit, area, iter, biol_no);
    return Value(out);
}

// [[Rcpp::export]]
double test_operatingModel_SSB_single_iter_year_season(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const int year, const int unit, const int season, const int area, const int iter, const fwdControl ctrl){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    const int biol_no = 1;
    adouble out = om.ssb(year, unit, season, area, iter, biol_no);
    return Value(out);
}


// [[Rcpp::export]]
Rcpp::IntegerVector test_operating_model_get_target_age_range_indices(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const fwdControl ctrl, const int target_no){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    const int biol_no = 1;
    Rcpp::IntegerVector age_range_indices = om.get_target_age_range_indices(target_no, biol_no);
    return age_range_indices;
}




// [[Rcpp::export]]
Rcpp::List test_operating_model_targets(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const fwdControl ctrl, const int fishery_no, const int catch_no, const int target_no){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    const int biol_no = 1;
    Rcpp::IntegerVector age_range_indices = om.get_target_age_range_indices(target_no, biol_no);
    FLQuantAD fbar_catch = om.fbar(age_range_indices, fishery_no, catch_no, biol_no);
    FLQuantAD fbar = om.fbar(age_range_indices, biol_no);
    FLQuantAD catches_catch_out = om.catches(fishery_no, catch_no, 1);
    FLQuantAD catches_out = om.catches(1);
    FLQuantAD landings_catch_out = om.landings(fishery_no, catch_no, 1);
    FLQuantAD landings_out = om.landings(1);
    FLQuantAD discards_catch_out = om.discards(fishery_no, catch_no, 1);
    FLQuantAD discards_out = om.discards(1);
    FLQuantAD ssb_out = om.ssb(biol_no);
    FLQuantAD biomass_out = om.biomass(biol_no);

	return Rcpp::List::create(Rcpp::Named("fbar_catch", fbar_catch),
                            Rcpp::Named("fbar",fbar),
                            Rcpp::Named("catches_catch",catches_catch_out),
                            Rcpp::Named("catches",catches_out),
                            Rcpp::Named("landings_catch",landings_catch_out),
                            Rcpp::Named("landings",landings_out),
                            Rcpp::Named("discards_catch",discards_catch_out),
                            Rcpp::Named("discards",discards_out),
                            Rcpp::Named("biomass",biomass_out),
                            Rcpp::Named("ssb",ssb_out));
}

// [[Rcpp::export]]
int test_operatingModel_get_target_fmult_timestep(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const fwdControl ctrl, const int target_no){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    return om.get_target_fmult_timestep(target_no);
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_eval_target(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const fwdControl ctrl, const int target_no){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    FLQuantAD out = om.eval_target(target_no);
    return out;
}

// [[Rcpp::export]]
std::vector<double> test_operatingModel_calc_target_value(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const fwdControl ctrl, const int target_no){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    // Pull out values
    std::vector<double> out = om.calc_target_value(target_no);
    return out;
}



// Assumes the targets are already ordered by time
// [[Rcpp::export]]
operatingModel test_operatingModel_run(const FLFisheriesAD fisheries, SEXP FLBiolSEXP, const std::string srr_model_name, const FLQuant srr_params, const FLQuant srr_residuals, const bool srr_residuals_mult, const int srr_timelag, FLQuant7AD f, FLQuant7 f_spwn, fwdControl ctrl){

    // Make the fwdBiol from the FLBiol and SRR bits
    fwdBiolAD biol(FLBiolSEXP, srr_model_name, srr_params, srr_timelag, srr_residuals, srr_residuals_mult); 
    // Make the OM
    operatingModel om(fisheries, biol, f, f_spwn, ctrl);

    om.run();

    return om;

}
*/
