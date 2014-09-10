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
operatingModel test_operatingModel_full_constructor(FLFisheries flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7 f, const FLQuant7 f_spwn, const fwdControl ctrl){
    fwdBiol biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    return om;
}

/*---------- Member updaters --------------- */

// [[Rcpp::export]]
operatingModel test_operatingModel_load_ad_members_year_season(FLFisheries flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7 f, const FLQuant7 f_spwn, const fwdControl ctrl, const unsigned int year, const unsigned int season){
    fwdBiol biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    om.load_ad_members(year,season);
    return om;
}


// [[Rcpp::export]]
operatingModel test_operatingModel_load_ad_members_timestep(FLFisheries flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7 f, const FLQuant7 f_spwn, const fwdControl ctrl, const int timestep){
    fwdBiol biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    om.load_ad_members(timestep);
    return om;
}

// [[Rcpp::export]]
operatingModel test_operatingModel_update_from_ad_members_year_season(FLFisheries flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7 f, const FLQuant7 f_spwn, const fwdControl ctrl, const unsigned int year, const unsigned int season){
    fwdBiol biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    om.update_from_ad_members(year,season);
    return om;
}

// [[Rcpp::export]]
operatingModel test_operatingModel_update_from_ad_members_timestep(FLFisheries flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7 f, const FLQuant7 f_spwn, const fwdControl ctrl, const int timestep){
    fwdBiol biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    om.update_from_ad_members(timestep);
    return om;
}

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
    return out.value();
}

// [[Rcpp::export]]
double test_operatingModel_SSB_single_iter_year_season(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const int year, const int unit, const int season, const int area, const int iter, const fwdControl ctrl){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    const int biol_no = 1;
    adouble out = om.ssb(year, unit, season, area, iter, biol_no);
    return out.value();
}

// [[Rcpp::export]]
operatingModel test_operating_model_project(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const int timestep, const fwdControl ctrl){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    om.project_timestep(timestep, 1, ctrl.get_niter());
    return om;
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
    FLQuantAD ssb_out = om.ssb(biol_no);
    FLQuantAD biomass_out = om.biomass(biol_no);

	return Rcpp::List::create(Rcpp::Named("fbar_catch", fbar_catch),
                            Rcpp::Named("fbar",fbar),
                            Rcpp::Named("catches_catch",catches_catch_out),
                            Rcpp::Named("catches",catches_out),
                            Rcpp::Named("biomass",biomass_out),
                            Rcpp::Named("ssb",ssb_out));
}


// [[Rcpp::export]]
int test_operatingModel_get_target_fmult_timestep(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const fwdControl ctrl, const int target_no){
    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
    operatingModel om(flfs, biol, f, f_spwn, ctrl);
    return om.get_target_fmult_timestep(target_no);
}

//// Evaluate by target no
//// [[Rcpp::export]]
//std::vector<double> test_operatingModel_eval_target(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const fwdControl ctrl, const int target_no, const int min_iter, const int max_iter){
//    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
//    operatingModel om(flfs, biol, f, f_spwn, ctrl);
//    std::vector<adouble> out_ad = om.eval_target(target_no, min_iter, max_iter);
//    std::vector<double> out(out_ad.size(), 0.0);
//    for (int i=0; i<out.size(); ++i){
//        out[i] = out_ad[i].value();
//    }
//    return out;
//}



//// Evaluate by target type
//// [[Rcpp::export]]
//std::vector<double> test_operatingModel_eval_target2(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const fwdControl ctrl, const int target_no){
//
//    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
//    operatingModel om(flfs, biol, f, f_spwn, ctrl);
//    const int year = ctrl.get_target_year(target_no); 
//    const int season = ctrl.get_target_season(target_no);
//    const int unit = 1;
//    const int area = 1;
//    const int biol_no = 1;
//    std::vector<adouble> out_ad = om.eval_target(target_no, year, unit, season, area);
//    std::vector<double> out(out_ad.size(), 0.0);
//    for (int i=0; i<out.size(); ++i){
//        out[i] = out_ad[i].value();
//    }
//    return out;
//}

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
*/
