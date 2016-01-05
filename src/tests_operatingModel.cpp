/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

// For timing functions
#include <time.h>
#include "../../inst/include/operating_model.h"

/* timestep convertors */

// [[Rcpp::export]]
unsigned int test_year_season_to_timestep_FLQuant_double(FLQuant flq, const unsigned int year, const unsigned int season){
    unsigned int timestep = 0;
    year_season_to_timestep(year, season, flq, timestep);
    return timestep;
}

// [[Rcpp::export]]
unsigned int test_year_season_to_timestep_FLQuant_adouble(FLQuantAD flqad, const unsigned int year, const unsigned int season){
    unsigned int timestep = 0;
    year_season_to_timestep(year, season, flqad, timestep);
    return timestep;
}

// [[Rcpp::export]]
unsigned int test_year_season_to_timestep(FLQuant flq, const int unsigned year, const int unsigned season){
    unsigned int timestep = 0;
    year_season_to_timestep(year, season, flq.get_nseason(), timestep);
    return timestep;
}

// [[Rcpp::export]]
Rcpp::IntegerVector test_timestep_to_year_season_FLQuant_double(FLQuant flq, const unsigned int timestep){
    unsigned int year = 0;
    unsigned int season = 0;
    timestep_to_year_season(timestep, flq, year, season);
    Rcpp::IntegerVector out(2);
    out[0] = year;
    out[1] = season;
    return out;
}

// [[Rcpp::export]]
Rcpp::IntegerVector test_timestep_to_year_season_FLQuant_adouble(FLQuantAD flqad, const unsigned int timestep){
    unsigned int year = 0;
    unsigned int season = 0;
    timestep_to_year_season(timestep, flqad, year, season);
    Rcpp::IntegerVector out(2);
    out[0] = year;
    out[1] = season;
    return out;
}

// [[Rcpp::export]]
Rcpp::IntegerVector test_timestep_to_year_season(FLQuant flq, const unsigned int timestep){
    unsigned int year = 0;
    unsigned int season = 0;
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
operatingModel test_operatingModel_full_constructor(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl){
    operatingModel om(flfs, biols, ctrl);
    return om;
}

/*----------- House keeping methods */

// [[Rcpp::export]]
unsigned int test_operatingModel_get_niter(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl){
    operatingModel om(flfs, biols, ctrl);
    return om.get_niter();
}

/*----------- SRP calculations--------------*/

// [[Rcpp::export]]
FLQuantAD test_operatingModel_SRP_FLQ_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.srp(biol_no, indices_min, indices_max);
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_f_prop_spwn_FLQ_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int fishery_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.f_prop_spwn(fishery_no, biol_no, indices_min, indices_max);
}

/*----------- catch.q, F and Z methods --------------*/

//// catch_q()
//// [[Rcpp::export]]
//double test_operatingModel_catch_q_adouble(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    adouble qad = om.catch_q(fishery_no, catch_no, biol_no, indices[0], indices[1], indices[2], indices[3], indices[4]); 
//    double q = Value(qad);
//    return q;
//}
//
//
////// [[Rcpp::export]]
////FLQuantAD test_operatingModel_catch_q_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
////    fwdBiolsAD biols(flbs_list_sexp);
////    operatingModel om(flfs, biols, ctrl);
////    FLQuantAD qad = om.catch_q(fishery_no, catch_no, biol_no, indices_min, indices_max); 
////    return qad;
////}
//
//// [[Rcpp::export]]
//FLQuantAD test_operatingModel_catch_q_FLQuantAD(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    FLQuantAD qad = om.catch_q(fishery_no, catch_no, biol_no);
//    return qad;
//}

// get_f()
// No check is made if FC catches B
// [[Rcpp::export]]
FLQuantAD test_operatingModel_get_f_FCB(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD f = om.get_f(fishery_no, catch_no, biol_no);
    return f;
}

// get_f() subset
// No check is made if FC catches B
// [[Rcpp::export]]
FLQuantAD test_operatingModel_get_f_FCB_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, std::vector<unsigned int> indices_min, std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD f = om.get_f(fishery_no, catch_no, biol_no, indices_min, indices_max);
    return f;
}

// Total F on a biol
// [[Rcpp::export]]
FLQuantAD test_operatingModel_get_f_B_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min,  const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD total_f = om.get_f(biol_no, indices_min, indices_max);
    return total_f;
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_get_f_B(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD total_f = om.get_f(biol_no);
    return total_f;
}

// [[Rcpp::export]]
operatingModel test_operatingModel_project_biols(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int timestep){
    operatingModel om(flfs, biols, ctrl);
    om.project_biols(timestep);
    return om;
}

// [[Rcpp::export]]
operatingModel test_operatingModel_project_fisheries(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int timestep){
    operatingModel om(flfs, biols, ctrl);
    om.project_fisheries(timestep);
    return om;
}

// [[Rcpp::export]]
operatingModel test_operatingModel_run(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const double indep_min, const double indep_max){
    operatingModel om(flfs, biols, ctrl);
    om.run(indep_min, indep_max);
    return om;
}


//// [[Rcpp::export]]
//FLQuantAD test_operatingModel_partial_f(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    FLQuantAD pf = om.partial_f(fishery_no, catch_no, biol_no);
//    return pf;
//}
//
//// [[Rcpp::export]]
//FLQuantAD test_operatingModel_partial_f_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    FLQuantAD pf = om.partial_f(fishery_no, catch_no, biol_no, indices_min, indices_max);
//    return pf;
//}
//
//// [[Rcpp::export]]
//FLQuantAD test_operatingModel_partial_fbar_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    FLQuantAD pf = om.fbar(fishery_no, catch_no, biol_no, indices_min, indices_max);
//    return pf;
//}
//
//// [[Rcpp::export]]
//FLQuantAD test_operatingModel_fbar_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    FLQuantAD pf = om.fbar(biol_no, indices_min, indices_max);
//    return pf;
//}
//
//
///*
//// [[Rcpp::export]]
//operatingModel test_operatingModel_project_timestep(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int timestep){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    om.project_timestep(timestep);
//    return om;
//}
//
//// [[Rcpp::export]]
//operatingModel test_operatingModel_project_biols_then_fisheries(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int timestep){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    om.project_biols(timestep);
//    om.project_fisheries(timestep);
//    return om;
//}
//*/
//
//
//
//// [[Rcpp::export]]
//FLQuantAD test_operatingModel_Z(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    FLQuantAD z = om.z(biol_no);
//    return z;
//}
//
//
///*----------- Project timestep --------------*/
//
///*
//// [[Rcpp::export]]
//operatingModel test_operatingModel_project(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int timestep){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    om.project_timestep(timestep);
//    return om;
//}
//*/
//
/*----------- target calculations--------------*/

// [[Rcpp::export]]
FLQuantAD test_operatingModel_eval_om(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const std::string strquantity, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    fwdControlTargetType quantity = ctrl.get_target_type(strquantity);
    FLQuantAD out = om.eval_om(quantity, fishery_no, catch_no, biol_no, indices_min, indices_max);
    return out;
}

// [[Rcpp::export]]
std::vector<double> test_operatingModel_get_target_value_hat(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int target_no, const int sim_target_no){
    operatingModel om(flfs, biols, ctrl);
    std::vector<adouble> out_ad = om.get_target_value_hat(target_no, sim_target_no);
    std::vector<double> out(out_ad.size());
    std::transform(out_ad.begin(), out_ad.end(), out.begin(), [](adouble x) {return Value(x);});
    return out;
}

// [[Rcpp::export]]
std::vector<double> test_operatingModel_get_target_value_hat2(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int target_no){
    operatingModel om(flfs, biols, ctrl);
    std::vector<adouble> out_ad = om.get_target_value_hat(target_no);
    std::vector<double> out(out_ad.size());
    std::transform(out_ad.begin(), out_ad.end(), out.begin(), [](adouble x) {return Value(x);});
    return out;
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_catches_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om.catches(biol_no, indices_min, indices_max);
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_landings_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om.landings(biol_no, indices_min, indices_max);
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_discards_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om.discards(biol_no, indices_min, indices_max);
}

// [[Rcpp::export]]
std::vector<double> test_operatingModel_get_target_value(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int target_no, const int sim_target_no){
    operatingModel om(flfs, biols, ctrl);
    std::vector<double> out = om.get_target_value(target_no, sim_target_no);
    return out;
}

// [[Rcpp::export]]
std::vector<double> test_operatingModel_get_target_value2(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int target_no){
    operatingModel om(flfs, biols, ctrl);
    std::vector<double> out = om.get_target_value(target_no);
    return out;
}



///*----------- SSB calculations--------------*/
//
//// [[Rcpp::export]]
//FLQuantAD test_operatingModel_SSB_FLQ(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    return om.ssb(biol_no);
//}
//
//// [[Rcpp::export]]
//FLQuantAD test_operatingModel_SSB_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    return om.ssb(biol_no, indices_min, indices_max);
//}
//
///*
//// [[Rcpp::export]]
//FLQuantAD test_operatingModel_SSB_iters(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const int timestep, const int unit, const int area, const fwdControl ctrl){
//    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
//    operatingModel om(flfs, biol, f, f_spwn, ctrl);
//    const int biol_no = 1;
//    FLQuantAD out = om.ssb(timestep, unit, area, biol_no);
//    return out;
//}
//
//// [[Rcpp::export]]
//double test_operatingModel_SSB_single_iter(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const int timestep, const int unit, const int area, const int iter, const fwdControl ctrl){
//    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
//    operatingModel om(flfs, biol, f, f_spwn, ctrl);
//    const int biol_no = 1;
//    adouble out = om.ssb(timestep, unit, area, iter, biol_no);
//    return Value(out);
//}
//
//// [[Rcpp::export]]
//double test_operatingModel_SSB_single_iter_year_season(FLFisheriesAD flfs, SEXP flb_sexp, const std::string model_name, const FLQuant params, const int timelag, const FLQuant residuals, const bool residuals_mult, const FLQuant7AD f, const FLQuant7 f_spwn, const int year, const int unit, const int season, const int area, const int iter, const fwdControl ctrl){
//    fwdBiolAD biol(flb_sexp, model_name, params, timelag, residuals, residuals_mult);
//    operatingModel om(flfs, biol, f, f_spwn, ctrl);
//    const int biol_no = 1;
//    adouble out = om.ssb(year, unit, season, area, iter, biol_no);
//    return Value(out);
//}
//
//*/
//
//// [[Rcpp::export]]
//std::vector<unsigned int> test_operatingModel_get_target_age_range_indices(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const unsigned int target_no, const unsigned int sim_target_no, const unsigned int biol_no){ 
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//    std::vector<unsigned int> age_range_indices = om.get_target_age_range_indices(target_no, sim_target_no, biol_no);
//    return age_range_indices;
//}


//
///*
//
//*/
