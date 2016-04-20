/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

// For timing functions
#include <time.h>
#include "../../inst/include/operating_model.h"

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

/*----------- SRP and Rec calculations--------------*/

// [[Rcpp::export]]
FLQuantAD test_operatingModel_f_prop_spwn_FLQ_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int fishery_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.f_prop_spwn(fishery_no, biol_no, indices_min, indices_max);
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_SRP_FLQ_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.srp(biol_no, indices_min, indices_max);
}

// [[Rcpp::export]]
std::vector<double> test_operatingModel_calc_rec(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const int unit, const int rec_timestep){
    operatingModel om(flfs, biols, ctrl);
    std::vector<adouble> out_ad = om.calc_rec(biol_no, unit, rec_timestep);
    std::vector<double> out(out_ad.size());
    std::transform(out_ad.begin(), out_ad.end(), out.begin(), [](adouble x) {return Value(x);});
    return out;
}

/*----------- F methods --------------*/

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
FLQuantAD test_operatingModel_fbar_subset1(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD fb = om.fbar(fishery_no, catch_no, biol_no, indices_min, indices_max);
    return fb;
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_fbar_subset2(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD fb = om.fbar(biol_no, indices_min, indices_max);
    return fb;
}

/*----------- unit_z and unit_f--------------*/

// [[Rcpp::export]]
FLQuantAD test_operatingModel_unit_z_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD z = om.get_unit_z(biol_no, indices_min, indices_max);
    return z;
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_unit_f_B_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD f = om.get_unit_f(biol_no, indices_min, indices_max);
    return f;
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_unit_f_FCB_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD f = om.get_unit_f(fishery_no, catch_no, biol_no, indices_min, indices_max);
    return f;
}

/*----------- project and run methods --------------*/
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
operatingModel test_operatingModel_run(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const double effort_mult_initial, const double indep_min, const double indep_max, const int nr_iters = 50){
    operatingModel om(flfs, biols, ctrl);
    Rcpp::IntegerMatrix solver_codes = om.run(effort_mult_initial, indep_min, indep_max, nr_iters);
    return om;
}

// [[Rcpp::export]]
Rcpp::List test_operatingModel_run2(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const double effort_mult_initial, const double indep_min, const double indep_max, const int nr_iters = 50){
    operatingModel om(flfs, biols, ctrl);
    Rcpp::IntegerMatrix solver_codes = om.run(effort_mult_initial, indep_min, indep_max, nr_iters);
	return Rcpp::List::create(Rcpp::Named("om", om),
        Rcpp::Named("solver_codes",solver_codes));
}

/*----------- getting target values --------------*/

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
std::vector<double> test_operatingModel_get_target_value(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int target_no, const int sim_target_no){
    operatingModel om(flfs, biols, ctrl);
    std::vector<adouble> out_ad = om.get_target_value(target_no, sim_target_no);
    std::vector<double> out(out_ad.size());
    std::transform(out_ad.begin(), out_ad.end(), out.begin(), [](adouble x) {return Value(x);});
    return out;
}

// [[Rcpp::export]]
std::vector<double> test_operatingModel_get_target_value2(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int target_no){
    operatingModel om(flfs, biols, ctrl);
    std::vector<adouble> out_ad = om.get_target_value(target_no);
    std::vector<double> out(out_ad.size());
    std::transform(out_ad.begin(), out_ad.end(), out.begin(), [](adouble x) {return Value(x);});
    return out;
}

// [[Rcpp::export]]
std::vector<unsigned int> test_operatingModel_get_target_age_range_indices(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const unsigned int target_no, const unsigned int sim_target_no){ 
    operatingModel om(flfs, biols, ctrl);
    std::vector<unsigned int> age_range_indices = om.get_target_age_range_indices(target_no, sim_target_no);
    return age_range_indices;
}

/*----------- target calculations --------------*/

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
FLQuantAD test_operatingModel_catch_n_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om.catch_n(biol_no, indices_min, indices_max);
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_landings_n_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om.landings_n(biol_no, indices_min, indices_max);
}

// [[Rcpp::export]]
FLQuantAD test_operatingModel_discards_n_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om.discards_n(biol_no, indices_min, indices_max);
}

