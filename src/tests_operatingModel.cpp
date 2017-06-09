/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

// For timing functions
#include <time.h>
#include "../../inst/include/operating_model.h"

/* -------------- constructors and wrappers ----------------- */

//'@title Tests for CPP implementation of operatingModel
//
//' Blood mountain
//'@param flfs a parameter
//'@param biols something
//'@param ctrl something
//'@param biol_no something
//'@param indices_min something
//'@param indices_max something
//'@param fishery_no something
//'@param unit something
//'@param rec_timestep something
//'@param catch_no something
//'@param timestep something
//'@param effort_mult_initial something
//'@param indep_min something
//'@param indep_max something
//'@param nr_iters something
//'@param strquantity something
//'@param target_no something
//'@param sim_target_no something
//'@param relative something
//'@param flbs_list_sexp something
//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
void test_operatingModel_empty_constructor(){
    operatingModel om;
    return;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
operatingModel test_operatingModel_full_constructor(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl){
    operatingModel om(flfs, biols, ctrl);
    return om;
}

/*----------- House keeping methods */

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
unsigned int test_operatingModel_get_niter(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl){
    operatingModel om(flfs, biols, ctrl);
    return om.get_niter();
}

/*----------- SRP and Rec calculations--------------*/

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_get_exp_z_pre_spwn(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD out = om.get_exp_z_pre_spwn(biol_no, indices_min, indices_max);
    return out;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_f_prop_spwn_FLQ_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int fishery_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.f_prop_spwn(fishery_no, biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_SRP_FLQ_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.srp(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_total_SRP_FLQ_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.total_srp(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
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
//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_get_f_FCB(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD f = om.get_f(fishery_no, catch_no, biol_no);
    return f;
}

// get_f() subset
// No check is made if FC catches B
//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_get_f_FCB_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, std::vector<unsigned int> indices_min, std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD f = om.get_f(fishery_no, catch_no, biol_no, indices_min, indices_max);
    return f;
}

// Total F on a biol
//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_get_f_B_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min,  const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD total_f = om.get_f(biol_no, indices_min, indices_max);
    return total_f;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_get_f_B(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD total_f = om.get_f(biol_no);
    return total_f;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_fbar_FCB(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD fb = om.fbar(fishery_no, catch_no, biol_no, indices_min, indices_max);
    return fb;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_fbar_B(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD fb = om.fbar(biol_no, indices_min, indices_max);
    return fb;
}

/*----------- nunit_z and nunit_f--------------*/

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_nunit_z_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD z = om.get_nunit_z(biol_no, indices_min, indices_max);
    return z;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_nunit_f_B_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD f = om.get_nunit_f(biol_no, indices_min, indices_max);
    return f;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_nunit_f_FCB_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuantAD f = om.get_nunit_f(fishery_no, catch_no, biol_no, indices_min, indices_max);
    return f;
}

/*----------- project and run methods --------------*/
//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_survivors(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    FLQuant out = om.survivors(biol_no, indices_min, indices_max);
    return out;
}


//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
operatingModel test_operatingModel_project_biols(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int timestep){
    operatingModel om(flfs, biols, ctrl);
    om.project_biols(timestep);
    return om;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
operatingModel test_operatingModel_project_fisheries(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int timestep){
    operatingModel om(flfs, biols, ctrl);
    om.project_fisheries(timestep);
    return om;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
operatingModel test_operatingModel_run(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const double effort_mult_initial, const double indep_min, const double indep_max, const int nr_iters = 50){
    operatingModel om(flfs, biols, ctrl);
    Rcpp::IntegerMatrix solver_codes = om.run(effort_mult_initial, indep_min, indep_max, nr_iters);
    return om;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_operatingModel_run2(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const double effort_mult_initial, const double indep_min, const double indep_max, const int nr_iters = 50){
    operatingModel om(flfs, biols, ctrl);
    Rcpp::IntegerMatrix solver_codes = om.run(effort_mult_initial, indep_min, indep_max, nr_iters);
	return Rcpp::List::create(Rcpp::Named("om", om),
        Rcpp::Named("solver_codes",solver_codes));
}

/*----------- getting target values --------------*/

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_eval_om(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const std::string strquantity, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    fwdControlTargetType quantity = ctrl.get_target_type(strquantity);
    FLQuantAD out = om.eval_om(quantity, fishery_no, catch_no, biol_no, indices_min, indices_max);
    return out;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
std::vector<double> test_operatingModel_get_target_value_hat(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int target_no, const int sim_target_no){
    operatingModel om(flfs, biols, ctrl);
    std::vector<adouble> out_ad = om.get_target_value_hat(target_no, sim_target_no);
    std::vector<double> out(out_ad.size());
    std::transform(out_ad.begin(), out_ad.end(), out.begin(), [](adouble x) {return Value(x);});
    return out;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
std::vector<double> test_operatingModel_get_target_value_hat2(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int target_no){
    operatingModel om(flfs, biols, ctrl);
    std::vector<adouble> out_ad = om.get_target_value_hat(target_no);
    std::vector<double> out(out_ad.size());
    std::transform(out_ad.begin(), out_ad.end(), out.begin(), [](adouble x) {return Value(x);});
    return out;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
std::vector<double> test_operatingModel_get_target_value(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int target_no, const int sim_target_no){
    operatingModel om(flfs, biols, ctrl);
    std::vector<double> out = om.get_target_value(target_no, sim_target_no);
    return out;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
std::vector<double> test_operatingModel_get_target_value2(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int target_no){
    operatingModel om(flfs, biols, ctrl);
    std::vector<double> out = om.get_target_value(target_no);
    return out;
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_operatingModel_get_target_hat_indices(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const unsigned int target_no, const unsigned int sim_target_no, const unsigned int target_component, const bool relative){
    operatingModel om(flfs, biols, ctrl);
    std::vector<unsigned int> indices_min(6,0); 
    std::vector<unsigned int> indices_max(6,0);
    om.get_target_hat_indices(indices_min, indices_max, target_no, sim_target_no, target_component, relative);
	return Rcpp::List::create(Rcpp::Named("indices_min", indices_min),
        Rcpp::Named("indices_max", indices_max));
}

/*----------- target calculations --------------*/

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_catches_subset(FLFisheriesAD flfs, fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.catches(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_landings_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om.landings(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_discards_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om.discards(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_catch_n_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om.catch_n(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_landings_n_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om.landings_n(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_discards_n_subset(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);
    return om.discards_n(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_ssb_start(const FLFisheriesAD flfs, const fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.ssb_start(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_biomass_start(const FLFisheriesAD flfs, const fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.biomass_start(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_ssb_end(const FLFisheriesAD flfs, const fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.ssb_end(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_ssb_spawn(const FLFisheriesAD flfs, const fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.ssb_spawn(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_ssb_flash(const FLFisheriesAD flfs, const fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.ssb_flash(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_biomass_flash(const FLFisheriesAD flfs, const fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.biomass_flash(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_biomass_end(const FLFisheriesAD flfs, const fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.biomass_end(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
FLQuantAD test_operatingModel_biomass_spawn(const FLFisheriesAD flfs, const fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.biomass_spawn(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
bool test_operatingModel_spawn_before_fishing(const FLFisheriesAD flfs, const fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.spawn_before_fishing(biol_no, indices_min, indices_max);
}

//'@rdname operatingModel-cpp-tests
// [[Rcpp::export]]
bool test_operatingModel_fishing_before_spawn(const FLFisheriesAD flfs, const fwdBiolsAD biols, const fwdControl ctrl, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    operatingModel om(flfs, biols, ctrl);
    return om.fishing_before_spawn(biol_no, indices_min, indices_max);
}

