
/* 
 * Copyright 2015 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

// For timing functions
#include <time.h>

#include "../inst/include/operating_model.h"


/* These tests are for development purposes only and could be removed before release.
 * They are not called as part of the testing suite - they are called by the R scratch script
 */



//--------- Conditional --------

// indep has a length of 1
// [[Rcpp::export]]
std::vector<double> test_standard_conditional(std::vector<double> indep){
    std::vector<adouble> indep_ad(1, indep[0]);
    // Tape on
    CppAD::Independent(indep_ad);
    std::vector<adouble> dep_ad(1, 0.0);
    // Standard conditional - only one option gets taped
    if (indep_ad[0] >= 4.0){
        Rprintf("Taping y = x^2\n");
        dep_ad[0] = indep_ad[0] * indep_ad[0];
    }
    else {
        Rprintf("Taping y = x^3\n");
        dep_ad[0] = indep_ad[0] * indep_ad[0] * indep_ad[0];
    }
    // Tape off
    CppAD::ADFun<double> fun(indep_ad, dep_ad);
    std::vector<double> dep(1, 0.0);
    std::vector<double> jac(1, 0.0);
    std::vector<double> out(4, 0.0);
    // Interogate tape at at x >= 4
    indep[0] = 4.0;
    dep = fun.Forward(0, indep); 
    jac = fun.Jacobian(indep);
    out[0] = dep[0];
    out[1] = jac[0];
    // Interrogate at x = 3
    indep[0] = 3.0;
    dep = fun.Forward(0, indep); 
    jac = fun.Jacobian(indep);
    out[2] = dep[0];
    out[3] = jac[0];
    // Both get evaluated at the one that was taped - not what we want
    return out;
}

// [[Rcpp::export]]
std::vector<double> test_cppad_conditional(std::vector<double> indep){
    std::vector<adouble> indep_ad(1, indep[0]);
    // Tape on
    CppAD::Independent(indep_ad);
    std::vector<adouble> dep_ad(1, 0.0);

    // Records both sides
    // result = CondExpRel(left, right, if_true, if_false) 
    // All types the same - no double
    adouble max_indep = 4.0;

//    dep_ad[0] = CondExpGe(indep_ad[0], max_indep,
//            indep_ad[0] * indep_ad[0],
//            indep_ad[0] * indep_ad[0] * indep_ad[0]);

    // Can we execute more complicated things?
    // Looks like it
    dep_ad[0] = CondExpGe(indep_ad[0], max_indep,
            ({ 
            adouble test = indep_ad[0];
            test * test;
            })
            ,
            {indep_ad[0] * indep_ad[0] * indep_ad[0]});
    // Tape off
    CppAD::ADFun<double> fun(indep_ad, dep_ad);
    std::vector<double> dep(1, 0.0);
    std::vector<double> jac(1, 0.0);
    std::vector<double> out(4, 0.0);
    // Interogate tape at at x >= 4
    indep[0] = 4.0;
    dep = fun.Forward(0, indep); 
    jac = fun.Jacobian(indep);
    out[0] = dep[0];
    out[1] = jac[0];
    // Interrogate at x = 3
    indep[0] = 3.0;
    dep = fun.Forward(0, indep); 
    jac = fun.Jacobian(indep);
    out[2] = dep[0];
    out[3] = jac[0];
    // Both get evaluated at the one that was taped - not what we want
    return out;
}


//--------- NA tests -------------


//// NAs passed in as a std::vector or converted to std::vector using as<> (same thing) is NOT detected by Rcpp::IntegerVector::is_na()
//// But an unsigned int created from an Rcpp::IntegerVector IS detected (weird)
//// [[Rcpp::export]]
//void test_integer_NA(const Rcpp::IntegerVector vec1, const std::vector<unsigned int> vec2){
//
//    Rprintf("Testing integers\n");
//    Rprintf("Testing Rcpp::IntegerVector\n");
//    if (Rcpp::IntegerVector::is_na(vec1[0])){
//            Rprintf("It's NA\n");
//    }
//    else {
//        Rprintf("It's not NA\n");
//    }
//    Rprintf("Testing std::vector<unsigned int>\n");
//    if (Rcpp::IntegerVector::is_na(vec2[0])){
//            Rprintf("It's NA\n");
//    }
//    else {
//        Rprintf("It's not NA\n");
//    }
//    unsigned int x = vec1[0];
//    Rprintf("Testing unsigned int from Rcpp::IntegerVector\n");
//    if (Rcpp::IntegerVector::is_na(x)){
//            Rprintf("It's NA\n");
//    }
//    else {
//        Rprintf("It's not NA\n");
//    }
//    std::vector<unsigned int> vec3 = Rcpp::as<std::vector<unsigned int>>(vec1);
//    Rprintf("Testing std::vector<unsigned int> from Rcpp::IntegerVector\n");
//    if (Rcpp::IntegerVector::is_na(vec3[0])){
//            Rprintf("It's NA\n");
//    }
//    else {
//        Rprintf("It's not NA\n");
//    }
//
//            
//// Rcpp::IntegerVector::is_na
//
//
//}
//
//
//
////--------- biomass --------------
//
//// [[Rcpp::export]]
//void fwdBiolAD_biomass_subset_speed(fwdBiolAD fwdb, const unsigned int year, const int unsigned season, const int rep){
//    //Rcpp::IntegerVector raw_dims = fwdb.biomass().get_dim();
//    std::vector<unsigned int> dims = fwdb.biomass().get_dim();
//
//    clock_t start, end;
//    start = clock();
//    // Get full biomass and then subset by year and season
//    for (int i = 1; i <= rep; ++i){
//        FLQuantAD biol_full = fwdb.biomass();
//        FLQuantAD biol_subset1 = biol_full(1, dims[0], year, year, 1, dims[2], season, season, 1, dims[4], 1, dims[5]);
//    }
//    end = clock();
//    Rprintf("biomass all CPU time: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
//
//
//    std::vector<unsigned int> indices_min {year, 1, season, 1, 1};
//    std::vector<unsigned int> indices_max {year, dims[2], season, dims[4], dims[5]};
//    start = clock();
//    // Get subset of biomass
//    for (int i = 1; i <= rep; ++i){
//        FLQuantAD biol_subset2 = fwdb.biomass(indices_min, indices_max);
//    }
//    end = clock();
//    Rprintf("biomass subset: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
//
//    return;
//}
//
////--------- catch_q --------------
//
//// [[Rcpp::export]]
//void catch_q_speed(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> dim_min, const std::vector<unsigned int> dim_max , const int rep){
//    fwdBiolsAD biols(flbs_list_sexp);
//    operatingModel om(flfs, biols, ctrl);
//
//    clock_t start, end;
//    start = clock();
//    // Get full catch_q FLQ, then subset
//    for (int i = 1; i <= rep; ++i){
//        FLQuantAD cq1 = om.catch_q(fishery_no, catch_no, biol_no);
//        FLQuantAD cq_subset1 = cq1(dim_min, dim_max);
//    }
//    end = clock();
//    Rprintf("q subset full: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
//    std::vector<unsigned int> dim_min5(dim_min.begin()+1, dim_min.end());
//    std::vector<unsigned int> dim_max5(dim_max.begin()+1, dim_max.end());
//    start = clock();
//    for (int i = 1; i <= rep; ++i){
//        FLQuantAD cq2 = om.catch_q(fishery_no, catch_no, biol_no, dim_min5, dim_max5);
//    }
//    end = clock();
//    Rprintf("q subset: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
//}
//
////---------------------- get f speed -------
//// [[Rcpp::export]]
//void get_f_speed(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max, const int rep){
//    Rprintf("in get f speed\n");
//    clock_t start, end;
//    start = clock();
//        fwdBiolsAD biols(flbs_list_sexp);
//        operatingModel om(flfs, biols, ctrl);
//    end = clock();
//    Rprintf("making om: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
//
//    start = clock();
//    // Get full get_f FLQ, then subset
//    for (int i = 1; i <= rep; ++i){
//        FLQuantAD f1 = om.get_f(fishery_no, catch_no, biol_no);
//        FLQuantAD f2 = f1(indices_min[0], indices_max[0], indices_min[1], indices_max[1], indices_min[2], indices_max[2], indices_min[3], indices_max[3], indices_min[4], indices_max[4], indices_min[5], indices_max[5]);
//    }
//    end = clock();
//    Rprintf("f subset full: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
//
//    start = clock();
//    // Get subset get_f FLQ, then subset
//    for (int i = 1; i <= rep; ++i){
//        FLQuantAD f3 = om.get_f(fishery_no, catch_no, biol_no, indices_min, indices_max);
//    }
//    end = clock();
//    Rprintf("f subset: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
//
//    std::vector<unsigned int> indices_min5(indices_min.begin()+1, indices_min.end());
//    std::vector<unsigned int> indices_max5(indices_max.begin()+1, indices_max.end());
//
//    start = clock();
//    // Get subset catch_q FLQ
//    for (int i = 1; i <= rep; ++i){
//        FLQuantAD q1 = om.catch_q(fishery_no, catch_no, biol_no, indices_min5, indices_max5);
//    }
//    end = clock();
//    Rprintf("q subset: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
//    return; 
//
//}
//
////---------------------- quant indices accessor-------
//
//// [[Rcpp::export]]
//void test_quant_indices_speed(fwdBiol fwdb, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max, const int rep){
//    clock_t start, end;
//    FLQuant test;
//    start = clock();
//    for (int i = 1; i <= rep; ++i){
//        test = fwdb.wt()(indices_min, indices_max);
//    }
//    end = clock();
//    Rprintf("whole then subset: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
//    start = clock();
//    for (int i = 1; i <= rep; ++i){
//        test = fwdb.wt(indices_min, indices_max);
//    }
//    end = clock();
//    Rprintf("subset: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
//    return;
//}
//
//
////---------------------- Remove all stuff below this line -------
//
//// [[Rcpp::export]]
//int useAuto() {
//    auto val = 42;		// val will be of type int
//    return val;
//}
//
//// [[Rcpp::export]]
//std::vector<int> init_list() {
//    //std::vector<int> out = {1,2,3,4};
//    auto out = {1,2,3,4}; // genius
//    return out;
//}
//using namespace Rcpp;
//
//// [[Rcpp::export]]
//NumericVector transformEx2(NumericVector x, NumericVector y) {
//    NumericVector z(x.size());
//    std::transform(x.begin(), x.end(), y.begin(), z.begin(), 
//                   [](double x, double y) { return sqrt(x*x + y*y); } );
//    return z;
//}
//
//class A {
//    public:
//    A()
//    {
//        v = {1,2,3,4,5};
//        Rprintf("size: %i\n", v.size());
//    }
//
//    A(std::vector<int> init){
//        v = init;
//        Rprintf("size: %i\n", v.size());
//    }
//
//    int* begin() {
//        return &v[0];
//    }
//
//    int* end() {
//        return &v[v.size()];
//    }
//
//    protected:
//        std::vector<int> v;
//};
//
//// [[Rcpp::export]]
//void begin_test(std::vector<int> init){
//    A a(init);
//    for( auto it : a )
//    {
//        Rprintf("%i\n", it);
//    }
//    Rprintf("\n");
//    A b;
//    for( auto it : b )
//    {
//        Rprintf("%i\n", it);
//    }
//}
//
//
//
