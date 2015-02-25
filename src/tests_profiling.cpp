
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

//--------- biomass --------------

// [[Rcpp::export]]
void fwdBiolAD_biomass_subset_speed(fwdBiolAD fwdb, const int year, const int season, const int rep){
    Rcpp::IntegerVector dims = fwdb.biomass().get_dim();

    clock_t start, end;
    start = clock();
    // Get full biomass and then subset by year and season
    for (int i = 1; i <= rep; ++i){
        FLQuantAD biol_full = fwdb.biomass();
        FLQuantAD biol_subset1 = biol_full(1, dims[0], year, year, 1, dims[2], season, season, 1, dims[4], 1, dims[5]);
    }
    end = clock();
    Rprintf("biomass all CPU time: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));

    start = clock();
    // Get subset of biomass
    for (int i = 1; i <= rep; ++i){
        FLQuantAD biol_subset2 = fwdb.biomass(year, year, 1, dims[2], season, season, 1, dims[4], 1, dims[5]);
    }
    end = clock();
    Rprintf("biomass subset: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));

    return;
}

// [[Rcpp::export]]
void catch_q_speed(FLFisheriesAD flfs, SEXP flbs_list_sexp, const fwdControl ctrl, const int fishery_no, const int catch_no, const int biol_no, const std::vector<unsigned int> indices, const int rep){
    fwdBiolsAD biols(flbs_list_sexp);
    operatingModel om(flfs, biols, ctrl);

    clock_t start, end;
    start = clock();
    // Get full catch_q FLQ, then subset
    for (int i = 1; i <= rep; ++i){
        FLQuantAD cq1 = om.catch_q(fishery_no, catch_no, biol_no);
        FLQuantAD cq_subset1 = cq1(1, 1, indices[0], indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], indices[7], indices[8], indices[9]);
    }
    end = clock();
    Rprintf("q subset full: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
    start = clock();
    for (int i = 1; i <= rep; ++i){
        FLQuantAD cq2 = om.catch_q(fishery_no, catch_no, biol_no,indices[0], indices[1], indices[2], indices[3], indices[4], indices[5], indices[6], indices[7], indices[8], indices[9]);
    }
    end = clock();
    Rprintf("q subset: %f\n", (end - start) / (double)(CLOCKS_PER_SEC));
}


//---------------------- Remove all stuff below this line -------

// [[Rcpp::export]]
int useAuto() {
    auto val = 42;		// val will be of type int
    return val;
}

// [[Rcpp::export]]
std::vector<int> init_list() {
    //std::vector<int> out = {1,2,3,4};
    auto out = {1,2,3,4}; // genius
    return out;
}
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector transformEx2(NumericVector x, NumericVector y) {
    NumericVector z(x.size());
    std::transform(x.begin(), x.end(), y.begin(), z.begin(), 
                   [](double x, double y) { return sqrt(x*x + y*y); } );
    return z;
}


