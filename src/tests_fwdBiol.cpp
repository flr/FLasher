/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */


#include "../../inst/include/fwdBiol.h"

/*-------------------------------------------------------*/

//'@title Tests for CPP implementation of fwdBiol
//
//' On black blessed wings
//'@param flb_sexp a parameter
//'@param fwdb something
//'@param model_name something
//'@param params something
//'@param residuals something
//'@param residuals_mult something
//'@param fwdb1 something
//'@param quant something
//'@param year something
//'@param unit something
//'@param season something
//'@param area something
//'@param iter something
//'@param value something
//'@param indices_min something
//'@param indices_max something
//'@param values something
//'@param fwb something
//'@param biols_in something
//'@param fwbs something
//'@param element something
//'@param biol_no something
//'@param fwbs1 something
//'@param dims something
//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiol test_fwdBiol_sexp_constructor(SEXP flb_sexp){
	fwdBiol fwdb(flb_sexp);
	return fwdb;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolAD test_fwdBiolAD_sexp_constructor(SEXP flb_sexp){
	fwdBiolAD fwdb(flb_sexp);
	return fwdb;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiol test_fwdBiol_as_wrap(fwdBiol fwdb){
	return fwdb;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolAD test_fwdBiolAD_as_wrap(fwdBiolAD fwdb){
	return fwdb;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiol_fwdSR_constructor(SEXP flb_sexp, const std::string model_name, const FLQuant params, const FLQuant residuals, const bool residuals_mult){
    fwdSR fwsr(model_name, params, residuals, residuals_mult);
    fwdBiol fwb(flb_sexp, fwsr);
	return Rcpp::List::create(Rcpp::Named("fwb", fwb),
				Rcpp::Named("srr",fwb.get_srr()));
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiolAD_fwdSRAD_constructor(SEXP flb_sexp, const std::string model_name, const FLQuant params, const FLQuant residuals, const bool residuals_mult){
    fwdSRAD fwsr(model_name, params, residuals, residuals_mult);
    fwdBiolAD fwb(flb_sexp, fwsr);
	return Rcpp::List::create(Rcpp::Named("fwb", fwb),
				Rcpp::Named("srr",fwb.get_srr()));
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiol_FLSR_bits_constructor(SEXP flb_sexp, const std::string model_name, const FLQuant params, const FLQuant residuals, const bool residuals_mult){
    fwdBiol fwb(flb_sexp, model_name, params, residuals, residuals_mult);
	return Rcpp::List::create(Rcpp::Named("fwb", fwb),
				Rcpp::Named("srr",fwb.get_srr()));
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiolAD_FLSR_bits_constructor(SEXP flb_sexp, const std::string model_name, const FLQuant params, const FLQuant residuals, const bool residuals_mult){
    fwdBiolAD fwb(flb_sexp, model_name, params, residuals, residuals_mult);
	return Rcpp::List::create(Rcpp::Named("fwb", fwb),
				Rcpp::Named("srr",fwb.get_srr()));
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiol test_fwdBiol_copy_constructor(fwdBiol fwdb1){
	fwdBiol fwdb2(fwdb1); // uses copy constructor
    return fwdb2;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolAD test_fwdBiolAD_copy_constructor(fwdBiolAD fwdb1){
	fwdBiolAD fwdb2(fwdb1); // uses copy constructor
    return fwdb2;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiol_copy_constructor2(fwdBiol fwdb1, int quant, int year, int unit, int season, int area, int iter, double value){
	fwdBiol fwdb2(fwdb1); // uses copy constructor
	fwdb2.n()(quant,year,unit,season,area,iter) = value;
	return Rcpp::List::create(Rcpp::Named("fwdb1", fwdb1),
				Rcpp::Named("fwdb2",fwdb2));
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiolAD_copy_constructor2(fwdBiolAD fwdb1, int quant, int year, int unit, int season, int area, int iter, double value){
    adouble value_ad = value;
	fwdBiolAD fwdb2(fwdb1); // uses copy constructor
	fwdb2.n()(quant,year,unit,season,area,iter) = value_ad;
	return Rcpp::List::create(Rcpp::Named("fwdb1", fwdb1),
				Rcpp::Named("fwdb2",fwdb2));
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiol test_fwdBiol_assignment_operator(fwdBiol fwdb1){
	fwdBiol fwdb2 = fwdb1; 
    return fwdb2;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolAD test_fwdBiolAD_assignment_operator(fwdBiolAD fwdb1){
	fwdBiolAD fwdb2 = fwdb1; 
    return fwdb2;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiol_assignment_operator2(fwdBiol fwdb1, int quant, int year, int unit, int season, int area, int iter, double value){
	fwdBiol fwdb2 = fwdb1; 
	fwdb2.n()(quant,year,unit,season,area,iter) = value;
	return Rcpp::List::create(Rcpp::Named("fwdb1", fwdb1),
				Rcpp::Named("fwdb2",fwdb2));
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiolAD_assignment_operator2(fwdBiolAD fwdb1, int quant, int year, int unit, int season, int area, int iter, double value){
    adouble value_ad = value;
	fwdBiolAD fwdb2 = fwdb1; 
	fwdb2.n()(quant,year,unit,season,area,iter) = value_ad;
	return Rcpp::List::create(Rcpp::Named("fwdb1", fwdb1),
				Rcpp::Named("fwdb2",fwdb2));
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::NumericVector test_fwdBiol_const_get_accessors(const fwdBiol fwdb,int quant, int year, int unit, int season, int area, int iter){
    Rcpp::NumericVector out(6);
    out[0] = fwdb.n()(quant, year, unit, season, area, iter);
    out[1] = fwdb.m()(quant, year, unit, season, area, iter);
    out[2] = fwdb.wt()(quant, year, unit, season, area, iter);
    out[3] = fwdb.fec()(quant, year, unit, season, area, iter);
    out[4] = fwdb.spwn()(1, year, unit, season, area, iter);
    out[5] = fwdb.mat()(quant, year, unit, season, area, iter);
    return out;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiol_const_get_accessors_subset(const fwdBiol fwdb, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    std::vector<unsigned int> spwn_indices_min = indices_min;
    spwn_indices_min[0] = 1;
    std::vector<unsigned int> spwn_indices_max = indices_max;
    spwn_indices_max[0] = 1;
    return Rcpp::List::create(
        Rcpp::Named("n", fwdb.n(indices_min, indices_max)),
        Rcpp::Named("m", fwdb.m(indices_min, indices_max)),
        Rcpp::Named("wt", fwdb.wt(indices_min, indices_max)),
        Rcpp::Named("fec", fwdb.fec(indices_min, indices_max)),
        Rcpp::Named("spwn", fwdb.spwn(spwn_indices_min, spwn_indices_max)),
        Rcpp::Named("mat", fwdb.mat(indices_min, indices_max)));
}


//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::NumericVector test_fwdBiol_get_accessors(fwdBiol fwdb,int quant, int year, int unit, int season, int area, int iter){
    Rcpp::NumericVector out(6);
    out[0] = fwdb.n()(quant, year, unit, season, area, iter);
    out[1] = fwdb.m()(quant, year, unit, season, area, iter);
    out[2] = fwdb.wt()(quant, year, unit, season, area, iter);
    out[3] = fwdb.fec()(quant, year, unit, season, area, iter);
    out[4] = fwdb.spwn()(1, year, unit, season, area, iter);
    out[5] = fwdb.mat()(quant, year, unit, season, area, iter);
    return out;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::NumericVector test_fwdBiolAD_const_get_accessors(const fwdBiolAD fwdb,int quant, int year, int unit, int season, int area, int iter){
    Rcpp::NumericVector out(6);
    //out[0] = fwdb.n()(quant, year, unit, season, area, iter).value();
    out[0] = Value(fwdb.n()(quant, year, unit, season, area, iter));
    out[1] = fwdb.m()(quant, year, unit, season, area, iter);
    out[2] = fwdb.wt()(quant, year, unit, season, area, iter);
    out[3] = fwdb.fec()(quant, year, unit, season, area, iter);
    out[4] = fwdb.spwn()(1, year, unit, season, area, iter);
    out[5] = fwdb.mat()(quant, year, unit, season, area, iter);
    return out;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::NumericVector test_fwdBiolAD_get_accessors(fwdBiolAD fwdb,int quant, int year, int unit, int season, int area, int iter){
    Rcpp::NumericVector out(6);
    //out[0] = fwdb.n()(quant, year, unit, season, area, iter).value();
    out[0] = Value(fwdb.n()(quant, year, unit, season, area, iter));
    out[1] = fwdb.m()(quant, year, unit, season, area, iter);
    out[2] = fwdb.wt()(quant, year, unit, season, area, iter);
    out[3] = fwdb.fec()(quant, year, unit, season, area, iter);
    out[4] = fwdb.spwn()(1, year, unit, season, area, iter);
    out[5] = fwdb.mat()(quant, year, unit, season, area, iter);
    return out;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiol test_fwdBiol_set_accessors(fwdBiol fwdb, int quant, int year, int unit, int season, int area, int iter, Rcpp::NumericVector values){
    fwdb.n()(quant, year, unit, season, area, iter) = values[0];
    fwdb.m()(quant, year, unit, season, area, iter) = values[1];
    fwdb.wt()(quant, year, unit, season, area, iter) = values[2];
    fwdb.fec()(quant, year, unit, season, area, iter) = values[3];
    fwdb.spwn()(1, year, unit, season, area, iter) = values[4];
    fwdb.mat()(quant, year, unit, season, area, iter) = values[5];
    return fwdb;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolAD test_fwdBiolAD_set_accessors(fwdBiolAD fwdb, int quant, int year, int unit, int season, int area, int iter, Rcpp::NumericVector values){
    adouble value_ad = values[0];
    fwdb.n()(quant, year, unit, season, area, iter) = value_ad;
    fwdb.m()(quant, year, unit, season, area, iter) = values[1];
    fwdb.wt()(quant, year, unit, season, area, iter) = values[2];
    fwdb.fec()(quant, year, unit, season, area, iter) = values[3];
    fwdb.spwn()(1, year, unit, season, area, iter) = values[4];
    fwdb.mat()(quant, year, unit, season, area, iter) = values[5];
    return fwdb;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
FLQuantAD fwdBiolAD_biomass_FLQ(fwdBiolAD fwdb){
    return fwdb.biomass();
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
FLQuantAD fwdBiolAD_biomass_subset(fwdBiolAD fwdb, const std::vector<unsigned int> indices_min, const std::vector<unsigned int> indices_max){
    return fwdb.biomass(indices_min, indices_max);
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolAD test_fwdBiolAD_n_direct_set_accessor(fwdBiolAD fwdb, int quant, int year, int unit, int season, int area, int iter, double value){
    fwdb.n(quant, year, unit, season, area, iter) = value;
    return fwdb;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
double test_fwdBiolAD_n_direct_get_accessor(fwdBiolAD fwdb, int quant, int year, int unit, int season, int area, int iter){
    adouble ad_out = fwdb.n(quant, year, unit, season, area, iter);
    double out = Value(ad_out);
    return out;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
unsigned int test_fwdBiol_srp_timelag(fwdBiol fwb){
    unsigned int out = fwb.srp_timelag();
    return out;
}

/*-------------------------------------------------------*/

// fwdBiols constructors

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiols test_fwdBiols_as_wrap(const fwdBiols biols_in){
    return biols_in;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolsAD test_fwdBiolsAD_as_wrap(const fwdBiolsAD biols_in){
    return biols_in;
}

// fwdBiol constructor
//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolsAD test_fwdBiolsAD_fwdBiolAD_constructor(SEXP flb_sexp, const std::string model_name, const FLQuant params, const FLQuant residuals, const bool residuals_mult){
    fwdBiolAD fwb(flb_sexp, model_name, params, residuals, residuals_mult);
    fwdBiolsAD fwbs(fwb);
    return fwbs;
}

// fwdBiolAD () get operator
//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolAD test_fwdBiolsAD_const_get_single_index_accessor(const fwdBiolsAD fwbs, const int element){
    fwdBiolAD fwb = fwbs(element);
    return fwb;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolAD test_fwdBiolsAD_get_single_index_accessor(fwdBiolsAD fwbs, const int element){
    fwdBiolAD fwb = fwbs(element);
    return fwb;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
double test_fwdBiolsAD_const_get_value_accessor(const fwdBiolsAD fwbs, const int biol_no, int quant, int year, int unit, int season, int area, int iter){
    adouble ad_value_out = fwbs(biol_no).n()(quant,year,unit,season,area,iter);
    return Value(ad_value_out);
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
double test_fwdBiolsAD_get_value_accessor(fwdBiolsAD fwbs, const int biol_no, int quant, int year, int unit, int season, int area, int iter){
    adouble ad_value_out = fwbs(biol_no).n()(quant,year,unit,season,area,iter);
    return Value(ad_value_out);
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolsAD test_fwdBiolsAD_set_single_index_accessor(fwdBiolsAD fwbs, const int element, SEXP flb_sexp, const std::string model_name, const FLQuant params, const FLQuant residuals, const bool residuals_mult){
    fwdBiolAD fwb(flb_sexp, model_name, params, residuals, residuals_mult);
    fwbs(element) = fwb;
    return fwbs;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolsAD test_fwdBiolsAD_set_value_accessor(fwdBiolsAD fwbs, const int biol_no, int quant, int year, int unit, int season, int area, int iter, double value){
    adouble ad_value = value;
    fwbs(biol_no).n()(quant,year,unit,season,area,iter) = ad_value;
    return fwbs;
}

// copy constructor
//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiolsAD_copy_constructor(fwdBiolsAD fwbs1, const int biol_no, const std::vector<int> dims, const double value){
    fwdBiolsAD fwbs2(fwbs1);
    fwbs1(biol_no).n()(dims[0], dims[1], dims[2], dims[3], dims[4], dims[5]) = value;
	return Rcpp::List::create(Rcpp::Named("fwbs1", fwbs1),
				Rcpp::Named("fwbs2", fwbs2));
}

// assignment operator
//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiolsAD_assignment_operator(fwdBiolsAD fwbs1, const int biol_no, const std::vector<int> dims, const double value){
    fwdBiolsAD fwbs2;
    fwbs2 = fwbs1;
    fwbs1(biol_no).n()(dims[0], dims[1], dims[2], dims[3], dims[4], dims[5]) = value;
	return Rcpp::List::create(Rcpp::Named("fwbs1", fwbs1),
				Rcpp::Named("fwbs2", fwbs2));
}

// iterators
//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
Rcpp::List test_fwdBiolsAD_const_iterator(const fwdBiolsAD fwbs){
    Rcpp::List out;
    for (const auto biol : fwbs){
        out.push_back(biol.n());
    }
    return out;
}

//'@rdname fwdBiol-cpp-tests
// [[Rcpp::export]]
fwdBiolsAD test_fwdBiolsAD_iterator(fwdBiolsAD fwbs, int quant, int year, int unit, int season, int area, int iter, double value){
    for (auto& biol : fwbs){
        biol.n()(quant, year, unit, season, area, iter) = value;
    }
    return fwbs;
}



