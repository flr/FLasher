/* 
 * Copyright 2014 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include <time.h>

#include "../inst/include/operating_model.h"

// Converting timestep to year and season and vice versa
// These are based on INDICES, not characters
template <typename T>
void year_season_to_timestep(const int year, const int season, const FLQuant_base<T>& flq, int& timestep){
    year_season_to_timestep(year, season, flq.get_nseason(), timestep);
}

template <typename T>
void timestep_to_year_season(const int timestep, const FLQuant_base<T>& flq, int& year, int& season){
    timestep_to_year_season(timestep, flq.get_nseason(), year, season);
}

void year_season_to_timestep(const int year, const int season, const int nseason, int& timestep){
    timestep = (year-1) * nseason + season;
}

void timestep_to_year_season(const int timestep, const int nseason, int& year, int& season){
    year =  (timestep-1) / nseason + 1; // integer divide - takes the floor
    season = (timestep-1) % nseason + 1;
}

// Instantiate
template void year_season_to_timestep(const int year, const int season, const FLQuant_base<double>& flq, int& timestep);
template void year_season_to_timestep(const int year, const int season, const FLQuant_base<adouble>& flq, int& timestep);

template void timestep_to_year_season(const int timestep, const FLQuant_base<double>& flq, int& year, int& season);
template void timestep_to_year_season(const int timestep, const FLQuant_base<adouble>& flq, int& year, int& season);

double euclid_norm(double* x, const int size_x){
    double xsum = 0;
    for (int i=0; i<size_x; ++i){
        xsum += x[i] * x[i];
    }
    xsum = sqrt(xsum);
    return xsum;
}

/*
int newton_raphson(std::vector<double>& indep, const int adolc_tape, const int max_iters, const double max_limit, const double tolerance){
    Rprintf("In Newton-Raphson\n");
    const int nindep = indep.size();
    double *x = new double[nindep];
    double *w = new double[nindep];
    // load initial values
    for (int i=0; i<nindep; ++i){
        x[i] = indep[i];
        w[i] = 1.0;
    }
    // solver continues until either the tolerance is reached, the maximum limit is reached, or the maximum iterations are reached
    int iter_count = 0;
    int reason_for_stopping = 0; // 0 - tolerance met; 1 - iter limit reached; 2 - max limit reached
    //while ((euclid_norm(w,nindep) > tolerance) && (euclid_norm(x,nindep) < max_limit) && (++iter_count < max_iters)){
    // First pass to get w
    function(adolc_tape, nindep, nindep, x, w);
    jac_solv(adolc_tape, nindep, x, w, 2);
    while(euclid_norm(w,nindep) > tolerance){
        iter_count++;
        if (euclid_norm(x,nindep) >= max_limit){
            reason_for_stopping = 2;
            Rprintf("Exiting Newton-Raphson early as x > max_limit.\n");
            break;
        }
        if (iter_count > max_iters){
            reason_for_stopping = 1;
            Rprintf("Exiting Newton-Raphson early as maxi_iters exceeded.\n");
            break;
        }
        function(adolc_tape, nindep, nindep, x, w);
        jac_solv(adolc_tape, nindep, x, w, 2);
        for (int i=0; i<nindep; ++i){
            x[i] -= w[i];	   
        }
        //Rprintf("x0 %f\n", x[0]);
        //Rprintf("x1 %f\n\n", x[1]);
    }
    Rprintf("NR iters: %i\n", iter_count);
    // load final values
    for (int i=0; i<nindep; ++i){
        indep[i] = x[i];
    }
    delete[] x;
    delete[] w;
    return reason_for_stopping;
}
*/



/*------------------------------------------------------------*/
// operatingModel class

// Empty constructor
operatingModel::operatingModel(){
    biol = fwdBiol();
    fisheries = FLFisheries();
    f = FLQuant7();
    f_spwn = FLQuant7();
    landings_n = FLQuant7AD();
    discards_n = FLQuant7AD();
    fad = FLQuant7AD();
    n = FLQuantAD();
}

// Main constructor
operatingModel::operatingModel(const FLFisheries fisheries_in, const fwdBiol biol_in, const FLQuant7 f_in, const FLQuant7 f_spwn_in, const fwdControl ctrl_in){
    // Check dims (1 - 5) of landings slots, F and biol are the same
    // Single Biol at the moment.
    // The Biol can be fished by multiple Catches - but each Catch must come from a seperate Fishery
    // Here we assume that each Fishery has one Catch that fishes that Biol - this assumption will break with multiple Biols
    // Biol dims (1 - 5) must therefore match the Catch dims (Fishery[[1]]) and all FLQuants in f and f_spwn
    // Dim 6 must be 1 or n
    const unsigned int nfisheries = fisheries_in.get_nfisheries();
    // nfisheries must equal length of f and f_spwn
    if (nfisheries != f_in.get_ndim7()){
        Rcpp::stop("operatingModel constructor: Number of fisheries must equal number F FLQuants\n");
    }
    if (nfisheries != f_spwn_in.get_ndim7()){
        Rcpp::stop("operatingModel constructor: Number of fisheries must equal number F_SPWN FLQuants\n");
    }
    Rcpp::IntegerVector catch_dim;
    Rcpp::IntegerVector f_dim;
    Rcpp::IntegerVector f_spwn_dim;
    Rcpp::IntegerVector biol_dim = biol_in.n().get_dim();
    for (int unsigned fishery_counter = 1; fishery_counter <= nfisheries; ++fishery_counter){
        catch_dim = fisheries_in(fishery_counter)(1).landings_n().get_dim(); // First catch of the fishery
        f_dim = f_in(fishery_counter).get_dim();
        f_spwn_dim = f_spwn_in(fishery_counter).get_dim();
        for (int dim_counter = 0; dim_counter < 5; ++dim_counter){
            if((biol_dim[dim_counter] != catch_dim[dim_counter]) || (biol_dim[dim_counter] != f_dim[dim_counter]) || (biol_dim[dim_counter] != f_spwn_dim[dim_counter])){
                Rcpp::stop("In operatingModel constructor: Biol dims must be the same as Catch, F and F_SPWN dims\n");
            }
        }
    }
    // Check residuals of the SRR - need to have them for the projection years in the control object

    // Add ITER check for ctrl
    biol = biol_in;
    fisheries = fisheries_in;
    f = f_in;
    f_spwn = f_spwn_in;
    ctrl = ctrl_in;
    /* Set AD members up - reserve space - how many iters? all ot them*/
    n = FLQuantAD(biol_dim[0], 1, biol_dim[2], 1, biol_dim[4], biol_dim[5]); // Assumes tha the number of iters in the biol is the same as all objects - is that right?
    landings_n = FLQuant7AD(n); // Add first FLQ to the list
    // Keep adding more elements to the list - one for each fishery
    for (unsigned int fishery_counter = 2; fishery_counter <= nfisheries; ++fishery_counter){
        landings_n(n); // push back
    } 
    discards_n = landings_n;
    fad = landings_n;
}

// Copy constructor - else members can be pointed at by multiple instances
operatingModel::operatingModel(const operatingModel& operatingModel_source){
    biol = operatingModel_source.biol;
    fisheries = operatingModel_source.fisheries;
    f = operatingModel_source.f;
    f_spwn = operatingModel_source.f_spwn;
    ctrl = operatingModel_source.ctrl;
}

// Assignment operator to ensure deep copy - else 'data' can be pointed at by multiple instances
operatingModel& operatingModel::operator = (const operatingModel& operatingModel_source){
	if (this != &operatingModel_source){
        biol = operatingModel_source.biol;
        fisheries = operatingModel_source.fisheries;
        f = operatingModel_source.f;
        f_spwn = operatingModel_source.f_spwn;
        ctrl = operatingModel_source.ctrl;
	}
	return *this;
}

/* Intrusive 'wrap' */
// Returns a list of stuff
operatingModel::operator SEXP() const{
    Rprintf("Wrapping operatingModel.\n");
    return Rcpp::List::create(Rcpp::Named("biol", biol),
                            Rcpp::Named("fisheries", fisheries),
                            Rcpp::Named("f", f),
                            Rcpp::Named("f_spwn", f_spwn),
                            Rcpp::Named("ctrl", ctrl),
                            Rcpp::Named("landings_n", landings_n),
                            Rcpp::Named("discards_n", discards_n),
                            Rcpp::Named("fad", fad),
                            Rcpp::Named("n", n));
}


// load up the landings_n, discards_n, fad and n members with the correct timestep data
void operatingModel::load_ad_members(const int year, const int season){
    Rcpp::IntegerVector dim = biol.n().get_dim();
    n = biol.n()(1,dim[0],year,year,1,dim[2],season,season,1,dim[4],1,dim[5]);
    for (unsigned int fishery_counter = 1; fishery_counter <= fisheries.get_nfisheries(); ++fishery_counter){
        landings_n(fishery_counter) =  fisheries(fishery_counter)(1).landings_n()(1,dim[0],year,year,1,dim[2],season,season,1,dim[4],1,dim[5]);
        discards_n(fishery_counter) =  fisheries(fishery_counter)(1).discards_n()(1,dim[0],year,year,1,dim[2],season,season,1,dim[4],1,dim[5]);
        fad(fishery_counter) =  f(fishery_counter)(1,dim[0],year,year,1,dim[2],season,season,1,dim[4],1,dim[5]);
    }
}

void operatingModel::load_ad_members(const int timestep){
    int year;
    int season;
    timestep_to_year_season(timestep, biol.n().get_nseason(), year, season);
    load_ad_members(year, season);
}

// Assumes that dims of n and landings and f are the same
// Only 1 biol
void operatingModel::update_from_ad_members(const int year, const int season){
    // n -> biol.n
    // Ugly nested for loops - sorry
    for (unsigned int quant_counter = 1; quant_counter <= n.get_nquant(); ++quant_counter){
        for (unsigned int unit_counter = 1; unit_counter <= n.get_nunit(); ++unit_counter){
            for (unsigned int area_counter = 1; area_counter <= n.get_narea(); ++area_counter){
                for (unsigned int iter_counter = 1; iter_counter <= n.get_niter(); ++iter_counter){
                    biol.n()(quant_counter, year, unit_counter, season, area_counter, iter_counter) = Value(n(quant_counter, 1, unit_counter, 1, area_counter, iter_counter));
                    for (unsigned int fishery_counter = 1; fishery_counter <= fisheries.get_nfisheries(); ++fishery_counter){
                        fisheries(fishery_counter)(1).landings_n()(quant_counter, year, unit_counter, season, area_counter, iter_counter) = Value(landings_n(quant_counter, 1, unit_counter, 1, area_counter, iter_counter, fishery_counter));
                        fisheries(fishery_counter)(1).discards_n()(quant_counter, year, unit_counter, season, area_counter, iter_counter) = Value(discards_n(quant_counter, 1, unit_counter, 1, area_counter, iter_counter, fishery_counter));
                        f(quant_counter, year, unit_counter, season, area_counter, iter_counter,fishery_counter) = Value(fad(quant_counter, 1, unit_counter, 1, area_counter, iter_counter, fishery_counter));
                    }
    }}}}
}

void operatingModel::update_from_ad_members(const int timestep){
    int year;
    int season;
    timestep_to_year_season(timestep, biol.n().get_nseason(), year, season);
    update_from_ad_members(year, season);
}

/*
// the timestep that fmult affects to calculate the target value
int operatingModel::get_target_fmult_timestep(const int target_no){
    fwdControlTargetType target_type = ctrl.get_target_type(target_no);
    int target_year = ctrl.get_target_year(target_no);
    int target_season = ctrl.get_target_season(target_no);
    int target_timestep = 0;
    year_season_to_timestep(target_year, target_season, biol.n(), target_timestep);
    if((target_type == target_ssb) || (target_type == target_biomass)){
        target_timestep = target_timestep - 1;
    }
    return target_timestep;
}
*/

/*
// Currently only for a single biol and a single catch
// Updates catch in timestep and biol in timestep+1
// Timestep is based on year / season
// The components of an operatingNodel (Biol and Fisheries) should all have the same time dims
void operatingModel::project_timestep(const int timestep, const int min_iter, const int max_iter){

//clock_t start_time;
//clock_t end_time;
//clock_t clk1;
//clock_t clk2;
//clock_t clk3;
//
//
//start_time = clock();

    // Check iters
    if (min_iter < 1){
        Rcpp::stop("project_timestep: min_iter is less than 1\n");
    }
    if (max_iter > biol.n().get_niter()){
        Rcpp::stop("project_timestep: max_iter is greater than number of iters\n");
    }
    // In preparation for multiple fisheries and catches!
    int fishery_count = 1;
    int catches_count = 1;
    int biol_no = 1;
    // Time placeholders
    int year = 0;
    int season = 0;
    int next_year = 0;
    int next_season = 0;
    int ssb_year = 0;
    int ssb_season = 0;
    timestep_to_year_season(timestep, biol.n(), year, season);
    timestep_to_year_season(timestep+1, biol.n(), next_year, next_season); 
    // Check that timestep does not exceed biol
    if (next_year > biol.n().get_nyear()){
        Rcpp::stop("In project_timestep. Trying to access year larger than biol years.\n");
    }
    adouble rec_temp = 0.0;
    adouble catch_temp = 0.0;
    adouble z = 0.0;
    adouble ssb_temp = 0.0;
    FLQuantAdolc ssb_flq;
    const int max_quant = f(1).get_nquant();
    FLQuantAdolc discards_ratio_temp = fisheries(fishery_count)(catches_count).discards_ratio();

    // Loop over iters - or should we do iter in Run loop?
    for (int iter_count = min_iter; iter_count <= max_iter; ++iter_count){
        // Calculate the landings and discards
        // quant_count is over the first dimension which is age
        // for looping when could add FLQuant - but need to subset by year and season
        for (int quant_count = 1; quant_count <= max_quant; ++quant_count){
            z =  f(quant_count, year, 1, season, 1, iter_count, 1) + biol.m()(quant_count, year, 1, season, 1, iter_count);
            catch_temp = (f(quant_count, year, 1, season, 1, iter_count, 1) / z) * (1 - exp(-z)) * biol.n()(quant_count, year, 1, season, 1, iter_count);
            fisheries(fishery_count)(catches_count).landings_n()(quant_count, year, 1, season, 1, iter_count) = (1 - discards_ratio_temp(quant_count, year, 1, season, 1, iter_count)) * catch_temp;
            fisheries(fishery_count)(catches_count).discards_n()(quant_count, year, 1, season, 1, iter_count) = discards_ratio_temp(quant_count, year, 1, season, 1, iter_count) * catch_temp;
        }
    }
    // Get SSB - all iters
    //ssb_flq = ssb(biol_no);
    // Get the year and season of the SSB that results in recruitment in the next timestep
    // Calculate the SSB that will lead to the recruitment in the NEXT TIME STEP, e.g. current SSB 
    // Add one to timestep because we are getting the recruitment in timestep+1
    timestep_to_year_season(timestep - biol.srr.get_timelag() + 1, biol.n(), ssb_year, ssb_season);

    for (int iter_count = min_iter; iter_count <= max_iter; ++iter_count){
        // Update population
        //ssb_temp = ssb(timestep - biol.srr.get_timelag() + 1, 1, 1, iter_count, biol_no);  
        //ssb_temp = ssb_flq(1,ssb_year,1,ssb_season,1,iter_count);
        ssb_temp = 1000000;
        // Get the recruitment - year and season passed in for time varying SRR parameters
        rec_temp = biol.srr.eval_model(ssb_temp, next_year, 1, next_season, 1, iter_count);

        // Apply the residuals
        // Use of if statement is OK because for each taping it will only branch the same way (depending on residuals_mult)
        if (biol.srr.get_residuals_mult()){
            rec_temp = rec_temp * biol.srr.get_residuals()(1,next_year,1,next_season,1,iter_count);
        }
        else{
            rec_temp = rec_temp + biol.srr.get_residuals()(1,next_year,1,next_season,1,iter_count);
        }
        biol.n()(1, next_year, 1, next_season, 1, iter_count) = rec_temp;

        for (int quant_count = 1; quant_count < max_quant; ++quant_count){
            z =  f(quant_count, year, 1, season, 1, iter_count,1) + biol.m()(quant_count, year, 1, season, 1, iter_count);
            biol.n()(quant_count+1, next_year, 1, next_season, 1, iter_count) = biol.n()(quant_count, year, 1, season, 1, iter_count) * exp(-z);
        }
        // Assume the last age is a plus group
        z =  f(max_quant, year, 1, season, 1, iter_count, 1) + biol.m()(max_quant, year, 1, season, 1, iter_count);
        biol.n()(max_quant, next_year, 1, next_season, 1, iter_count) = biol.n()(max_quant, next_year, 1, next_season, 1, iter_count) + (biol.n()(max_quant, year, 1, season, 1, iter_count) * exp(-z));
    }


//end_time = clock();
//Rprintf("Time in project: %f\n", (float)(end_time - start_time)/CLOCKS_PER_SEC);

    return;
}
*/



/*
// Returns the indices of the age range, starts at 0
// biol_no not used yet
Rcpp::IntegerVector operatingModel::get_target_age_range_indices(const int target_no, const int biol_no) const {
    Rcpp::IntegerVector age_range = ctrl.get_age_range(target_no);
    Rcpp::IntegerVector age_range_indices(2);
    // Convert the age names to a vector of strings
    std::vector<std::string> age_names = Rcpp::as<std::vector<std::string> >(biol.n().get_dimnames()[0]);
    // Use find() to match names - precheck in R that they exist - if not find, returns the last
    std::vector<string>::iterator age_min_iterator = find(age_names.begin(), age_names.end(), number_to_string(age_range[0]));
    if(age_min_iterator != age_names.end()){
        age_range_indices[0] = std::distance(age_names.begin(), age_min_iterator);
    }
    else {
        Rcpp::stop("min_age in control not found in dimnames of FLBiol\n");
    }
    std::vector<string>::iterator age_max_iterator = find(age_names.begin(), age_names.end(), number_to_string(age_range[1]));
    if(age_max_iterator != age_names.end()){
        age_range_indices[1] = std::distance(age_names.begin(), age_max_iterator);
    }
    else {
        Rcpp::stop("max_age in control not found in dimnames of FLBiol\n");
    }
    return age_range_indices;
}
*/


/*
FLQuantAdolc operatingModel::eval_target(const int target_no) const {
    // Scrape biol, fishery and catch no from the control object
    // For the time being, assume that biol_no = 1, and fishery and catch are NA (i.e. all)
    const int biol_no = 1;
    fwdControlTargetType target_type = ctrl.get_target_type(target_no);
    Rcpp::IntegerVector age_range_indices;
    FLQuantAdolc out;
    // Get the output depending on target type
    switch(target_type){
        case target_f:
            // If no fishery in the control object get total fbar on biol
            //if (Rcpp::IntegerVector::is_na(fishery_no)){
            age_range_indices = get_target_age_range_indices(target_no, biol_no);
            out = fbar(age_range_indices, biol_no);
            //}
            //else {
            //    out_flq = fbar(age_range_indices, fishery_no, catch_no, biol_no);
            //}
           break;
        case target_catch:
            // If no fishery in the control object get total fbar on biol
            //if (Rcpp::IntegerVector::is_na(fishery_no)){
            out = catches(biol_no);
            //}
            //else {
            //    out_flq = catches(fishery_no, catch_no, biol_no);
            //}
            break;
        case target_ssb:
            out = ssb(biol_no);
            break;
        case target_biomass:
            out = biomass(biol_no);
            break;
        default:
            Rcpp::stop("target_type not found in switch statement - giving up\n");
            break;
    }
    return out;
}
*/

/*
// Similar to fwdControl::get_target_value but calcs value from relative values
// gets all iters. col: 1 = min, 2 = value, 3 = max
std::vector<double> operatingModel::calc_target_value(const int target_no) const{
    // Pull out the min, value and max iterations from the control object
    std::vector<double> value = ctrl.get_target_value(target_no, 2);
    std::vector<double> min_value = ctrl.get_target_value(target_no, 1);
    std::vector<double> max_value = ctrl.get_target_value(target_no, 3);

    // Are we dealing with absolute or relative values?
    int target_rel_year = ctrl.get_target_rel_year(target_no);
    int target_rel_season = ctrl.get_target_rel_season(target_no);
    // Are rel_year and rel_season NAs or do they have values?
    bool target_rel_year_na = Rcpp::IntegerVector::is_na(target_rel_year);
    bool target_rel_season_na = Rcpp::IntegerVector::is_na(target_rel_season);
    // Both are either NA, or neither are, if one or other is NA then something has gone wrong (XOR)
    if ((target_rel_year_na ^ target_rel_season_na)){
        Rcpp::stop("in operatingModel::calc_target_value. Only one of rel_year or rel_season is NA. Must be neither or both.\n");
    }
    // If target is relative we have to calc the value
    if (!target_rel_year_na){
        Rprintf("Relative target\n");
        // Get the value we are relative to from the operatingModel
        FLQuantAdolc rel_value = eval_target(target_no);
        // Modify it by the relative amount
        double rel_single_value = 0.0;
        for (unsigned int iter_count = 0; iter_count < value.size(); ++iter_count){
            rel_single_value = rel_value(1,target_rel_year,1,target_rel_season, 1, iter_count+1).value();
            value[iter_count] = value[iter_count] * rel_single_value;
            min_value[iter_count] = min_value[iter_count] * rel_single_value;
            max_value[iter_count] = max_value[iter_count] * rel_single_value;
        }
    }
    // Sort out minimum and maximum stuff
    int target_year = ctrl.get_target_year(target_no);
    int target_season = ctrl.get_target_season(target_no);
    // If we have minimum and maximum values then we shouldn't have values (values == NA)
    // If values are NA, then calculate them from the operatingModel
    // As all iterations should be either NA or a real value, just check the first iteration
    if(Rcpp::NumericVector::is_na(value[0])){
        // Annoyingly eval_target returns adouble, so we need to take the value
        FLQuantAdolc value_ad = eval_target(target_no);
        for (unsigned int iter_count = 0; iter_count < value.size(); ++iter_count){
            value[iter_count] = value_ad(1, target_year, 1, target_season, 1, iter_count+1).value();
        }
    }
    // If first iter of min_value is NA, then all of them are
    if(!Rcpp::NumericVector::is_na(min_value[0])){ 
    Rprintf("Minimum target set\n");
    // Update each iter accordingly
        for (unsigned int iter_count = 0; iter_count < value.size(); ++iter_count){
            if(value[iter_count] < min_value[iter_count]){
                value[iter_count] = min_value[iter_count];
            }
        }
    }
    // If first iter of max_value is NA, then all of them are
    if(!Rcpp::NumericVector::is_na(max_value[0])){ 
    Rprintf("Maximum target set\n");
    // Update each iter accordingly
        for (unsigned int iter_count = 0; iter_count < value.size(); ++iter_count){
            if(value[iter_count] > max_value[iter_count]){
                value[iter_count] = max_value[iter_count];
            }
        }
    }
    return value;
}
*/

void operatingModel::run(){

    Rprintf("In run\n");

    /*
    const int ntarget = ctrl.get_ntarget();
    const int niter = ctrl.get_niter(); // number of iters taken from control object - not from Biol or Fisheries
    int target_year = 0;
    int target_season = 0;
    int target_timestep = 0;
    // The timestep of F that we need to multiply to get the target.
    // e.g. target is 'catch', target_fmult_timestep is the same as the timestep in the control object
    // if target is 'biomass' target_fmult_timestep will be the year before the timestep in the control object
    int target_fmult_timestep = 0;
    int target_fmult_year = 0;
    int target_fmult_season = 0;

    // independent variables
    double fmult_initial = 1;
    std::vector<double> fmult(1,fmult_initial); // For the solver
    std::vector<adouble> fmult_ad(1,fmult_initial); 
    // dependent variables
    // Just doing 1 iter at a time for the moment 
    std::vector<double> error(1,0.0);
    std::vector<adouble> target_value_hat(1,0.0);
    FLQuantAdolc target_value_hat_flq;
    // from control object
    //double target_value; 
    std::vector<double> target_value(niter, 0.0); 

    int tape_tag = 1;

    int nr_out = 0;

    for (int target_count = 1; target_count <= ntarget; ++target_count){
        Rprintf("\nResolving target: %i\n", target_count);
        // What time step are we hitting this target?
        target_year = ctrl.get_target_year(target_count);
        target_season = ctrl.get_target_season(target_count);
        year_season_to_timestep(target_year, target_season, biol.n(), target_timestep);

        // Get timestep, year, season of which F to adjust
        target_fmult_timestep = get_target_fmult_timestep(target_count);
        timestep_to_year_season(target_fmult_timestep, biol.n(), target_fmult_year, target_fmult_season);

        //Rprintf("target_fmult_year: %i\n", target_fmult_year);
        //Rprintf("target_fmult_season: %i\n", target_fmult_season);
        //Rprintf("target_fmult_timestep: %i\n", target_fmult_timestep);

        // Get the value that we are trying to hit
        // This either comes directly from the control object
        // or is calculated if not a min / max or rel value)
        target_value = calc_target_value(target_count); 

        for (int iter_count = 1; iter_count <= niter; ++iter_count){
            Rprintf("Resolving iter: %i\n", iter_count);

            // Tape the process
            trace_on(tape_tag);
            fmult_ad[0] <<= fmult_initial; // Or move this above iter loop and do all iters at same time
            // Update om.f = om.f * fmult in that year / season
            // Use target_fmult_year / season here
            for (int quant_count = 1; quant_count <= f(1).get_nquant(); ++quant_count){
                f(quant_count,target_fmult_year,1,target_fmult_season,1,iter_count,1) = f(quant_count,target_fmult_year,1,target_fmult_season,1,iter_count,1) * fmult_ad[0];
            }
            // use target_fmult_timestep here
            project_timestep(target_fmult_timestep, iter_count, iter_count); 

            // What is the current value of the predicted target in the operatingModel
            target_value_hat_flq = eval_target(target_count);
            // subset to get all the iters
            target_value_hat[0] = target_value_hat_flq(1,target_year, 1, target_season, 1, iter_count);

            //Rprintf("target_value_hat: %f\n", target_value_hat[0].value());
            //Rprintf("target_value: %f\n", target_value[0]);

            // Calculate the error term that we want to be 0
            //target_value_hat_flq(1,target_year, 1, target_season, 1, iter_count) = (target_value_hat_flq(1,target_year, 1, target_season, 1, iter_count) - target_value[iter_count-1]); // this works better - no need to reset initial fmult each time?
            target_value_hat[0] = (target_value_hat[0] - target_value[iter_count-1]); // this works better - no need to reset initial fmult each time?
            //target_value_hat[0] = (target_value_hat[0] - target_value) * (target_value_hat[0] - target_value); // squared error - less stable

            // Set dependent variable
            target_value_hat[0] >>= error[0];
            //target_value_hat_flq(1,target_year, 1, target_season, 1, iter_count) >>= error[0];
            trace_off();

            // Solve
            // reset initial solver value - also can just use: error = target_hat - target without squaring
            // faster to do this outside of the iter loop and start NR with solution of previous iter - OK until a bad iter
            fmult[0] = 1.0;  // Needs to be rethought. If previous target ends up with high F, this is a bad start
            //Rprintf("fmult pre NR: %f\n", fmult[0]);
            nr_out = newton_raphson(fmult, tape_tag, 200, 1000);
            // Run some check on this


            //Rprintf("fmult post NR: %f\n", fmult[0]);
            // Test output code for what happened
            // Correct values are now in fmult
            // Project with these values
            for (int quant_count = 1; quant_count <= f(1).get_nquant(); ++quant_count){
                //f(quant_count,target_year,1,target_season,1,iter_count,1) = f(quant_count,target_year,1,target_season,1,iter_count,1) * fmult[0];
                f(quant_count,target_fmult_year,1,target_fmult_season,1,iter_count,1) = f(quant_count,target_fmult_year,1,target_fmult_season,1,iter_count,1) * fmult[0];
            }
            project_timestep(target_fmult_timestep, iter_count, iter_count); 
            // We're done!
        }

    }
*/
}


/*
void operatingModel::run_all_iters(){

    const int ntarget = ctrl.get_ntarget();
    const int niter = ctrl.get_niter(); // number of iters taken from control object - not from Biol or Fisheries
    int target_year = 0;
    int target_season = 0;
    int target_timestep = 0;
    // The timestep of F that we need to multiply to get the target.
    // e.g. target is 'catch', target_fmult_timestep is the same as the timestep in the control object
    // if target is 'biomass' target_fmult_timestep will be the year before the timestep in the control object
    int target_fmult_timestep = 0;
    int target_fmult_year = 0;
    int target_fmult_season = 0;

    // independent variables
    double fmult_initial = 1;
    std::vector<double> fmult(niter,fmult_initial); // For the solver
    std::vector<adouble> fmult_ad(niter,fmult_initial); 
    // dependent variables
    std::vector<double> error(niter,0.0);
    std::vector<adouble> target_value_hat(niter,0.0);
    FLQuantAdolc target_value_hat_flq;
    std::vector<double> target_value(niter, 0.0); 

    int tape_tag = 1;
    int nr_out = 0;


clock_t clk1;
clock_t clk2;
    for (int target_count = 1; target_count <= ntarget; ++target_count){
clk1 = clock();
        Rprintf("\nResolving target: %i\n", target_count);

        // What time step are we hitting this target?
        target_year = ctrl.get_target_year(target_count);
        target_season = ctrl.get_target_season(target_count);
        year_season_to_timestep(target_year, target_season, biol.n(), target_timestep);
        // Get timestep, year, season of which F to adjust
        target_fmult_timestep = get_target_fmult_timestep(target_count);
        timestep_to_year_season(target_fmult_timestep, biol.n(), target_fmult_year, target_fmult_season);
        Rprintf("target_fmult_year: %i\n", target_fmult_year);
        Rprintf("target_fmult_season: %i\n", target_fmult_season);
        Rprintf("target_fmult_timestep: %i\n", target_fmult_timestep);

        // Get the value that we are trying to hit
        // This either comes directly from the control object
        // or is calculated if not a min / max or rel value)
        target_value = calc_target_value(target_count); 

        // Tape the process
        trace_on(tape_tag);

        // Update om.f = om.f * fmult in that year / season
        for (int iter_count = 0; iter_count < niter; ++ iter_count){
            // Set independent variable
            fmult_ad[iter_count] <<= fmult_initial;  
            // Update f with fmult
            for (int quant_count = 1; quant_count <= f(1).get_nquant(); ++quant_count){
                f(quant_count,target_fmult_year,1,target_fmult_season,1,iter_count+1,1) = f(quant_count,target_fmult_year,1,target_fmult_season,1,iter_count+1,1) * fmult_ad[iter_count];
            }
        }

        // use target_fmult_timestep here
        project_timestep(target_fmult_timestep, 1, niter); 

        // What is the current value of the predicted target in the operatingModel
        target_value_hat_flq = eval_target(target_count);
        //Rprintf("target_value_hat: %f\n", target_value_hat[0].value());
        //Rprintf("target_value: %f\n", target_value[0]);

        // Calculate the error term that we want to be 0
        // And set the dependent variable
        for (int iter_count = 0; iter_count < niter; ++ iter_count){
            (target_value_hat_flq(1,target_year, 1, target_season, 1, iter_count+1) - target_value[iter_count]) >>= error[iter_count];
        }
        trace_off();

        // Solve
        // reset initial solver value - also can just use: error = target_hat - target without squaring
        // faster to do this outside of the iter loop and start NR with solution of previous iter - OK until a bad iter
        //fmult[0] = 1.0;  // Needs to be rethought. If previous target ends up with high F, this is a bad start
        //Rprintf("fmult pre NR: %f\n", fmult[0]);
//        for (int iter_count = 0; iter_count < niter; ++ iter_count){
//            fmult[iter_count] = 1.0;
//        }

clk2 = clock();
Rprintf("Tape and project: %f\n", (float)(clk2 - clk1)/CLOCKS_PER_SEC);
        std::fill(fmult.begin(), fmult.end(), 1.0);

clk1 = clock();
        nr_out = newton_raphson(fmult, tape_tag, 200, 1000);
        // Run some check on this
clk2 = clock();
Rprintf("Time in NR: %f\n", (float)(clk2 - clk1)/CLOCKS_PER_SEC);
        //Rprintf("fmult post NR: %f\n", fmult[0]);
        // Test output code for what happened
        // Correct values are now in fmult
        // Project with these values
        for (int iter_count = 0; iter_count < niter; ++ iter_count){
            for (int quant_count = 1; quant_count <= f(1).get_nquant(); ++quant_count){
                f(quant_count,target_fmult_year,1,target_fmult_season,1,iter_count+1,1) = f(quant_count,target_fmult_year,1,target_fmult_season,1,iter_count+1,1) * fmult[iter_count];
            }
        }
        project_timestep(target_fmult_timestep, 1, niter); 
        // We're done!
    }
}
*/

//---------------Target methods ----------------------------

/*
// Total biomass at the beginning of the timestep
// biol_no not currently used
FLQuantAdolc operatingModel::biomass(const int biol_no) const {
    FLQuantAdolc biomass = quant_sum(biol.n() * biol.wt());
    return biomass;
}

// SSB calculations - Actual SSB that results in recruitment
// Return an FLQuant
// biol_no not currently used
FLQuantAdolc operatingModel::ssb(const int biol_no) const {
    // Loop over all the Fs that catch the biol
    FLQuantAdolc f_portion = f(1) * f_spwn(1);
    for (unsigned int f_count = 2; f_count <= f.get_ndim7(); ++f_count){
        f_portion = f_portion + f(f_count) * f_spwn(f_count);
    }
//clock_t clk1 = clock();
//    FLQuantAdolc ssb_part = biol.n() * biol.wt() * biol.fec() * exp(-1.0*(biol.m() * biol.spwn() + f_portion));
//clock_t clk2 = clock();
    FLQuantAdolc ssb = quant_sum(biol.wt() * biol.fec() * exp(-1.0*(biol.m() * biol.spwn() + f_portion)) * biol.n());
//    Rprintf("ssb part: %f\n", (float)(clk2 - clk1)/CLOCKS_PER_SEC);
// AD * D takes longer than D * D
// Goes from left to right
    return ssb;
}

// Return all iterations but single timestep as an FLQuant
FLQuantAdolc operatingModel::ssb(const int timestep, const int unit, const int area, const int biol_no) const {
    FLQuantAdolc full_ssb = ssb(biol_no);
    int year = 0;
    int season = 0;
    timestep_to_year_season(timestep, full_ssb, year, season);
    FLQuantAdolc out = full_ssb(1,1,year,year,unit,unit,season,season,area,area,1,full_ssb.get_niter());
    return out;
}

// Return a single value given timestep, unit, area and iter
adouble operatingModel::ssb(const int timestep, const int unit, const int area, const int iter, const int biol_no) const {
    FLQuantAdolc full_ssb = ssb(biol_no);
    int year = 0;
    int season = 0;
    timestep_to_year_season(timestep, full_ssb, year, season);
    adouble out = full_ssb(1,year,unit,season,area,iter);
    return out;
}

// Return a single value given year, season, unit, area and iter
adouble operatingModel::ssb(const int year, const int unit, const int season, const int area, const int iter, const int biol_no) const {
    FLQuantAdolc full_ssb = ssb(biol_no);
    adouble out = full_ssb(1,year,unit,season,area,iter);
    return out;
}

FLQuantAdolc operatingModel::fbar(const Rcpp::IntegerVector age_range_indices, const int fishery_no, const int catch_no, const int biol_no) const{
    Rcpp::IntegerVector fdim = f(fishery_no).get_dim();
    FLQuantAdolc f_age_trim = f(fishery_no)(age_range_indices[0]+1, age_range_indices[1]+1, 1, fdim[1], 1, fdim[2], 1, fdim[3], 1, fdim[4], 1, fdim[5]);  // subsetting
    FLQuantAdolc fbar_out = quant_mean(f_age_trim);
    return fbar_out;

}

// Assume that catch is catches[[1]] for the moment
FLQuantAdolc operatingModel::fbar(const Rcpp::IntegerVector age_range_indices, const int biol_no) const{
    //// Make an empty FLQ with the right dims - based on the first fishery
    FLQuantAdolc fbar_out = fbar(age_range_indices, 1,1,biol_no);
    for (unsigned int fishery_count = 2; fishery_count <= fisheries.get_nfisheries(); ++fishery_count){
        fbar_out = fbar_out + fbar(age_range_indices, fishery_count,1,biol_no);
    }
    return fbar_out;
}

// Catch of a particular fishery
FLQuantAdolc operatingModel::catches(const int fishery_no, const int catch_no, const int biol_no) const{
    return fisheries(fishery_no, catch_no).catches();
}

// Total catch from an FLBiol
// Assumes the catch is the first FLCatch in the FLFishery
FLQuantAdolc operatingModel::catches(const int biol_no) const{
    // Get the catch from the first fishery
    FLQuantAdolc catches_out = catches(1,1,biol_no);
    for (unsigned int fishery_count = 2; fishery_count <= fisheries.get_nfisheries(); ++fishery_count){
        catches_out = catches_out + catches(fishery_count,1,biol_no);
    }
    return catches_out;
}
*/


/*----------------------------------------------------------------------------------*/

// Assumes the targets are already ordered by time

// [[Rcpp::export]]
operatingModel test_run(const FLFisheries fisheries, SEXP FLBiolSEXP, const std::string srr_model_name, const FLQuant srr_params, const FLQuant srr_residuals, const bool srr_residuals_mult, const int srr_timelag, FLQuant7 f, FLQuant7 f_spwn, fwdControl ctrl){

    // Make the fwdBiol from the FLBiol and SRR bits
    fwdBiol biol(FLBiolSEXP, srr_model_name, srr_params, srr_timelag, srr_residuals, TRUE); 
    // Make the OM
    operatingModel om(fisheries, biol, f, f_spwn, ctrl);

    om.run();

    return om;

}


/*
// [[Rcpp::export]]
operatingModel test_run_all_iters(const FLFisheriesAdolc fisheries, SEXP FLBiolSEXP, const std::string srr_model_name, const FLQuant srr_params, const FLQuant srr_residuals, const bool srr_residuals_mult, const int srr_timelag, FLQuant7Adolc f, FLQuant7 f_spwn, fwdControl ctrl){

    // Make the fwdBiol from the FLBiol and SRR bits
    fwdBiolAdolc biol(FLBiolSEXP, srr_model_name, srr_params, srr_timelag, srr_residuals, TRUE); 
    // Make the OM
    operatingModel om(fisheries, biol, f, f_spwn, ctrl);

    om.run_all_iters();

    return om;

}

// [[Rcpp::export]]
int get_data_element_speed_test1(const FLQuant flq, const int quant, const int year, const int unit, const int season, const int area, int iter){
    return flq.get_data_element(quant, year, unit, season, area, iter);

}

// [[Rcpp::export]]
int get_data_element_speed_test2(const FLQuant flq, const int quant, const int year, const int unit, const int season, const int area, int iter){
    return flq.get_data_element2(quant, year, unit, season, area, iter);

}
*/
