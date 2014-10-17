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

/*
double euclid_norm(double* x, const int size_x){
    double xsum = 0;
    for (int i=0; i<size_x; ++i){
        xsum += x[i] * x[i];
    }
    xsum = sqrt(xsum);
    return xsum;
}
*/

double euclid_norm(std::vector<double> x){
    double xsum = std::inner_product(x.begin(), x.end(), x.begin(), 0.0); // must be 0.0 else automatically casts to int (argh)
    xsum = sqrt(xsum);
    return xsum;
}






// We should offer the option of doing iterations in chunks - i.e. if 5000 iters, do 1000 at a time else Jacobian becomes massive and we hit memory problems
// And we need to make sure that solving for multiple targets (e.g. if two fleets and we have two fmults) works
// So we pass in the number of iterations we want to solve, and how many simultaneous targets there in that iteration, i.e. the dimension of the problem
// As each iteration is indpendent, we solve each iteration of simultaneous targets separately
// find x: f(x) = 0
// x1 = x0 - f(x0) / f'(x0)
// w = f(x0) / f'(x0)
// We want w.
// Rearrange w:
// f'(x0) w = f(x0)
// We use LU solve, give it f'(x0) and f(x0) to get w
// x1 = x0 - w
int newton_raphson(std::vector<double>& indep, CppAD::ADFun<double>& fun, const int niter, const int nsim_targets, const int max_iters, const double max_limit, const double tolerance){
    // Check that product of niter and nsim_targets = length of indep (otherwise something has gone wrong)
    if (indep.size() != (niter * nsim_targets)){
        Rcpp::stop("In newton_raphson: length of indep does not equal product of niter and nsim_targets\n");
    }

    double logdet = 0.0;
    std::vector<double> y(niter * nsim_targets, 1000.0);
    std::vector<double> delta_indep(niter * nsim_targets, 0.0); // For updating indep in final step
    std::vector<double> jac(niter * nsim_targets * niter * nsim_targets);
    std::vector<double> iter_jac(nsim_targets * nsim_targets);
    std::vector<double> iter_y(nsim_targets);
    std::vector<int> iter_solved(niter, 0); // If 0, that iter has not been solved

    int jac_element = 0; // an index for Jacobian used for subsetting the Jacobian for each iter
    int nr_count = 0;
    int reason_for_stopping = 0; // 0 - tolerance met; 1 - iter limit reached; 2 - max limit reached

    // Test all iters have been solved
    while((std::accumulate(iter_solved.begin(), iter_solved.end(), 0) < niter) & (nr_count < max_iters)){ 
    //for (nr_count = 0; nr_count < 10; ++nr_count){
        ++nr_count;
        //Rprintf("\nnr_count: %i\n", nr_count);
        // Get y = f(x0)
        y = fun.Forward(0, indep); // HERE - why is y changing
        // Get f'(x0)
        jac = fun.SparseJacobian(indep);
        // Get w (f(x0) / f'(x0)) for each iteration if necessary
        // Loop over iters, solving if necessary
        for (int iter_count = 0; iter_count < niter; ++iter_count){
            //Rprintf("iter_count: %i\n", iter_count);
            // Only solve if that iter has not been solved
            if(iter_solved[iter_count] == 0){
                //Rprintf("Iter %i not yet solved\n", iter_count);
                // Subsetting y and Jacobian for that iter only
                for(int jac_count_row = 0; jac_count_row < nsim_targets; ++jac_count_row){
                    iter_y[jac_count_row] = y[iter_count * nsim_targets + jac_count_row];
                    // Fill up mini Jacobian for that iteration 
                    for(int jac_count_col = 0; jac_count_col < nsim_targets; ++jac_count_col){
                        jac_element = (iter_count * niter * nsim_targets * nsim_targets) + (iter_count * nsim_targets) + jac_count_row + (jac_count_col * niter * nsim_targets);
                        iter_jac[jac_count_row + (jac_count_col * nsim_targets)] = jac[jac_element];
                    }
                }
                // Solve to get w = f(x0) / f'(x0)
                // Puts resulting w into iter_y
                CppAD::LuSolve(nsim_targets, nsim_targets, iter_jac, iter_y, iter_y, logdet); 
                //Rprintf("iter_y[0] %f\n", iter_y[0]*1e12);
                // Has iter now been solved? If so, set the flag to 1
                if (euclid_norm(iter_y) < tolerance){
                    //Rprintf("iter %i now solved\n", iter_count);
                    //Rprintf("euclid_norm * 1e12: %f\n", euclid_norm(iter_y)*1e12);
                    iter_solved[iter_count] = 1;
                }
                // put iter_y back into delta_indep - needs for loop
                for(int jac_count = 0; jac_count < nsim_targets; ++jac_count){
                    delta_indep[iter_count * nsim_targets + jac_count] = iter_y[jac_count];
                }
            }
        }
        //CppAD::LuSolve(niter * nsim_targets, niter * nsim_targets, jac, y, y, logdet); 

        // Update x = x - w
        // Ideally should only update the iterations that have not hit the tolerance
        std::transform(indep.begin(),indep.end(),delta_indep.begin(),indep.begin(),std::minus<double>());
    }
    return reason_for_stopping;
}


/*------------------------------------------------------------*/
// operatingModel class

// Empty constructor
operatingModel::operatingModel(){
    biol = fwdBiolAD();
    fisheries = FLFisheriesAD();
    f = FLQuant7AD();
    f_spwn = FLQuant7();
    //landings_n = FLQuant7AD();
    //discards_n = FLQuant7AD();
    //fad = FLQuant7AD();
    //n = FLQuantAD();
}

// Main constructor
operatingModel::operatingModel(const FLFisheriesAD fisheries_in, const fwdBiolAD biol_in, const FLQuant7AD f_in, const FLQuant7 f_spwn_in, const fwdControl ctrl_in){
    // Checking dims (1 - 5) of landings slots, F and biol are the same
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
    //n = FLQuantAD(biol_dim[0], 1, biol_dim[2], 1, biol_dim[4], biol_dim[5]); // Assumes that the number of iters in the biol is the same as all objects - is that right?
    //landings_n = FLQuant7AD(n); // Add first FLQ to the list
    //// Keep adding more elements to the list - one for each fishery
    //for (unsigned int fishery_counter = 2; fishery_counter <= nfisheries; ++fishery_counter){
    //    landings_n(n); // push back
    //} 
    //discards_n = landings_n;
    //fad = landings_n;
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
    return Rcpp::List::create(
                            Rcpp::Named("biol", biol),
                            Rcpp::Named("fisheries", fisheries),
                            Rcpp::Named("f", f),
                            Rcpp::Named("f_spwn", f_spwn),
                            Rcpp::Named("ctrl", ctrl));
                            //Rcpp::Named("landings_n", landings_n),
                            //Rcpp::Named("discards_n", discards_n),
                            //Rcpp::Named("fad", fad),
                            //Rcpp::Named("n", n));
}


// The timestep that fmult affects to calculate the target value
// e.g. if Biomass is target, then adjust the fmult in the previous timestep
// Add more target types to it
int operatingModel::get_target_fmult_timestep(const int target_no){
    fwdControlTargetType target_type = ctrl.get_target_type(target_no);
    int target_year = ctrl.get_target_year(target_no);
    int target_season = ctrl.get_target_season(target_no);
    int target_timestep = 0;
    year_season_to_timestep(target_year, target_season, biol.n(), target_timestep);
    // Biomass timesteps
    if((target_type == target_ssb) ||
       (target_type == target_biomass)){
        target_timestep = target_timestep - 1;
    }
    return target_timestep;
}


void operatingModel::project_timestep(const int timestep){
    Rprintf("In project\n");
    int year = 0;
    int season = 0;
    int next_year = 0;
    int next_season = 0;
    int ssb_year = 0;
    int ssb_season = 0;
    timestep_to_year_season(timestep, biol.n(), year, season);
    timestep_to_year_season(timestep+1, biol.n(), next_year, next_season); 

    // Dims for getting time slices
    Rcpp::IntegerVector biol_dim = biol.m().get_dim();
    // CAREFUL WITH NUMBER OF ITERS
    unsigned int niter = biol_dim[5];
    unsigned int unit = 1;
    unsigned int area = 1;
    unsigned int biol_no = 1;

    // timestep checks
    if ((year > biol_dim[1]) | (season > biol_dim[3])){
        Rcpp::stop("project_timestep: timestep outside of range");
    }

    // Total F from all fisheries - just get a timestep to reduce memory overload
    FLQuantAD total_f = f(1)(1, biol_dim[0], year, year, 1, biol_dim[2], season, season, 1, biol_dim[4], 1, biol_dim[5]);
    for (int fisheries_count = 2; fisheries_count <= fisheries.get_nfisheries(); ++ fisheries_count){
        total_f += f(fisheries_count)(1, biol_dim[0], year, year, 1, biol_dim[2], season, season, 1, biol_dim[4], 1, biol_dim[5]);
    }
    // Total mortality on the biol (adjust when multiple biols)
    FLQuantAD z = biol.m()(1, biol_dim[0], year, year, 1, biol_dim[2], season, season, 1, biol_dim[4], 1, biol_dim[5]) + total_f;

    // Get landings and discards for each fishery
    CppAD::AD<double> catch_temp;
    CppAD::AD<double> discards_ratio_temp; // DN / (DN + LN)
    CppAD::AD<double> landings_n_temp; 
    CppAD::AD<double> discards_n_temp; 
    unsigned int max_quant = biol_dim[0];
    for (int fisheries_count = 1; fisheries_count <= fisheries.get_nfisheries(); ++ fisheries_count){
        for (int iter_count = 1; iter_count <= niter; ++iter_count){
            for (int quant_count = 1; quant_count <= max_quant; ++quant_count){
                // values in current timestep are used to calculate the discards ratio of that timestep, and then overwritten
                landings_n_temp = fisheries(fisheries_count)(1).landings_n()(quant_count, year, unit, season, area, iter_count);
                discards_n_temp = fisheries(fisheries_count)(1).discards_n()(quant_count, year, unit, season, area, iter_count);
                discards_ratio_temp = discards_n_temp / (discards_n_temp + landings_n_temp);
                catch_temp = (f(fisheries_count)(quant_count, year, unit, season, area, iter_count) / z(quant_count, 1, 1, 1, 1, iter_count)) * (1.0 - exp(-1.0 * z(quant_count, 1, 1, 1, 1, iter_count))) * biol.n()(quant_count, year, unit, season, area, iter_count);
                fisheries(fisheries_count)(1).landings_n()(quant_count, year, unit, season, area, iter_count) = (1.0 - discards_ratio_temp) * catch_temp;
                fisheries(fisheries_count)(1).discards_n()(quant_count, year, unit, season, area, iter_count) = discards_ratio_temp * catch_temp;
            }
        }
    }


    // Update biol in next timestep only if within time range
    if ((next_year <= biol_dim[1]) & (next_season <= biol_dim[3])){
        // Get the year and season of the SSB that will result in recruitment in the next timestep
        // Add one to timestep because we are getting the recruitment in timestep+1
        int ssb_timestep = timestep - biol.srr.get_timelag() + 1;
        if (ssb_timestep < 1){
            Rcpp::stop("project_timestep: ssb timestep outside of range");
        }
        timestep_to_year_season(ssb_timestep, biol.n(), ssb_year, ssb_season);
        // Get SSB - all iters and years etc - could be more efficient
        FLQuantAD ssb_flq= ssb(biol_no);
        // Then update biol in next timestep
        // next n = current n * exp(-z)
        // Check if timestep is the final timestep - if so, don't update biol
        adouble rec_temp = 0.0;
        adouble ssb_temp = 0.0;
        for (int iter_count = 1; iter_count <= niter; ++iter_count){
            // Recruitment
            // rec is calculated in a scalar way - i.e. not passing it a vector of SSBs, so have to do one iter at a time
            ssb_temp = ssb_flq(1,ssb_year,1,ssb_season,1,iter_count);
            //Rprintf("year: %i, season: %i, ssb_year: %i, ssb_season: %i, ssb_temp: %f\n", year, season, ssb_year, ssb_season, Value(ssb_temp));
            rec_temp = biol.srr.eval_model(ssb_temp, next_year, 1, next_season, 1, iter_count);
            // Apply the residuals to rec_temp
            // Use of if statement is OK because for each taping it will only branch the same way (depending on residuals_mult)
            if (biol.srr.get_residuals_mult()){
                rec_temp = rec_temp * biol.srr.get_residuals()(1,next_year,1,next_season,1,iter_count);
            }
            else{
                rec_temp = rec_temp + biol.srr.get_residuals()(1,next_year,1,next_season,1,iter_count);
            }
            biol.n()(1, next_year, 1, next_season, 1, iter_count) = rec_temp;
            for (int quant_count = 1; quant_count < max_quant; ++quant_count){
                biol.n()(quant_count+1, next_year, 1, next_season, 1, iter_count) = biol.n()(quant_count, year, 1, season, 1, iter_count) * exp(-z(quant_count,1,1,1,1,iter_count));
            }
            // plus group - assume last age is a plusgroup
            biol.n()(max_quant, next_year, 1, next_season, 1, iter_count) = biol.n()(max_quant, next_year, 1, next_season, 1, iter_count) + (biol.n()(max_quant, year, 1, season, 1, iter_count) * exp(-z(max_quant, 1, 1, 1, 1, iter_count)));
        }
    }

    Rprintf("Leaving project_timestep\n");
    return; 
}


// Returns the indices of the age range, starts at 0
// biol_no not used yet
Rcpp::IntegerVector operatingModel::get_target_age_range_indices(const int target_no, const int biol_no) const {
    Rcpp::IntegerVector age_range = ctrl.get_age_range(target_no);
    Rcpp::IntegerVector age_range_indices(2);
    // Convert the age names to a vector of strings
    std::vector<std::string> age_names = Rcpp::as<std::vector<std::string> >(biol.n().get_dimnames()[0]);
    // Use find() to match names - precheck in R that they exist - if not find, returns the last
    std::vector<std::string>::iterator age_min_iterator = find(age_names.begin(), age_names.end(), number_to_string(age_range[0]));
    if(age_min_iterator != age_names.end()){
        age_range_indices[0] = std::distance(age_names.begin(), age_min_iterator);
    }
    else {
        Rcpp::stop("minAge in control not found in dimnames of FLBiol\n");
    }
    std::vector<std::string>::iterator age_max_iterator = find(age_names.begin(), age_names.end(), number_to_string(age_range[1]));
    if(age_max_iterator != age_names.end()){
        age_range_indices[1] = std::distance(age_names.begin(), age_max_iterator);
    }
    else {
        Rcpp::stop("maxAge in control not found in dimnames of FLBiol\n");
    }
    return age_range_indices;
}


FLQuantAD operatingModel::eval_target(const int target_no) const {
    // Scrape biol, fishery and catch no from the control object
    // For the time being, assume that biol_no = 1, and fishery and catch are NA (i.e. all)
    const int biol_no = 1;
    fwdControlTargetType target_type = ctrl.get_target_type(target_no);
    Rcpp::IntegerVector age_range_indices;
    FLQuantAD out;
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
        FLQuantAD rel_value = eval_target(target_no);
        // Modify it by the relative amount
        double rel_single_value = 0.0;
        for (unsigned int iter_count = 0; iter_count < value.size(); ++iter_count){
            //rel_single_value = rel_value(1,target_rel_year,1,target_rel_season, 1, iter_count+1).value();
            // if rel single value depends on fmult then we cannot use Value() (http://www.coin-or.org/CppAD/Doc/value.cpp.xml)
            rel_single_value = Value(rel_value(1,target_rel_year,1,target_rel_season, 1, iter_count+1));
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
        FLQuantAD value_ad = eval_target(target_no);
        for (unsigned int iter_count = 0; iter_count < value.size(); ++iter_count){
            //value[iter_count] = value_ad(1, target_year, 1, target_season, 1, iter_count+1).value();
            value[iter_count] = Value(value_ad(1, target_year, 1, target_season, 1, iter_count+1));
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

void operatingModel::run(){

    Rprintf("In run\n");

// No of targets
// Target loop
// No of simultaneous targets in that target ( = 1)
// get timestep of target
// no_fmults = no_sim_targets
// fmults = 1 (grow or shrink fmult as no sim targets changes)
// turn on tape
// f = f * fmult
// calc error = target_value (from control)  - target_hat_value (in object)
//     Need calc_error method
//         target_value method
//         target_hat_value method
// solve
// project again

    const unsigned int ntarget = ctrl.get_ntarget();
    const unsigned int nsim_targets = 1; // Simultaneous targets - fix at 1 for now - will change with target number
    const unsigned int niter = ctrl.get_niter(); // number of iters taken from control object - not from Biol or Fisheries
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
    std::vector<adouble> fmult_ad(niter,fmult_initial); // Length will vary depending on no. simultaneous targets
    std::vector<double> fmult(niter,fmult_initial); // For the solver
    // dependent variables
    //std::vector<double> error(niter,0.0);
    //std::vector<adouble> target_value_hat(niter,0.0);
    FLQuantAD target_value_hat_flq;
    std::vector<double> target_value(niter, 0.0); //  Length will vary depending on no. simultaneous targets
    int nr_out = 0;
    
    // Length of error will vary depending on no. simultaneous targets
    // Initialise with 1 so we pass the first check for NR
    //std::vector<CppAD::AD<double> > error(niter, 1.0); 
    std::vector<adouble> error(niter, 1.0);

    for (int target_count = 1; target_count <= ntarget; ++target_count){
    //int target_count = 1;
    // Get number of simultaneous targets in that target
        Rprintf("\nResolving target: %i\n", target_count);

        // What time step are we hitting this target?
        target_year = ctrl.get_target_year(target_count);
        target_season = ctrl.get_target_season(target_count);
        year_season_to_timestep(target_year, target_season, biol.n(), target_timestep);
        Rprintf("target_year: %i\n", target_year);
        Rprintf("target_season: %i\n", target_season);
        Rprintf("target_timestep: %i\n", target_timestep);
        // Get timestep, year, season of which F to adjust
        target_fmult_timestep = get_target_fmult_timestep(target_count);
        timestep_to_year_season(target_fmult_timestep, biol.n(), target_fmult_year, target_fmult_season);
        Rprintf("target_fmult_year: %i\n", target_fmult_year);
        Rprintf("target_fmult_season: %i\n", target_fmult_season);
        Rprintf("target_fmult_timestep: %i\n", target_fmult_timestep);

        // Turn on tape
        CppAD::Independent(fmult_ad);
        Rprintf("Turned on tape\n");
        // Update F with fmult_ad
        // Update om.f = om.f * fmult in that year / season
        for (int iter_count = 0; iter_count < niter; ++ iter_count){
            for (int quant_count = 1; quant_count <= f(1).get_nquant(); ++quant_count){
                f(quant_count,target_fmult_year,1,target_fmult_season,1,iter_count+1,1) = f(quant_count,target_fmult_year,1,target_fmult_season,1,iter_count+1,1) * fmult_ad[iter_count];
            }
        }

        // use target_fmult_timestep here
        project_timestep(target_fmult_timestep); 

        // Could put all this in a calc_error() method
        // Get the value that we are trying to hit (either comes directly from the control object  or is calculated if not a min / max or rel value)
        target_value = calc_target_value(target_count); 
        // What is the current value of the predicted target in the operatingModel
        target_value_hat_flq = eval_target(target_count);
        // Calculate the error term that we want to be 0
        for (int iter_count = 0; iter_count < niter; ++ iter_count){
            error[iter_count] = (target_value_hat_flq(1,target_year, 1, target_season, 1, iter_count+1) - target_value[iter_count]);
        }

        // Stop recording
        CppAD::ADFun<double> fun(fmult_ad, error);
        Rprintf("Turned off tape\n");

        // Solve
        // reset initial solver value 
        std::fill(fmult.begin(), fmult.end(), 1.0);



        Rprintf("Calling NR\n");
        // Just solves 1 simultaneous target so far
        nr_out = newton_raphson(fmult, fun, niter, 1);
        Rprintf("NR done\n");

        Rprintf("Updating and projecting\n");
        // Update f with new fmult
        // Update F
        for (int iter_count = 1; iter_count <= biol.n().get_niter(); ++iter_count){
            for (int quant_count = 1; quant_count <= biol.n().get_nquant(); ++quant_count){
                f(1)(quant_count, target_fmult_year, 1, target_fmult_season, 1, iter_count) = f(1)(quant_count, target_fmult_year, 1, target_fmult_season, 1, iter_count) * fmult[iter_count-1];
            }
        }
        project_timestep(target_fmult_timestep);


    }


    Rprintf("Leaving run\n");
}



//---------------Target methods ----------------------------

// SSB calculations - Actual SSB that results in recruitment
// Return an FLQuant
// biol_no not currently used
FLQuantAD operatingModel::ssb(const int biol_no) const {
    // Loop over all the Fs that catch the biol
    FLQuantAD f_portion = f(1) * f_spwn(1);
    for (unsigned int f_count = 2; f_count <= f.get_ndim7(); ++f_count){
        f_portion = f_portion + f(f_count) * f_spwn(f_count);
    }
    FLQuantAD ssb = quant_sum(biol.wt() * biol.fec() * exp(-1.0*(biol.m() * biol.spwn() + f_portion)) * biol.n());
    return ssb;
}

// Return all iterations but single timestep as an FLQuant
FLQuantAD operatingModel::ssb(const int timestep, const int unit, const int area, const int biol_no) const {
    FLQuantAD full_ssb = ssb(biol_no);
    int year = 0;
    int season = 0;
    timestep_to_year_season(timestep, full_ssb, year, season);
    FLQuantAD out = full_ssb(1,1,year,year,unit,unit,season,season,area,area,1,full_ssb.get_niter());
    return out;
}

// Return a single value given timestep, unit, area and iter
adouble operatingModel::ssb(const int timestep, const int unit, const int area, const int iter, const int biol_no) const {
    FLQuantAD full_ssb = ssb(biol_no);
    int year = 0;
    int season = 0;
    timestep_to_year_season(timestep, full_ssb, year, season);
    adouble out = full_ssb(1,year,unit,season,area,iter);
    return out;
}

// Return a single value given year, season, unit, area and iter
adouble operatingModel::ssb(const int year, const int unit, const int season, const int area, const int iter, const int biol_no) const {
    FLQuantAD full_ssb = ssb(biol_no);
    adouble out = full_ssb(1,year,unit,season,area,iter);
    return out;
}
// Total biomass at the beginning of the timestep
// biol_no not currently used
FLQuantAD operatingModel::biomass(const int biol_no) const {
    FLQuantAD biomass = quant_sum(biol.n() * biol.wt());
    return biomass;
}

FLQuantAD operatingModel::fbar(const Rcpp::IntegerVector age_range_indices, const int fishery_no, const int catch_no, const int biol_no) const{
    Rcpp::IntegerVector fdim = f(fishery_no).get_dim();
    FLQuantAD f_age_trim = f(fishery_no)(age_range_indices[0]+1, age_range_indices[1]+1, 1, fdim[1], 1, fdim[2], 1, fdim[3], 1, fdim[4], 1, fdim[5]);  // subsetting
    FLQuantAD fbar_out = quant_mean(f_age_trim);
    return fbar_out;

}

// Assume that catch is catches[[1]] for the moment
FLQuantAD operatingModel::fbar(const Rcpp::IntegerVector age_range_indices, const int biol_no) const{
    //// Make an empty FLQ with the right dims - based on the first fishery
    FLQuantAD fbar_out = fbar(age_range_indices, 1,1,biol_no);
    for (unsigned int fishery_count = 2; fishery_count <= fisheries.get_nfisheries(); ++fishery_count){
        fbar_out = fbar_out + fbar(age_range_indices, fishery_count,1,biol_no);
    }
    return fbar_out;
}

// Catch of a particular fishery
FLQuantAD operatingModel::catches(const int fishery_no, const int catch_no, const int biol_no) const{
    return fisheries(fishery_no, catch_no).catches();
}

// Total catch from an FLBiol
// Assumes the catch is the first FLCatch in the FLFishery
FLQuantAD operatingModel::catches(const int biol_no) const{
    // Get the catch from the first fishery
    FLQuantAD catches_out = catches(1,1,biol_no);
    for (unsigned int fishery_count = 2; fishery_count <= fisheries.get_nfisheries(); ++fishery_count){
        catches_out = catches_out + catches(fishery_count,1,biol_no);
    }
    return catches_out;
}


