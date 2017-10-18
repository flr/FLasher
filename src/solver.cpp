/* 
 * Copyright 2015 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

// For timing functions
#include <time.h>

#include "../inst/include/solver.h"

double euclid_norm(std::vector<double> x){
    double xsum = std::inner_product(x.begin(), x.end(), x.begin(), 0.0);
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
// Need to add max_limit

/*! \brief A simple Newton-Raphson optimiser
 *
 * The Newton-Raphons optimiser uses the Jacobian matrix calculated by CppAD.
 * As all iterations of the simulations can be run simultaneously the Jacobian can be treated in discrete chunks along the diagonal.
 * Also, the Jacobian matrix can be very sparse. 
 * This implementation therefore solves each 'chunk' independently to save inverting a massive matrix.
 * The size of each chunk is given by the number of simulation targets (the parameter nsim_targets).
 * Iterations continue until either all the chunks are solved to within the desired tolerance or the maximum number of iterations has been hit.
 * Limits are applied to the minimum and maximum value of indep. These limits are applied while solving to prevent the solver going to strange places.
 * \param indep The initial values of the independent values.
 * \param fun The CppAD function object.
 * \param niter The number of iterations in the simulations.
 * \param nsim_targets The number of targets to solve for in each iteration (determines of the size of the Jacobian chunks).
 * \param indep_min The minimum value of the independent variable (default is 0).
 * \param indep_max The maximum value of the independent variable (default is 1000).
 * \param max_iters The maximum number of solver iterations (not FLR iterations).
 * \param tolerance The tolerance of the solutions.
 */
std::vector<int> newton_raphson(std::vector<double>& indep, CppAD::ADFun<double>& fun, const unsigned int niter, const unsigned int nsim_targets, const double indep_min, const double indep_max, const unsigned int max_iters, const double tolerance){
    bool verbose = false;
    //Rprintf("indep.size(): %i niter: %i, nsim_targets: %i\n",indep.size(), niter, nsim_targets);
    if(verbose){Rprintf("\nIn Newton Raphson\n");}
    // Check that product of niter and nsim_targets = length of indep (Jacobian must be square - otherwise cannot do LU Solve)
    if (indep.size() != (niter * nsim_targets)){
        Rcpp::stop("In newton_raphson: length of indep does not equal product of niter and nsim_targets\n");
    }
    double logdet = 0.0; // Not sure what this actually does but is used in the CppAD LUsolve function
    std::vector<double> y(niter * nsim_targets, 1000.0);
    std::vector<double> delta_indep(niter * nsim_targets, 0.0); // For updating indep in final step
    std::vector<double> jac(niter * nsim_targets * niter * nsim_targets);
    std::vector<double> iter_jac(nsim_targets * nsim_targets);
    std::vector<double> iter_y(nsim_targets);
    std::vector<unsigned int> iter_solved(niter, 0); // If 0, that iter has not been solved
    int jac_element = 0; // an index for Jacobian used for subsetting the Jacobian for each iter
    // Reasons for stopping
    //  1 - Solved within tolerance
    // -1 - Iteration limit reached (default position, if it hasn't stopped for any other reason then it's because the iterations have maxed out)
    // -2 - Min limit reached
    // -3 - Max limit reached
    std::vector<int> success_code(niter, -1); 
    unsigned int nr_count = 0;
    unsigned int start_accum = 0;
    // Keep looping until all sim_targets have been solved, or number of iterations (NR iterations, not FLR iterations) has been hit
    while((std::accumulate(iter_solved.begin(), iter_solved.end(), start_accum) < niter) & (nr_count < max_iters)){ 
        ++nr_count;
        if(verbose){Rprintf("\nnr_count: %i\n", nr_count);}
        // Get y = f(x0)
        if(verbose){Rprintf("Forward\n");}
        y = fun.Forward(0, indep); 
        // Get f'(x0) -  gets Jacobian for all simultaneous targets
        //if(verbose){Rprintf("Getting SparseJacobian\n");}
        jac = fun.SparseJacobian(indep);
        //jac = fun.Jacobian(indep);
        // Alt - does the same 
        //std::vector<double> weight(1);
        //weight[0] = 1.0;
        //jac   = fun.Reverse(1, weight);
        // Get w (f(x0) / f'(x0)) for each iteration if necessary
        // Loop over simultaneous targets, solving if necessary
        for (unsigned int iter_count = 0; iter_count < niter; ++iter_count){
            // Only solve if that iter has not been solved
            if(iter_solved[iter_count] == 0){
            if(verbose){Rprintf("iter_count: %i\n", iter_count);}
                // Subsetting y and Jacobian for that iter only
                for(unsigned int jac_count_row = 0; jac_count_row < nsim_targets; ++jac_count_row){
                    iter_y[jac_count_row] = y[iter_count * nsim_targets + jac_count_row];
                    // Fill up mini Jacobian for that iteration 
                    for(unsigned int jac_count_col = 0; jac_count_col < nsim_targets; ++jac_count_col){
                        jac_element = (iter_count * niter * nsim_targets * nsim_targets) + (iter_count * nsim_targets) + jac_count_row + (jac_count_col * niter * nsim_targets);
                        iter_jac[jac_count_row + (jac_count_col * nsim_targets)] = jac[jac_element];
                    }
                }
                if(verbose){
                    Rprintf("Current X:\n");
                    for(auto x_count = 0; x_count<nsim_targets; ++x_count){
                        Rprintf("%f\t", indep[x_count + (iter_count * nsim_targets)]);
                    }
                    Rprintf("\nCurrent Y:\n");
                    for(auto y_count = 0; y_count<nsim_targets; ++y_count){
                        Rprintf("%f\t", iter_y[y_count]);
                    }
                    Rprintf("\nJacobian of iter %i:\n", iter_count);
                    for (auto print_jac_row=0; print_jac_row < nsim_targets; ++print_jac_row){
                        for (auto print_jac_col=0; print_jac_col < nsim_targets; ++print_jac_col){
                            Rprintf("%f\t", iter_jac[print_jac_row * nsim_targets + print_jac_col]);
                        }
                        Rprintf("\n");
                    }
                }
                // Solve to get w = f(x0) / f'(x0)
                // Puts resulting w (delta_indep) into iter_y
                if(verbose){Rprintf("LU Sove\n");}
                // Solve A. X = B for X given B and A
                // A is n x n, X is n x m, B is n x m
                // n, m, A, B, X, logdet - result placed in X
                // Jacobian must be square - i.e no of indeps must equal no. deps
                CppAD::LuSolve(nsim_targets, nsim_targets, iter_jac, iter_y, iter_y, logdet); 
                if(verbose){Rprintf("Done LU Solving\n");}
                if(verbose){
                    Rprintf("Delta x:\n");
                    for(auto dx_count = 0; dx_count<nsim_targets; ++dx_count){
                        Rprintf("%f\t", iter_y[dx_count]);
                    }
                    Rprintf("\n");
                }
                // Has iter now been solved? If so, set the flag to 1
                if (euclid_norm(iter_y) < tolerance){
                    //Rprintf("Solved iter\n");
                    iter_solved[iter_count] = 1;
                    success_code[iter_count] = 1;
                    // Set that iter_y to 0 as we want to stop updating all the solved iterations when solving the remainder
                    fill(iter_y.begin(), iter_y.end(), 0.0);
                }
                // put iter_y into delta_indep - needs for loop
                for(unsigned int jac_count = 0; jac_count < nsim_targets; ++jac_count){
                    delta_indep[iter_count * nsim_targets + jac_count] = iter_y[jac_count];
                }
            }
        }
        // Update x = x - w
        // Ideally should only update the iterations that have not hit the tolerance
        std::transform(indep.begin(),indep.end(),delta_indep.begin(),indep.begin(),std::minus<double>());
        // Bluntly enforce limits
        // indep cannot be less than minimum value or greater than maximum value
        // Limit during solving loop to prevent the solver going off to weird places? Yes
        // Or just ID the breached iters at the end and correct them (even though solver may go outside limit on way to solution within limit)
        // Should each indep value have it's own limit? - maybe later
        for (unsigned int minmax_counter = 0; minmax_counter < indep.size(); ++minmax_counter){
            // Have we breached min limit?
            if (indep[minmax_counter] <= indep_min){
                indep[minmax_counter] = indep_min;
                success_code[minmax_counter / nsim_targets] = -2;
            }
            // Have we breached max limit?
            if (indep[minmax_counter] >= indep_max){
                indep[minmax_counter] = indep_max;
                success_code[minmax_counter / nsim_targets] = -3;
            }
        } 
    }
    if(verbose){Rprintf("Leaving solver after %i iterations.\n\n", nr_count);}
    return success_code;
}


