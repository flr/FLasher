/* 
 * Copyright 2013 FLR Team. Distributed under the GPL 2 or later
 * Maintainer: Finlay Scott, JRC
 */

#include "../../inst/include/FLQuant_base.h"

/* There are 18 options per operator
 * For example, consider the '*' operator:
 * 6 operator assignment
 * FLQ   = FLQ     *= FLQ
 * FLQAD = FLQAD   *= FLQ
 * FLQAD = FLQAD   *= FLQAD
 * FLQ   = FLQ     *= double
 * FLQAD = FLQAD   *= double
 * FLQAD = FLQAD   *= adouble
 * 4 binary 'FLQ FLQ' arithmetic operator
 * FLQ   = FLQ     *  FLQ
 * FLQAD = FLQAD   *  FLQ
 * FLQAD = FLQ     *  FLQAD
 * FLQAD = FLQAD   *  FLQAD
 * 8 binary 'FLQ scalar' arithmetic operator
 * FLQ   = FLQ     *  double
 * FLQ   = double  *  FLQ
 * FLQAD = FLQAD   *  double
 * FLQAD = double  *  FLQAD
 * FLQAD = FLQ     *  adouble
 * FLQAD = adouble *  FLQ
 * FLQAD = FLQAD   *  adouble
 * FLQAD = adouble *  FLQAD
 */

//------------------ Multiplication ----------------------

/* Multiplication assignment
 * 6 of them
 */

// [[Rcpp::export]]
FLQuant test_FLQuant_FLQuant_multiplier_assignment_operator(FLQuant flq1, FLQuant flq2){
    flq1 *= flq2;
    return flq1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuant_multiplier_assignment_operator(FLQuantAD flqad1, FLQuant flq2){
    flqad1 *= flq2;
    return flqad1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuantAD_multiplier_assignment_operator(FLQuantAD flqad1, FLQuantAD flqad2){
    flqad1 *= flqad2;
    return flqad1;
}

// [[Rcpp::export]]
FLQuant test_FLQuant_double_multiplier_assignment_operator(FLQuant flq1, double value){
    flq1 *= value;
    return flq1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_double_multiplier_assignment_operator(FLQuantAD flqad1, double value){
    flqad1 *= value;
    return flqad1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_adouble_multiplier_assignment_operator(FLQuantAD flqad1, double value){
    adouble ad_value = value;
    flqad1 *= ad_value;
    return flqad1;
}

/* Doesn't compile; which is good as it isn't supposed to. Just checking understanding of the member method.
// [[Rcpp::export]]
FLQuant test_FLQuant_FLQuantAD_multiplier_assignment_operator(FLQuant flq1, FLQuantAD flqad2){
    flq1 *= flqad2;
    return flq1;
}
*/

/* Binary Multiplication
 * 12 of them
 */

// [[Rcpp::export]]
FLQuant test_FLQuant_FLQuant_multiplier_operator(FLQuant flq1, FLQuant flq2){
    FLQuant flq3 = flq1 * flq2;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuant_multiplier_operator(FLQuantAD flqad1, FLQuant flq2){
    FLQuantAD flqad3;
    flqad3 = flqad1 * flq2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuant_FLQuantAD_multiplier_operator(FLQuant flq1, FLQuantAD flqad2){
    FLQuantAD flqad3;
    flqad3 = flq1 * flqad2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuantAD_multiplier_operator(FLQuantAD flqad1, FLQuantAD flqad2){
    FLQuantAD flqad3;
    flqad3 = flqad1 * flqad2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuant test_FLQuant_double_multiplier_operator(FLQuant flq1, double value){
    FLQuant flq3 = flq1 * value;
    return flq3;
}

// [[Rcpp::export]]
FLQuant test_double_FLQuant_multiplier_operator(double value, FLQuant flq1){
    FLQuant flq3 = value * flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_double_FLQuantAD_multiplier_operator(double value, FLQuantAD flq1){
    FLQuantAD flq3 = value * flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_double_multiplier_operator(FLQuantAD flq1, double value){
    FLQuantAD flq3;
    flq1 * value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuant_adouble_multiplier_operator(FLQuant flq1, double value){
    adouble ad_value = value;
    FLQuantAD flq3 = flq1 * ad_value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_adouble_FLQuant_multiplier_operator(double value, FLQuant flq1){
    adouble ad_value = value;
    FLQuantAD flq3 = ad_value * flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_adouble_multiplier_operator(FLQuantAD flq1, double value){
    adouble ad_value = value;
    FLQuantAD flq3 = flq1 * ad_value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_adouble_FLQuantAD_multiplier_operator(double value, FLQuantAD flq1){
    adouble ad_value = value;
    FLQuantAD flq3 = ad_value * flq1;
    return flq3;
}

//------------------ Division ----------------------

/* Division assignment
 * 6 of them
 */

// [[Rcpp::export]]
FLQuant test_FLQuant_FLQuant_division_assignment_operator(FLQuant flq1, FLQuant flq2){
    flq1 /= flq2;
    return flq1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuant_division_assignment_operator(FLQuantAD flqad1, FLQuant flq2){
    flqad1 /= flq2;
    return flqad1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuantAD_division_assignment_operator(FLQuantAD flqad1, FLQuantAD flqad2){
    flqad1 /= flqad2;
    return flqad1;
}

// [[Rcpp::export]]
FLQuant test_FLQuant_double_division_assignment_operator(FLQuant flq1, double value){
    flq1 /= value;
    return flq1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_double_division_assignment_operator(FLQuantAD flqad1, double value){
    flqad1 /= value;
    return flqad1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_adouble_division_assignment_operator(FLQuantAD flqad1, double value){
    adouble ad_value = value;
    flqad1 /= ad_value;
    return flqad1;
}

/* Binary division
 * 12 of them
 */

// [[Rcpp::export]]
FLQuant test_FLQuant_FLQuant_division_operator(FLQuant flq1, FLQuant flq2){
    FLQuant flq3 = flq1 / flq2;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuant_division_operator(FLQuantAD flqad1, FLQuant flq2){
    FLQuantAD flqad3;
    flqad3 = flqad1 / flq2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuant_FLQuantAD_division_operator(FLQuant flq1, FLQuantAD flqad2){
    FLQuantAD flqad3;
    flqad3 = flq1 / flqad2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuantAD_division_operator(FLQuantAD flqad1, FLQuantAD flqad2){
    FLQuantAD flqad3;
    flqad3 = flqad1 / flqad2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuant test_FLQuant_double_division_operator(FLQuant flq1, double value){
    FLQuant flq3 = flq1 / value;
    return flq3;
}

// [[Rcpp::export]]
FLQuant test_double_FLQuant_division_operator(double value, FLQuant flq1){
    FLQuant flq3 = value / flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_double_FLQuantAD_division_operator(double value, FLQuantAD flq1){
    FLQuantAD flq3 = value / flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_double_division_operator(FLQuantAD flq1, double value){
    FLQuantAD flq3 = flq1 / value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuant_adouble_division_operator(FLQuant flq1, double value){
    adouble ad_value = value;
    FLQuantAD flq3 = flq1 / ad_value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_adouble_FLQuant_division_operator(double value, FLQuant flq1){
    adouble ad_value = value;
    FLQuantAD flq3 = ad_value / flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_adouble_division_operator(FLQuantAD flq1, double value){
    adouble ad_value = value;
    FLQuantAD flq3 = flq1 / ad_value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_adouble_FLQuantAD_division_operator(double value, FLQuantAD flq1){
    adouble ad_value = value;
    FLQuantAD flq3 = ad_value / flq1;
    return flq3;
}

//------------------ Subtraction ----------------------

/* Subtraction assignment
 * 6 of them
 */

// [[Rcpp::export]]
FLQuant test_FLQuant_FLQuant_subtraction_assignment_operator(FLQuant flq1, FLQuant flq2){
    flq1 -= flq2;
    return flq1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuant_subtraction_assignment_operator(FLQuantAD flqad1, FLQuant flq2){
    flqad1 -= flq2;
    return flqad1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuantAD_subtraction_assignment_operator(FLQuantAD flqad1, FLQuantAD flqad2){
    flqad1 -= flqad2;
    return flqad1;
}

// [[Rcpp::export]]
FLQuant test_FLQuant_double_subtraction_assignment_operator(FLQuant flq1, double value){
    flq1 -= value;
    return flq1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_double_subtraction_assignment_operator(FLQuantAD flqad1, double value){
    flqad1 -= value;
    return flqad1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_adouble_subtraction_assignment_operator(FLQuantAD flqad1, double value){
    adouble ad_value = value;
    flqad1 -= ad_value;
    return flqad1;
}

/* Binary subtraction
 * 12 of them
 */

// [[Rcpp::export]]
FLQuant test_FLQuant_FLQuant_subtraction_operator(FLQuant flq1, FLQuant flq2){
    FLQuant flq3 = flq1 - flq2;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuant_subtraction_operator(FLQuantAD flqad1, FLQuant flq2){
    FLQuantAD flqad3;
    flqad3 = flqad1 - flq2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuant_FLQuantAD_subtraction_operator(FLQuant flq1, FLQuantAD flqad2){
    FLQuantAD flqad3;
    flqad3 = flq1 - flqad2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuantAD_subtraction_operator(FLQuantAD flqad1, FLQuantAD flqad2){
    FLQuantAD flqad3;
    flqad3 = flqad1 - flqad2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuant test_FLQuant_double_subtraction_operator(FLQuant flq1, double value){
    FLQuant flq3 = flq1 - value;
    return flq3;
}

// [[Rcpp::export]]
FLQuant test_double_FLQuant_subtraction_operator(double value, FLQuant flq1){
    FLQuant flq3 = value - flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_double_FLQuantAD_subtraction_operator(double value, FLQuantAD flq1){
    FLQuantAD flq3 = value - flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_double_subtraction_operator(FLQuantAD flq1, double value){
    FLQuantAD flq3 = flq1 - value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuant_adouble_subtraction_operator(FLQuant flq1, double value){
    adouble ad_value = value;
    FLQuantAD flq3 = flq1 - ad_value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_adouble_FLQuant_subtraction_operator(double value, FLQuant flq1){
    adouble ad_value = value;
    FLQuantAD flq3 = ad_value - flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_adouble_subtraction_operator(FLQuantAD flq1, double value){
    adouble ad_value = value;
    FLQuantAD flq3 = flq1 - ad_value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_adouble_FLQuantAD_subtraction_operator(double value, FLQuantAD flq1){
    adouble ad_value = value;
    FLQuantAD flq3 = ad_value - flq1;
    return flq3;
}

//------------------ Addition ----------------------

/* Addition assignment
 * 6 of them
 */

// [[Rcpp::export]]
FLQuant test_FLQuant_FLQuant_addition_assignment_operator(FLQuant flq1, FLQuant flq2){
    flq1 += flq2;
    return flq1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuant_addition_assignment_operator(FLQuantAD flqad1, FLQuant flq2){
    flqad1 += flq2;
    return flqad1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuantAD_addition_assignment_operator(FLQuantAD flqad1, FLQuantAD flqad2){
    flqad1 += flqad2;
    return flqad1;
}

// [[Rcpp::export]]
FLQuant test_FLQuant_double_addition_assignment_operator(FLQuant flq1, double value){
    flq1 += value;
    return flq1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_double_addition_assignment_operator(FLQuantAD flqad1, double value){
    flqad1 += value;
    return flqad1;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_adouble_addition_assignment_operator(FLQuantAD flqad1, double value){
    adouble ad_value = value;
    flqad1 += ad_value;
    return flqad1;
}

/* Binary addition
 * 12 of them
 */

// [[Rcpp::export]]
FLQuant test_FLQuant_FLQuant_addition_operator(FLQuant flq1, FLQuant flq2){
    FLQuant flq3 = flq1 + flq2;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuant_addition_operator(FLQuantAD flqad1, FLQuant flq2){
    FLQuantAD flqad3;
    flqad3 = flqad1 + flq2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuant_FLQuantAD_addition_operator(FLQuant flq1, FLQuantAD flqad2){
    FLQuantAD flqad3;
    flqad3 = flq1 + flqad2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_FLQuantAD_addition_operator(FLQuantAD flqad1, FLQuantAD flqad2){
    FLQuantAD flqad3;
    flqad3 = flqad1 + flqad2;
    return flqad3;
}

// [[Rcpp::export]]
FLQuant test_FLQuant_double_addition_operator(FLQuant flq1, double value){
    FLQuant flq3 = flq1 + value;
    return flq3;
}

// [[Rcpp::export]]
FLQuant test_double_FLQuant_addition_operator(double value, FLQuant flq1){
    FLQuant flq3 = value + flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_double_FLQuantAD_addition_operator(double value, FLQuantAD flq1){
    FLQuantAD flq3 = value + flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_double_addition_operator(FLQuantAD flq1, double value){
    FLQuantAD flq3;
    flq1 + value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuant_adouble_addition_operator(FLQuant flq1, double value){
    adouble ad_value = value;
    FLQuantAD flq3 = flq1 + ad_value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_adouble_FLQuant_addition_operator(double value, FLQuant flq1){
    adouble ad_value = value;
    FLQuantAD flq3 = ad_value + flq1;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_adouble_addition_operator(FLQuantAD flq1, double value){
    adouble ad_value = value;
    FLQuantAD flq3 = flq1 + ad_value;
    return flq3;
}

// [[Rcpp::export]]
FLQuantAD test_adouble_FLQuantAD_addition_operator(double value, FLQuantAD flq1){
    adouble ad_value = value;
    FLQuantAD flq3 = ad_value + flq1;
    return flq3;
}

//--------- log and exp functions ----------------------

// [[Rcpp::export]]
FLQuant test_FLQuant_log(FLQuant flq){
    FLQuant flq_out = log(flq);
    return flq_out;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_log(FLQuantAD flq){
    FLQuantAD flq_out = log(flq);
    return flq_out;
}

// [[Rcpp::export]]
FLQuant test_FLQuant_exp(FLQuant flq){
    FLQuant flq_out = exp(flq);
    return flq_out;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_exp(FLQuantAD flq){
    FLQuantAD flq_out = exp(flq);
    return flq_out;
}


//----------- Composite tests -----------------
// Can we string all these together?

// [[Rcpp::export]]
FLQuantAD test_composite_arithmetic_operators(FLQuantAD flqad, FLQuant flq, double value){
    FLQuantAD flqad_out;
    flqad_out = ((((value * flq) + value) - flq) / flq) * ((value / flqad) - value);
    return flqad_out;
}

//------------- Other mathematical functions ------------------

// quant_sum
// [[Rcpp::export]]
FLQuant test_FLQuant_quant_sum(FLQuant flq){
    FLQuant flq_out = quant_sum(flq);
    return flq_out;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_quant_sum(FLQuantAD flq){
    FLQuantAD flq_out = quant_sum(flq);
    return flq_out;
}

// quant_mean
// [[Rcpp::export]]
FLQuant test_FLQuant_quant_mean(FLQuant flq){
    FLQuant flq_out = quant_mean(flq);
    return flq_out;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_quant_mean(FLQuantAD flq){
    FLQuantAD flq_out = quant_mean(flq);
    return flq_out;
}

// max_quant
// [[Rcpp::export]]
FLQuant test_FLQuant_max_quant(FLQuant flq){
    FLQuant flq_out = max_quant(flq);
    return flq_out;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_max_quant(FLQuantAD flq){
    FLQuantAD flq_out = max_quant(flq);
    return flq_out;
}

// max_quant
// [[Rcpp::export]]
FLQuant test_FLQuant_scale_by_max_quant(FLQuant flq){
    FLQuant flq_out = scale_by_max_quant(flq);
    return flq_out;
}

// [[Rcpp::export]]
FLQuantAD test_FLQuantAD_scale_by_max_quant(FLQuantAD flq){
    FLQuantAD flq_out = scale_by_max_quant(flq);
    return flq_out;
}


//-------------------------------
// Sweep
// Mult
// [[Rcpp::export]]
FLQuantAD test_sweep_multADAD(FLQuantAD flq1, FLQuantAD flq2){
    return sweep_mult(flq1, flq2);
}

// [[Rcpp::export]]
FLQuant test_sweep_multDD(FLQuant flq1, FLQuant flq2){
    return sweep_mult(flq1, flq2);
}

// [[Rcpp::export]]
FLQuantAD test_sweep_multADD(FLQuantAD flq1, FLQuant flq2){
    return sweep_mult(flq1, flq2);
}

// [[Rcpp::export]]
FLQuantAD test_sweep_multDAD(FLQuant flq1, FLQuantAD flq2){
    return sweep_mult(flq1, flq2);
}

// Div
// [[Rcpp::export]]
FLQuantAD test_sweep_divADAD(FLQuantAD flq1, FLQuantAD flq2){
    return sweep_div(flq1, flq2);
}

// [[Rcpp::export]]
FLQuant test_sweep_divDD(FLQuant flq1, FLQuant flq2){
    return sweep_div(flq1, flq2);
}

// [[Rcpp::export]]
FLQuantAD test_sweep_divADD(FLQuantAD flq1, FLQuant flq2){
    return sweep_div(flq1, flq2);
}

// [[Rcpp::export]]
FLQuantAD test_sweep_divDAD(FLQuant flq1, FLQuantAD flq2){
    return sweep_div(flq1, flq2);
}

// Plus
// [[Rcpp::export]]
FLQuantAD test_sweep_plusADAD(FLQuantAD flq1, FLQuantAD flq2){
    return sweep_plus(flq1, flq2);
}

// [[Rcpp::export]]
FLQuant test_sweep_plusDD(FLQuant flq1, FLQuant flq2){
    return sweep_plus(flq1, flq2);
}

// [[Rcpp::export]]
FLQuantAD test_sweep_plusADD(FLQuantAD flq1, FLQuant flq2){
    return sweep_plus(flq1, flq2);
}

// [[Rcpp::export]]
FLQuantAD test_sweep_plusDAD(FLQuant flq1, FLQuantAD flq2){
    return sweep_plus(flq1, flq2);
}

// Minus
// [[Rcpp::export]]
FLQuantAD test_sweep_minusADAD(FLQuantAD flq1, FLQuantAD flq2){
    return sweep_minus(flq1, flq2);
}

// [[Rcpp::export]]
FLQuant test_sweep_minusDD(FLQuant flq1, FLQuant flq2){
    return sweep_minus(flq1, flq2);
}

// [[Rcpp::export]]
FLQuantAD test_sweep_minusADD(FLQuantAD flq1, FLQuant flq2){
    return sweep_minus(flq1, flq2);
}

// [[Rcpp::export]]
FLQuantAD test_sweep_minusDAD(FLQuant flq1, FLQuantAD flq2){
    return sweep_minus(flq1, flq2);
}

