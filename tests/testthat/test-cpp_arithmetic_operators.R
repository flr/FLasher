# Maintainer: Finlay Scott, JRC
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

context("Arithmetic operators for FLQuant and FLQuantAD")
source("expect_funs.R")

# There are 18 options per operator
# For example, consider the '*' operator:
# 6 operator assignment
# FLQ   = FLQ     *= FLQ
# FLQAD = FLQAD   *= FLQ
# FLQAD = FLQAD   *= FLQAD
# FLQ   = FLQ     *= double
# FLQAD = FLQAD   *= double
# FLQAD = FLQAD   *= adouble
# 4 binary 'FLQ FLQ' arithmetic operator
# FLQ   = FLQ     *  FLQ
# FLQAD = FLQAD   *  FLQ
# FLQAD = FLQ     *  FLQAD
# FLQAD = FLQAD   *  FLQAD
# 8 binary 'FLQ scalar' arithmetic operator
# FLQ   = FLQ     *  double
# FLQ   = double  *  FLQ
# FLQAD = FLQAD   *  double
# FLQAD = double  *  FLQAD
# FLQAD = FLQ     *  adouble
# FLQAD = adouble *  FLQ
# FLQAD = FLQAD   *  adouble
# FLQAD = adouble *  FLQAD

test_that("multiplication",{
    flq1 <- random_FLQuant_generator()
    flq2 <- flq1
    flq2[] <- rnorm(prod(dim(flq1)),sd = 10)
    value <- rnorm(1)
    # Multiplier assignment
    flq_out <- test_FLQuant_FLQuant_multiplier_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_multiplier_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuant_double_multiplier_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 * value, flq_out)
    flq_out <- test_FLQuantAD_double_multiplier_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 * value, flq_out)
    flq_out <- test_FLQuantAD_adouble_multiplier_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 * value, flq_out)
    # Binary multiplers FLQ<> * FLQ<>
    flq_out <- test_FLQuant_FLQuant_multiplier_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_multiplier_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuant_FLQuantAD_multiplier_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    # Binary multiplers FLQ<> * scalar
    flq_out <- test_FLQuant_double_multiplier_operator(flq1, value)
    expect_FLQuant_equal(flq1 * value, flq_out)
    flq_out <- test_double_FLQuant_multiplier_operator(value, flq1)
    expect_FLQuant_equal(flq1 * value, flq_out)
    flq_out <-  test_FLQuantAD_double_multiplier_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 * value, flq_out)
    flq_out <-  test_double_FLQuantAD_multiplier_operator(value, flq1)
    expect_FLQuant_equal(flq1 * value, flq_out)
    flq_out <- test_FLQuant_adouble_multiplier_operator(flq1, value)
    expect_FLQuant_equal(flq1 * value, flq_out)
    flq_out <- test_adouble_FLQuant_multiplier_operator(value, flq1)
    expect_FLQuant_equal(flq1 * value, flq_out)
    flq_out <- test_FLQuantAD_adouble_multiplier_operator(flq1, value)
    expect_FLQuant_equal(flq1 * value, flq_out)
    flq_out <- test_adouble_FLQuantAD_multiplier_operator(value, flq1)
    expect_FLQuant_equal(flq1 * value, flq_out)
})

test_that("division",{
    flq1 <- random_FLQuant_generator()
    flq2 <- flq1
    flq2[] <- rnorm(prod(dim(flq1)),sd = 10)
    value <- rnorm(1)
    # Division assignment
    flq_out <- test_FLQuant_FLQuant_division_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_division_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_division_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuant_double_division_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 / value, flq_out)
    flq_out <- test_FLQuantAD_double_division_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 / value, flq_out)
    flq_out <- test_FLQuantAD_adouble_division_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 / value, flq_out)
    # Binary division FLQ<> * FLQ<>
    flq_out <- test_FLQuant_FLQuant_division_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_division_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuant_FLQuantAD_division_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_division_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    # Binary division FLQ<> * scalar
    flq_out <- test_FLQuant_double_division_operator(flq1, value)
    expect_FLQuant_equal(flq1 / value, flq_out)
    flq_out <- test_double_FLQuant_division_operator(value, flq1)
    expect_FLQuant_equal(value / flq1, flq_out)
    flq_out <-  test_FLQuantAD_double_division_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 / value, flq_out)
    flq_out <-  test_double_FLQuantAD_division_operator(value, flq1)
    expect_FLQuant_equal(value / flq1, flq_out)
    flq_out <- test_FLQuant_adouble_division_operator(flq1, value)
    expect_FLQuant_equal(flq1 / value, flq_out)
    flq_out <- test_adouble_FLQuant_division_operator(value, flq1)
    expect_FLQuant_equal(value / flq1, flq_out)
    flq_out <- test_FLQuantAD_adouble_division_operator(flq1, value)
    expect_FLQuant_equal(flq1 / value, flq_out)
    flq_out <- test_adouble_FLQuantAD_division_operator(value, flq1)
    expect_FLQuant_equal(value / flq1, flq_out)
})

test_that("subtraction",{
    flq1 <- random_FLQuant_generator()
    flq2 <- flq1
    flq2[] <- rnorm(prod(dim(flq1)),sd = 10)
    value <- rnorm(1)
    # Subtraction assignment
    flq_out <- test_FLQuant_FLQuant_subtraction_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_subtraction_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuant_double_subtraction_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 - value, flq_out)
    flq_out <- test_FLQuantAD_double_subtraction_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 - value, flq_out)
    flq_out <- test_FLQuantAD_adouble_subtraction_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 - value, flq_out)
    # Binary subtraction FLQ<> * FLQ<>
    flq_out <- test_FLQuant_FLQuant_subtraction_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_subtraction_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuant_FLQuantAD_subtraction_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    # Binary subtraction FLQ<> * scalar
    flq_out <- test_FLQuant_double_subtraction_operator(flq1, value)
    expect_FLQuant_equal(flq1 - value, flq_out)
    flq_out <- test_double_FLQuant_subtraction_operator(value, flq1)
    expect_FLQuant_equal(value - flq1, flq_out)
    flq_out <-  test_FLQuantAD_double_subtraction_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 - value, flq_out)
    flq_out <-  test_double_FLQuantAD_subtraction_operator(value, flq1)
    expect_FLQuant_equal(value - flq1, flq_out)
    flq_out <- test_FLQuant_adouble_subtraction_operator(flq1, value)
    expect_FLQuant_equal(flq1 - value, flq_out)
    flq_out <- test_adouble_FLQuant_subtraction_operator(value, flq1)
    expect_FLQuant_equal(value - flq1, flq_out)
    flq_out <- test_FLQuantAD_adouble_subtraction_operator(flq1, value)
    expect_FLQuant_equal(flq1 - value, flq_out)
    flq_out <- test_adouble_FLQuantAD_subtraction_operator(value, flq1)
    expect_FLQuant_equal(value - flq1, flq_out)
})

test_that("addition",{
    flq1 <- random_FLQuant_generator()
    flq2 <- flq1
    flq2[] <- rnorm(prod(dim(flq1)),sd = 10)
    value <- rnorm(1)
    # Addition assignment
    flq_out <- test_FLQuant_FLQuant_addition_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_addition_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_addition_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuant_double_addition_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 + value, flq_out)
    flq_out <- test_FLQuantAD_double_addition_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 + value, flq_out)
    flq_out <- test_FLQuantAD_adouble_addition_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 + value, flq_out)
    # Binary addition FLQ<> + FLQ<>
    flq_out <- test_FLQuant_FLQuant_addition_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_addition_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuant_FLQuantAD_addition_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_addition_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    # Binary addition FLQ<> + scalar
    flq_out <- test_FLQuant_double_addition_operator(flq1, value)
    expect_FLQuant_equal(flq1 + value, flq_out)
    flq_out <- test_double_FLQuant_addition_operator(value, flq1)
    expect_FLQuant_equal(flq1 + value, flq_out)
    flq_out <-  test_FLQuantAD_double_addition_assignment_operator(flq1, value)
    expect_FLQuant_equal(flq1 + value, flq_out)
    flq_out <-  test_double_FLQuantAD_addition_operator(value, flq1)
    expect_FLQuant_equal(flq1 + value, flq_out)
    flq_out <- test_FLQuant_adouble_addition_operator(flq1, value)
    expect_FLQuant_equal(flq1 + value, flq_out)
    flq_out <- test_adouble_FLQuant_addition_operator(value, flq1)
    expect_FLQuant_equal(flq1 + value, flq_out)
    flq_out <- test_FLQuantAD_adouble_addition_operator(flq1, value)
    expect_FLQuant_equal(flq1 + value, flq_out)
    flq_out <- test_adouble_FLQuantAD_addition_operator(value, flq1)
    expect_FLQuant_equal(flq1 + value, flq_out)
})

test_that("composite_arithmetic",{
    flq <- random_FLQuant_generator()
    flqad <- flq
    flqad[] <- rnorm(prod(dim(flq)),sd = 10)
    value <- rnorm(1)
    flq_out <- test_composite_arithmetic_operators(flqad, flq, value)
    result <- ((((value * flq) + value) - flq) / flq) * ((value / flqad) - value)
    expect_FLQuant_equal(flq_out, result)
})

test_that("log and exp",{
    flq <- abs(random_FLQuant_generator())
    # log
    flq_out <- test_FLQuant_log(flq)
    expect_FLQuant_equal(flq_out, log(flq))
    flq_out <- test_FLQuantAD_log(flq)
    expect_FLQuant_equal(flq_out, log(flq))
    # exp
    flq_out <- test_FLQuant_exp(flq)
    expect_FLQuant_equal(flq_out, exp(flq))
    flq_out <- test_FLQuantAD_exp(flq)
    expect_FLQuant_equal(flq_out, exp(flq))
})

test_that("FLQuant and FLQuantAD summary functions", {
    # Test quant_sum
    flq_in <- random_FLQuant_generator(min_dims=c(2,2,2,2,2,2))
    flq_out <- test_FLQuant_quant_sum(flq_in)
    flq_sum <- quantSums(flq_in)
    expect_identical(dim(flq_out), dim(flq_sum))
    expect_identical(dimnames(flq_out), dimnames(flq_sum))
    expect_identical(units(flq_out), units(flq_sum))
    expect_equal(flq_out, flq_sum) # Not using identical as small numeric differences as + mathematical operation - see above
    # Adolc quant_sum
    flq_out <- test_FLQuantAD_quant_sum(flq_in)
    flq_sum <- quantSums(flq_in)
    expect_identical(dim(flq_out), dim(flq_sum))
    expect_identical(dimnames(flq_out), dimnames(flq_sum))
    expect_identical(units(flq_out), units(flq_sum))
    expect_equal(flq_out, flq_sum) 
    # Year sum
    flq_in <- random_FLQuant_generator(min_dims=c(2,2,2,2,2,2))
    flq_out <- test_FLQuant_year_sum(flq_in)
    flq_sum <- yearSums(flq_in)
    expect_FLQuant_equal(flq_out, flq_sum)
    # Adolc unit_sum
    flq_out <- test_FLQuantAD_unit_sum(flq_in)
    flq_sum <- unitSums(flq_in)
    expect_FLQuant_equal(flq_sum, flq_out)
    expect_identical(dim(flq_out), dim(flq_sum))
    expect_identical(dimnames(flq_out), dimnames(flq_sum))
    expect_identical(units(flq_out), units(flq_sum))
    expect_equal(flq_out, flq_sum) 
    # quant_mean
    flq_in <- random_FLQuant_generator()
    flq_out <- test_FLQuant_quant_mean(flq_in)
    flq_mean <- quantMeans(flq_in)
    expect_identical(dim(flq_out), dim(flq_mean))
    expect_identical(dimnames(flq_out), dimnames(flq_mean))
    expect_identical(units(flq_out), units(flq_mean))
    expect_equal(flq_out, flq_mean) # Not using identical as small numeric differences as + mathematical operation - see above
    # year mean
    flq_in <- random_FLQuant_generator()
    flq_out <- test_FLQuant_year_mean(flq_in)
    flq_mean <- yearMeans(flq_in)
    expect_FLQuant_equal(flq_out, flq_mean)
    # Adolc quant mean
    flq_out <- test_FLQuantAD_quant_mean(flq_in)
    flq_mean <- quantMeans(flq_in)
    expect_identical(dim(flq_out), dim(flq_mean))
    expect_identical(dimnames(flq_out), dimnames(flq_mean))
    expect_identical(units(flq_out), units(flq_mean))
    expect_equal(flq_out, flq_mean) # Not using identical as small numeric differences as + mathematical operation - see above
    # max_quant
    flq_in <- random_FLQuant_generator()
    max_flq_in <- apply(flq_in,2:6, max)
    max_flq_out <- test_FLQuant_max_quant(flq_in)
    expect_FLQuant_equal(max_flq_in, max_flq_out)
    max_flq_out <- test_FLQuantAD_max_quant(flq_in)
    expect_FLQuant_equal(max_flq_in, max_flq_out)
    # scale_by_max_quant
    flq_in <- abs(random_FLQuant_generator())
    scaled_in <- flq_in %/% apply(flq_in, 2:6, max)
    scaled_out <- test_FLQuant_scale_by_max_quant(flq_in)
    expect_identical(dimnames(scaled_in), dimnames(scaled_out))
    expect_FLQuant_equal(scaled_in, scaled_out)
    scaled_out <- test_FLQuantAD_scale_by_max_quant(flq_in)
    expect_that(dimnames(scaled_in), is_identical_to(dimnames(scaled_out)))
    expect_FLQuant_equal(scaled_in, scaled_out)
})

test_that("Multiplication: iter = 1 or n", {
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- 1
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    # FLQ *= FLQ
    # niter * = 1iter
    flq_out <- test_FLQuant_FLQuant_multiplier_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    # 1iter * = niter
    flq_out <- test_FLQuant_FLQuant_multiplier_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    # FLQAD *= FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    # FLQAD *= FLQ
    flq_out <- test_FLQuantAD_FLQuant_multiplier_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_multiplier_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    # FLQ * FLQ
    flq_out <- test_FLQuant_FLQuant_multiplier_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuant_FLQuant_multiplier_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    # FLQAdolc * FLQ
    flq_out <- test_FLQuantAD_FLQuant_multiplier_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_multiplier_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    # FLQAD * FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    # FLQ * FLQAD
    flq_out <- test_FLQuant_FLQuantAD_multiplier_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    flq_out <- test_FLQuant_FLQuantAD_multiplier_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 * flq2, flq_out)
    # n * m iters - non-conformable
    flq1 <- random_FLQuant_generator(fixed_dims = c(NA, NA, NA, NA, NA, round(runif(1,min=2,max=10))))
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- fixed_dims[6]+5 
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    expect_error(test_FLQuant_FLQuant_multiplier_assignment_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuantAD_multiplier_assignment_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuant_multiplier_assignment_operator(flq1, flq2))
    expect_error(test_FLQuant_FLQuant_multiplier_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuant_multiplier_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuantAD_multiplier_operator(flq1, flq2))
    expect_error(test_FLQuant_FLQuantAD_multiplier_operator(flq1, flq2))
})

test_that("Division: iter = 1 or n", {
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- 1
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    # FLQ /= FLQ
    # niter / = 1iter
    flq_out <- test_FLQuant_FLQuant_division_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    # 1iter / = niter
    flq_out <- test_FLQuant_FLQuant_division_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 / flq1, flq_out)
    # FLQAD /= FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_division_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_division_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 / flq1, flq_out)
    # FLQAD /= FLQ
    flq_out <- test_FLQuantAD_FLQuant_division_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_division_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 / flq1, flq_out)
    # FLQ / FLQ
    flq_out <- test_FLQuant_FLQuant_division_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuant_FLQuant_division_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 / flq1, flq_out)
    # FLQAdolc / FLQ
    flq_out <- test_FLQuantAD_FLQuant_division_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_division_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 / flq1, flq_out)
    # FLQAD / FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_division_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_division_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 / flq1, flq_out)
    # FLQ / FLQAD
    flq_out <- test_FLQuant_FLQuantAD_division_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 / flq2, flq_out)
    flq_out <- test_FLQuant_FLQuantAD_division_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 / flq1, flq_out)
    # n / m iters - non-conformable
    flq1 <- random_FLQuant_generator(fixed_dims = c(NA, NA, NA, NA, NA, round(runif(1,min=2,max=10))))
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- fixed_dims[6]+5 
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    expect_error(test_FLQuant_FLQuant_division_assignment_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuantAD_division_assignment_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuant_division_assignment_operator(flq1, flq2))
    expect_error(test_FLQuant_FLQuant_division_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuant_division_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuantAD_division_operator(flq1, flq2))
    expect_error(test_FLQuant_FLQuantAD_division_operator(flq1, flq2))
})

test_that("Subtraction: iter = 1 or n", {
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- 1
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    # FLQ -= FLQ
    # niter - = 1iter
    flq_out <- test_FLQuant_FLQuant_subtraction_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    # 1iter - = niter
    flq_out <- test_FLQuant_FLQuant_subtraction_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 - flq1, flq_out)
    # FLQAD -= FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 - flq1, flq_out)
    # FLQAD -= FLQ
    flq_out <- test_FLQuantAD_FLQuant_subtraction_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_subtraction_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 - flq1, flq_out)
    # FLQ - FLQ
    flq_out <- test_FLQuant_FLQuant_subtraction_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuant_FLQuant_subtraction_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 - flq1, flq_out)
    # FLQAdolc - FLQ
    flq_out <- test_FLQuantAD_FLQuant_subtraction_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_subtraction_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 - flq1, flq_out)
    # FLQAD - FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 - flq1, flq_out)
    # FLQ - FLQAD
    flq_out <- test_FLQuant_FLQuantAD_subtraction_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 - flq2, flq_out)
    flq_out <- test_FLQuant_FLQuantAD_subtraction_operator(flq2, flq1)
    expect_FLQuant_equal(flq2 - flq1, flq_out)
    # n - m iters - non-conformable
    flq1 <- random_FLQuant_generator(fixed_dims = c(NA, NA, NA, NA, NA, round(runif(1,min=2,max=10))))
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- fixed_dims[6]+5 
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    expect_error( test_FLQuant_FLQuant_subtraction_assignment_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuantAD_subtraction_assignment_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuant_subtraction_assignment_operator(flq1, flq2))
    expect_error(test_FLQuant_FLQuant_subtraction_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuant_subtraction_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuantAD_subtraction_operator(flq1, flq2))
    expect_error(test_FLQuant_FLQuantAD_subtraction_operator(flq1, flq2))
})

test_that("Addition: iter = 1 or n", {
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- 1
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    # FLQ -= FLQ
    # niter + = 1iter
    flq_out <- test_FLQuant_FLQuant_addition_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    # 1iter + = niter
    flq_out <- test_FLQuant_FLQuant_addition_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    # FLQAD -= FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_addition_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_addition_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    # FLQAD -= FLQ
    flq_out <- test_FLQuantAD_FLQuant_addition_assignment_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_addition_assignment_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    # FLQ + FLQ
    flq_out <- test_FLQuant_FLQuant_addition_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuant_FLQuant_addition_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    # FLQAdolc + FLQ
    flq_out <- test_FLQuantAD_FLQuant_addition_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuant_addition_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    # FLQAD + FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_addition_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuantAD_FLQuantAD_addition_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    # FLQ + FLQAD
    flq_out <- test_FLQuant_FLQuantAD_addition_operator(flq1, flq2)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    flq_out <- test_FLQuant_FLQuantAD_addition_operator(flq2, flq1)
    expect_FLQuant_equal(flq1 + flq2, flq_out)
    # n + m iters + non-conformable
    flq1 <- random_FLQuant_generator(fixed_dims = c(NA, NA, NA, NA, NA, round(runif(1,min=2,max=10))))
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- fixed_dims[6]+5 
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    expect_error(test_FLQuant_FLQuant_addition_assignment_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuantAD_addition_assignment_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuant_addition_assignment_operator(flq1, flq2))
    expect_error(test_FLQuant_FLQuant_addition_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuant_addition_operator(flq1, flq2))
    expect_error(test_FLQuantAD_FLQuantAD_addition_operator(flq1, flq2))
    expect_error(test_FLQuant_FLQuantAD_addition_operator(flq1, flq2))
})

test_that("sweep operations",{
    # Get dims of the FLQs to be operated on
    dim_base <- round(runif(6, min=2, max=10))
    dim1 <- rep(1,6)
    # Which dims to set to 1
    set1 <- round(runif(round(runif(1,min=1,max=4)),min=1, max=6))
    dim1[set1] <- dim_base[set1]
    # The other FLQ
    dim2 <- rep(1,6)
    set2 <- (1:6)[!(1:6 %in% set1)]
    dim2[set2] <- dim_base[set2]
    flq1 <- random_FLQuant_generator(fixed_dims=dim1)
    flq2 <- random_FLQuant_generator(fixed_dims=dim2)
    # Assume that the operations work as the %*% etc. for FLQuant
    # 4 tests for each operator
    # Multiplication
    flqout <- flq1 %*% flq2
    expect_FLQuant_equal(test_sweep_multADAD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_multDD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_multADD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_multDAD(flq1, flq2), flqout)
    # Division
    flqout <- flq1 %/% flq2
    expect_FLQuant_equal(test_sweep_divADAD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_divDD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_divADD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_divDAD(flq1, flq2), flqout)
    # Plus
    flqout <- flq1 %+% flq2
    expect_FLQuant_equal(test_sweep_plusADAD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_plusDD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_plusADD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_plusDAD(flq1, flq2), flqout)
    # Minus
    flqout <- flq1 %-% flq2
    expect_FLQuant_equal(test_sweep_minusADAD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_minusDD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_minusADD(flq1, flq2), flqout)
    expect_FLQuant_equal(test_sweep_minusDAD(flq1, flq2), flqout)
    # Dimension match - OK
    flqout <- flq1 %*% flq1
    expect_FLQuant_equal(test_sweep_multADAD(flq1, flq1), flqout)
    # Dimension mismatch - fails
    index <- round(runif(1,min=1,max=6))
    dim1[index] <- round(runif(1,min=3,max=5))
    dim2[index] <- dim1[index] - 1
    flq1 <- random_FLQuant_generator(fixed_dims=dim1)
    flq2 <- random_FLQuant_generator(fixed_dims=dim2)
    expect_error(test_sweep_multADAD(flq1, flq2))
})

