context("Arithmetic operators for FLQuant and FLQuantAD")

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
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data * flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuant_multiplier_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data * flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data * flq2@.Data))
    flq_out <- test_FLQuant_double_multiplier_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 * value))
    flq_out <- test_FLQuantAD_double_multiplier_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 * value))
    flq_out <- test_FLQuantAD_adouble_multiplier_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 * value))
    # Binary multiplers FLQ<> * FLQ<>
    flq_out <- test_FLQuant_FLQuant_multiplier_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data * flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuant_multiplier_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data * flq2@.Data))
    flq_out <- test_FLQuant_FLQuantAD_multiplier_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data * flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data * flq2@.Data))
    # Binary multiplers FLQ<> * scalar
    flq_out <- test_FLQuant_double_multiplier_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 * value))
    flq_out <- test_double_FLQuant_multiplier_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value * flq1))
    flq_out <-  test_FLQuantAD_double_multiplier_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(value * flq1))
    flq_out <-  test_double_FLQuantAD_multiplier_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value * flq1))
    flq_out <- test_FLQuant_adouble_multiplier_operator(flq1, value)
    expect_that(flq_out, is_identical_to(value * flq1))
    flq_out <- test_adouble_FLQuant_multiplier_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value * flq1))
    flq_out <- test_FLQuantAD_adouble_multiplier_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 * value))
    flq_out <- test_adouble_FLQuantAD_multiplier_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value * flq1))
})

test_that("division",{
    flq1 <- random_FLQuant_generator()
    flq2 <- flq1
    flq2[] <- rnorm(prod(dim(flq1)),sd = 10)
    value <- rnorm(1)
    # Division assignment
    flq_out <- test_FLQuant_FLQuant_division_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data / flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuant_division_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data / flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_division_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data / flq2@.Data))
    flq_out <- test_FLQuant_double_division_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 / value))
    flq_out <- test_FLQuantAD_double_division_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 / value))
    flq_out <- test_FLQuantAD_adouble_division_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 / value))
    # Binary division FLQ<> * FLQ<>
    flq_out <- test_FLQuant_FLQuant_division_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data / flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuant_division_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data / flq2@.Data))
    flq_out <- test_FLQuant_FLQuantAD_division_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data / flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_division_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data / flq2@.Data))
    # Binary division FLQ<> * scalar
    flq_out <- test_FLQuant_double_division_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 / value))
    flq_out <- test_double_FLQuant_division_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value / flq1))
    flq_out <-  test_FLQuantAD_double_division_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 / value))
    flq_out <-  test_double_FLQuantAD_division_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value / flq1))
    flq_out <- test_FLQuant_adouble_division_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 / value))
    flq_out <- test_adouble_FLQuant_division_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value / flq1))
    flq_out <- test_FLQuantAD_adouble_division_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 / value))
    flq_out <- test_adouble_FLQuantAD_division_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value / flq1))
})

test_that("subtraction",{
    flq1 <- random_FLQuant_generator()
    flq2 <- flq1
    flq2[] <- rnorm(prod(dim(flq1)),sd = 10)
    value <- rnorm(1)
    # Subtraction assignment
    flq_out <- test_FLQuant_FLQuant_subtraction_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data - flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuant_subtraction_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data - flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data - flq2@.Data))
    flq_out <- test_FLQuant_double_subtraction_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 - value))
    flq_out <- test_FLQuantAD_double_subtraction_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 - value))
    flq_out <- test_FLQuantAD_adouble_subtraction_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 - value))
    # Binary subtraction FLQ<> * FLQ<>
    flq_out <- test_FLQuant_FLQuant_subtraction_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data - flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuant_subtraction_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data - flq2@.Data))
    flq_out <- test_FLQuant_FLQuantAD_subtraction_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data - flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data - flq2@.Data))
    # Binary subtraction FLQ<> * scalar
    flq_out <- test_FLQuant_double_subtraction_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 - value))
    flq_out <- test_double_FLQuant_subtraction_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value - flq1))
    flq_out <-  test_FLQuantAD_double_subtraction_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 - value))
    flq_out <-  test_double_FLQuantAD_subtraction_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value - flq1))
    flq_out <- test_FLQuant_adouble_subtraction_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 - value))
    flq_out <- test_adouble_FLQuant_subtraction_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value - flq1))
    flq_out <- test_FLQuantAD_adouble_subtraction_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 - value))
    flq_out <- test_adouble_FLQuantAD_subtraction_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value - flq1))
})

test_that("addition",{
    flq1 <- random_FLQuant_generator()
    flq2 <- flq1
    flq2[] <- rnorm(prod(dim(flq1)),sd = 10)
    value <- rnorm(1)
    # Addition assignment
    flq_out <- test_FLQuant_FLQuant_addition_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data + flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuant_addition_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data + flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_addition_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data + flq2@.Data))
    flq_out <- test_FLQuant_double_addition_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 + value))
    flq_out <- test_FLQuantAD_double_addition_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 + value))
    flq_out <- test_FLQuantAD_adouble_addition_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 + value))
    # Binary addition FLQ<> + FLQ<>
    flq_out <- test_FLQuant_FLQuant_addition_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data + flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuant_addition_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data + flq2@.Data))
    flq_out <- test_FLQuant_FLQuantAD_addition_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data + flq2@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_addition_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to(flq1@.Data + flq2@.Data))
    # Binary addition FLQ<> + scalar
    flq_out <- test_FLQuant_double_addition_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 + value))
    flq_out <- test_double_FLQuant_addition_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value + flq1))
    flq_out <-  test_FLQuantAD_double_addition_assignment_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 + value))
    flq_out <-  test_double_FLQuantAD_addition_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value + flq1))
    flq_out <- test_FLQuant_adouble_addition_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 + value))
    flq_out <- test_adouble_FLQuant_addition_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value + flq1))
    flq_out <- test_FLQuantAD_adouble_addition_operator(flq1, value)
    expect_that(flq_out, is_identical_to(flq1 + value))
    flq_out <- test_adouble_FLQuantAD_addition_operator(value, flq1)
    expect_that(flq_out, is_identical_to(value + flq1))
})

test_that("composite_arithmetic",{
    flq <- random_FLQuant_generator()
    flqad <- flq
    flqad[] <- rnorm(prod(dim(flq)),sd = 10)
    value <- rnorm(1)
    flq_out <- test_composite_arithmetic_operators(flqad, flq, value)
    result <- ((((value * flq) + value) - flq) / flq) * ((value / flqad) - value)
    expect_that(flq_out@.Data, is_identical_to(result@.Data))
})

test_that("log and exp",{
    flq <- abs(random_FLQuant_generator())
    # log
    flq_out <- test_FLQuant_log(flq)
    expect_that(flq_out@.Data, is_identical_to(log(flq)@.Data))
    flq_out <- test_FLQuantAD_log(flq)
    expect_that(flq_out@.Data, is_identical_to(log(flq)@.Data))
    # exp
    flq_out <- test_FLQuant_exp(flq)
    expect_that(flq_out@.Data, is_identical_to(exp(flq)@.Data))
    flq_out <- test_FLQuantAD_exp(flq)
    expect_that(flq_out@.Data, is_identical_to(exp(flq)@.Data))
})

test_that("FLQuant and FLQuantAD summary functions", {
    # Test quant_sum
    flq_in <- random_FLQuant_generator()
    flq_out <- test_FLQuant_quant_sum(flq_in)
    flq_sum <- quantSums(flq_in)
    expect_that(dim(flq_out), is_identical_to(dim(flq_sum)))
    expect_that(dimnames(flq_out), is_identical_to(dimnames(flq_sum)))
    expect_that(units(flq_out), is_identical_to(units(flq_sum)))
    expect_that(flq_out, equals(flq_sum)) # Not using identical as small numeric differences as + mathematical operation - see above
    # Adolc quant_sum
    flq_out <- test_FLQuantAD_quant_sum(flq_in)
    flq_sum <- quantSums(flq_in)
    expect_that(dim(flq_out), is_identical_to(dim(flq_sum)))
    expect_that(dimnames(flq_out), is_identical_to(dimnames(flq_sum)))
    expect_that(units(flq_out), is_identical_to(units(flq_sum)))
    expect_that(flq_out, equals(flq_sum)) # Not using identical as small numeric differences as + mathematical operation - see above

    # quant_mean
    flq_in <- random_FLQuant_generator()
    flq_out <- test_FLQuant_quant_mean(flq_in)
    flq_mean <- quantMeans(flq_in)
    expect_that(dim(flq_out), is_identical_to(dim(flq_mean)))
    expect_that(dimnames(flq_out), is_identical_to(dimnames(flq_mean)))
    expect_that(units(flq_out), is_identical_to(units(flq_mean)))
    expect_that(flq_out, equals(flq_mean)) # Not using identical as small numeric differences as + mathematical operation - see above
    # Adolc quant mean
    flq_out <- test_FLQuantAD_quant_mean(flq_in)
    flq_mean <- quantMeans(flq_in)
    expect_that(dim(flq_out), is_identical_to(dim(flq_mean)))
    expect_that(dimnames(flq_out), is_identical_to(dimnames(flq_mean)))
    expect_that(units(flq_out), is_identical_to(units(flq_mean)))
    expect_that(flq_out, equals(flq_mean)) # Not using identical as small numeric differences as + mathematical operation - see above

    # max_quant
    flq_in <- random_FLQuant_generator()
    max_flq_in <- apply(flq_in,2:6, max)
    max_flq_out <- test_FLQuant_max_quant(flq_in)
    expect_that(max_flq_in, is_identical_to(max_flq_out))
    max_flq_out <- test_FLQuantAD_max_quant(flq_in)
    expect_that(max_flq_in, is_identical_to(max_flq_out))
    
    # scale_by_max_quant
    flq_in <- abs(random_FLQuant_generator())
    scaled_in <- flq_in %/% apply(flq_in, 2:6, max)
    scaled_out <- test_FLQuant_scale_by_max_quant(flq_in)
    expect_that(dimnames(scaled_in), is_identical_to(dimnames(scaled_out)))
    expect_that(scaled_in@.Data, is_identical_to(scaled_out@.Data))
    scaled_out <- test_FLQuantAD_scale_by_max_quant(flq_in)
    expect_that(dimnames(scaled_in), is_identical_to(dimnames(scaled_out)))
    expect_that(scaled_in@.Data, is_identical_to(scaled_out@.Data))
})

test_that("Multiplication: iter = 1 or n", {
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- 1
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    # FLQ *= FLQ
    # niter * = 1iter
    flq_out <- test_FLQuant_FLQuant_multiplier_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 * flq2)@.Data))
    # 1iter * = niter
    flq_out <- test_FLQuant_FLQuant_multiplier_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 * flq1)@.Data))
    # FLQAD *= FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 * flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 * flq1)@.Data))
    # FLQAD *= FLQ
    flq_out <- test_FLQuantAD_FLQuant_multiplier_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 * flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuant_multiplier_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 * flq1)@.Data))
    # FLQ * FLQ
    flq_out <- test_FLQuant_FLQuant_multiplier_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 * flq2)@.Data))
    flq_out <- test_FLQuant_FLQuant_multiplier_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 * flq1)@.Data))
    # FLQAdolc * FLQ
    flq_out <- test_FLQuantAD_FLQuant_multiplier_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 * flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuant_multiplier_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 * flq1)@.Data))
    # FLQAD * FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 * flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_multiplier_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 * flq1)@.Data))
    # FLQ * FLQAD
    flq_out <- test_FLQuant_FLQuantAD_multiplier_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 * flq2)@.Data))
    flq_out <- test_FLQuant_FLQuantAD_multiplier_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 * flq1)@.Data))
    # n * m iters - non-conformable
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- fixed_dims[6]+5 
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    expect_that(test_FLQuant_FLQuant_multiplier_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuantAD_multiplier_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuant_multiplier_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuant_FLQuant_multiplier_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuant_multiplier_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuantAD_multiplier_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuant_FLQuantAD_multiplier_operator(flq1, flq2), throws_error())
})

test_that("Division: iter = 1 or n", {
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- 1
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    # FLQ /= FLQ
    # niter / = 1iter
    flq_out <- test_FLQuant_FLQuant_division_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 / flq2)@.Data))
    # 1iter / = niter
    flq_out <- test_FLQuant_FLQuant_division_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 / flq1)@.Data))
    # FLQAD /= FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_division_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 / flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_division_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 / flq1)@.Data))
    # FLQAD /= FLQ
    flq_out <- test_FLQuantAD_FLQuant_division_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 / flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuant_division_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 / flq1)@.Data))
    # FLQ / FLQ
    flq_out <- test_FLQuant_FLQuant_division_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 / flq2)@.Data))
    flq_out <- test_FLQuant_FLQuant_division_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 / flq1)@.Data))
    # FLQAdolc / FLQ
    flq_out <- test_FLQuantAD_FLQuant_division_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 / flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuant_division_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 / flq1)@.Data))
    # FLQAD / FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_division_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 / flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_division_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 / flq1)@.Data))
    # FLQ / FLQAD
    flq_out <- test_FLQuant_FLQuantAD_division_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 / flq2)@.Data))
    flq_out <- test_FLQuant_FLQuantAD_division_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 / flq1)@.Data))
    # n / m iters - non-conformable
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- fixed_dims[6]+5 
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    expect_that(test_FLQuant_FLQuant_division_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuantAD_division_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuant_division_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuant_FLQuant_division_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuant_division_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuantAD_division_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuant_FLQuantAD_division_operator(flq1, flq2), throws_error())
})


test_that("Subtraction: iter = 1 or n", {
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- 1
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    # FLQ -= FLQ
    # niter - = 1iter
    flq_out <- test_FLQuant_FLQuant_subtraction_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 - flq2)@.Data))
    # 1iter - = niter
    flq_out <- test_FLQuant_FLQuant_subtraction_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 - flq1)@.Data))
    # FLQAD -= FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 - flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 - flq1)@.Data))
    # FLQAD -= FLQ
    flq_out <- test_FLQuantAD_FLQuant_subtraction_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 - flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuant_subtraction_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 - flq1)@.Data))
    # FLQ - FLQ
    flq_out <- test_FLQuant_FLQuant_subtraction_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 - flq2)@.Data))
    flq_out <- test_FLQuant_FLQuant_subtraction_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 - flq1)@.Data))
    # FLQAdolc - FLQ
    flq_out <- test_FLQuantAD_FLQuant_subtraction_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 - flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuant_subtraction_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 - flq1)@.Data))
    # FLQAD - FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 - flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_subtraction_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 - flq1)@.Data))
    # FLQ - FLQAD
    flq_out <- test_FLQuant_FLQuantAD_subtraction_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 - flq2)@.Data))
    flq_out <- test_FLQuant_FLQuantAD_subtraction_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 - flq1)@.Data))
    # n - m iters - non-conformable
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- fixed_dims[6]+5 
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    expect_that(test_FLQuant_FLQuant_subtraction_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuantAD_subtraction_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuant_subtraction_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuant_FLQuant_subtraction_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuant_subtraction_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuantAD_subtraction_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuant_FLQuantAD_subtraction_operator(flq1, flq2), throws_error())
})

test_that("Addition: iter = 1 or n", {
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- 1
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    # FLQ -= FLQ
    # niter + = 1iter
    flq_out <- test_FLQuant_FLQuant_addition_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 + flq2)@.Data))
    # 1iter + = niter
    flq_out <- test_FLQuant_FLQuant_addition_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 + flq1)@.Data))
    # FLQAD -= FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_addition_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 + flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_addition_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 + flq1)@.Data))
    # FLQAD -= FLQ
    flq_out <- test_FLQuantAD_FLQuant_addition_assignment_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 + flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuant_addition_assignment_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 + flq1)@.Data))
    # FLQ + FLQ
    flq_out <- test_FLQuant_FLQuant_addition_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 + flq2)@.Data))
    flq_out <- test_FLQuant_FLQuant_addition_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 + flq1)@.Data))
    # FLQAdolc + FLQ
    flq_out <- test_FLQuantAD_FLQuant_addition_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 + flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuant_addition_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 + flq1)@.Data))
    # FLQAD + FLQAD
    flq_out <- test_FLQuantAD_FLQuantAD_addition_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 + flq2)@.Data))
    flq_out <- test_FLQuantAD_FLQuantAD_addition_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 + flq1)@.Data))
    # FLQ + FLQAD
    flq_out <- test_FLQuant_FLQuantAD_addition_operator(flq1, flq2)
    expect_that(flq_out@.Data, is_identical_to((flq1 + flq2)@.Data))
    flq_out <- test_FLQuant_FLQuantAD_addition_operator(flq2, flq1)
    expect_that(flq_out@.Data, is_identical_to((flq2 + flq1)@.Data))
    # n + m iters + non-conformable
    flq1 <- random_FLQuant_generator()
    fixed_dims <- dim(flq1)
    fixed_dims[6] <- fixed_dims[6]+5 
    flq2 <- random_FLQuant_generator(fixed_dim=fixed_dims)
    expect_that(test_FLQuant_FLQuant_addition_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuantAD_addition_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuant_addition_assignment_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuant_FLQuant_addition_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuant_addition_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuantAD_FLQuantAD_addition_operator(flq1, flq2), throws_error())
    expect_that(test_FLQuant_FLQuantAD_addition_operator(flq1, flq2), throws_error())
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
    test_FLQuant_equal(test_sweep_multADAD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_multDD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_multADD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_multDAD(flq1, flq2), flqout)
    # Division
    flqout <- flq1 %/% flq2
    test_FLQuant_equal(test_sweep_divADAD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_divDD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_divADD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_divDAD(flq1, flq2), flqout)
    # Plus
    flqout <- flq1 %+% flq2
    test_FLQuant_equal(test_sweep_plusADAD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_plusDD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_plusADD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_plusDAD(flq1, flq2), flqout)
    # Minus
    flqout <- flq1 %-% flq2
    test_FLQuant_equal(test_sweep_minusADAD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_minusDD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_minusADD(flq1, flq2), flqout)
    test_FLQuant_equal(test_sweep_minusDAD(flq1, flq2), flqout)
    # Dimension match - OK
    flqout <- flq1 %*% flq1
    test_FLQuant_equal(test_sweep_multADAD(flq1, flq1), flqout)
    # Dimension mismatch - fails
    index <- round(runif(1,min=1,max=6))
    dim1[index] <- round(runif(1,min=3,max=5))
    dim2[index] <- dim1[index] - 1
    flq1 <- random_FLQuant_generator(fixed_dims=dim1)
    flq2 <- random_FLQuant_generator(fixed_dims=dim2)
    expect_error(test_sweep_multADAD(flq1, flq2))
}

