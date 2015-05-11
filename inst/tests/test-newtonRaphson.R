context("Test Newton Raphson functions")

test_that("euclid_norm",{
    size_x <- runif(1, min=5, max = 100) 
    x <- rnorm(size_x)
    expect_that(test_euclid_norm(x), equals(sqrt(sum(x^2))))
})

test_that("Newton-Raphson tests",{
    # Simplest test - solution is 2 or -2/3
    max_iters <- 50
    indep_min <- -1000
    indep_max <- 1000
    tol <- .Machine$double.eps ^ 0.5   
    initial <- rnorm(1, sd=1)
    fit <- test_NR1(initial, max_iters, indep_min, indep_max, tol)
    expect_that((abs((fit$x/2)-1) < tol) | (abs((fit$x/(-2/3))-1) < tol) , is_true())
    expect_that(fit$out, equals(1L))
    # If indep min is set 0 - we hit min barrier
    indep_min <- 0
    initial <- 0.1
    fit <- test_NR1(initial, max_iters, indep_min, indep_max, tol)
    expect_that(fit$out, equals(-2L))
    expect_that(fit$x, equals(indep_min))
    # If indep max is too small
    indep_min <- -100
    indep_max <- 1
    initial <- 0.9
    fit <- test_NR1(initial, max_iters, indep_min, indep_max, tol)
    expect_that(fit$out, equals(-3L))
    expect_that(fit$x, equals(indep_max))

    # 2D test 
    max_iters <- 50
    initial <- abs(rnorm(2, sd=2))
    indep_min <- -1000
    indep_max <- 1000
    fit <- test_NR2(initial, max_iters, indep_min, indep_max, tol)
    y1 <- fit$x[1]^2 + fit$x[2]^2 - 4
    y2 <- fit$x[1]^2 - fit$x[2] + 1
    expect_that(c(y1,y2), equals(c(0.0,0.0)))
    expect_that(fit$out, equals(1L))

    # Check multiple iters

})

