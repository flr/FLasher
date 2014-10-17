context("Test Newton Raphson functions")

test_that("euclid_norm",{
    size_x <- runif(1, min=5, max = 100) 
    x <- rnorm(size_x)
    expect_that(test_euclid_norm(x), equals(sqrt(sum(x^2))))
})

test_that("Newton-Raphson tests",{
    # Simplest test - solution is 2 or -2/3
    max_iters <- 50
    max_limit <- 100
    tol <- .Machine$double.eps ^ 0.5   
    initial <- rnorm(1, sd=1)
    fit <- test_NR1(initial, max_iters, max_limit, tolerance)
    expect_that((abs((fit$x/2)-1) < tol) | (abs((fit$x/(-2/3))-1) < tol) , is_true())

    #expect_that(fit$out, is_identical_to(0L))
    # Check max iters
    #fit <- test_NR1(-10, 2, max_limit, tolerance)
    #expect_that(fit$out, is_identical_to(1L))
    ## Check max limit
    #fit <- test_NR1(200, max_iters, max_limit, tolerance)
    #expect_that(fit$out, is_identical_to(2L))
    # tolerance test

    # 2D test 
    max_iters <- 50
    max_limit <- 1e9 # Crank up the limit - it's OK for this example
    tolerance <- 1e-12
    initial <- abs(rnorm(2, sd=2))
    fit <- test_NR2(initial, max_iters, max_limit, tolerance)
    y1 <- fit$x[1]^2 + fit$x[2]^2 - 4
    y2 <- fit$x[1]^2 - fit$x[2] + 1
    expect_that(c(y1,y2), equals(c(0.0,0.0)))


    #expect_that(fit$out, is_identical_to(0L))
})

