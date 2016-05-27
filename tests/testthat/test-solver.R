context("Test Newton Raphson functions")

test_that("euclid_norm",{
    size_x <- runif(1, min=5, max = 100) 
    x <- rnorm(size_x)
    expect_equal(test_euclid_norm(x), sqrt(sum(x^2)))
})

# Simple quadratic test
test_that("Newton-Raphson quadratic tests",{
    quad_solve <- function(coefs){
        a <- coefs[1]
        b <- coefs[2]
        c <- coefs[3]
        out <- c(NA,NA)
        out[1] <-(-b + sqrt(b^2 - 4*a*c))/(2*a)
        out[2] <-(-b - sqrt(b^2 - 4*a*c))/(2*a)
        return(out)
    }
    # Solver params
    max_iters <- 50
    indep_min <- -1000
    indep_max <- 1000
    tol <- .Machine$double.eps ^ 0.5   
    initial <- rnorm(1, sd=1)
    # Coefs
    coefs <- rbind(matrix(c(3,-4,-4),nrow=1),
                   matrix(c(1,4,3),nrow=1),
                   matrix(c(0,0,5),nrow=1), # no solution
                   matrix(c(1,0,-1e10),nrow=1),
                   matrix(c(1,0,-1e-10),nrow=1)
                   ) 
    # Test 1: 3x^2 - 4x - 4 
    fit1 <- test_NR_quad_iters(coefs[1,,drop=FALSE], initial, max_iters, indep_min, indep_max, tol)
    out1 <- quad_solve(coefs[1,])
    expect_true(any(abs(fit1$x- out1) < tol))
    expect_true(fit1$success_code==1)
    # Test 2: x^2 + 4x + 3 
    fit2 <- test_NR_quad_iters(coefs[2,,drop=FALSE], initial, max_iters, indep_min, indep_max, tol)
    out2 <- quad_solve(coefs[2,])
    expect_true(any(abs(fit2$x- out2) < tol))
    expect_true(fit2$success_code==1)
    # Test 3: No solution! 0x^2 + 0x + 5 as indep x has no impact on y
    # What should it do?
    max_iters <- 10
    fit3 <- test_NR_quad_iters(coefs[3,,drop=FALSE], initial, max_iters, indep_min, indep_max, tol)
    expect_true(fit3$success_code==-1)
    # Test 4: x^2 - 1e10. Solution is outside max limits
    max_iters <- 50
    fit4 <- test_NR_quad_iters(coefs[4,,drop=FALSE], 1.0, max_iters, indep_min, 100, tol)
    out4 <- quad_solve(coefs[4,])
    expect_true(fit4$success_code == -3)
    # Test 5: x^2 - 1e-10. Solution is outside min limits
    fit5 <- test_NR_quad_iters(coefs[5,,drop=FALSE], 1.0, max_iters, 1e-3, 1e9, tol)
    out5 <- quad_solve(coefs[5,])
    expect_true(fit5$success_code == -2)
    # Test 5: Do everyone altogether!
    max_iters <- 50
    fit6 <- test_NR_quad_iters(coefs, 1.0, max_iters, -1e9, 1e9, tol)
    expect_true(any(abs(fit6$x[1]- out1) < tol))
    expect_true(any(abs(fit6$x[2]- out2) < tol))
    expect_true(any(abs(fit6$x[4]- out4) < tol))
    expect_true(any(abs(fit6$x[5]- out5) < tol))
    expect_true(all(fit6$success_code[c(1,2,4,5)] == 1))
    expect_true(fit6$success_code[3]==-1)
})

# Solve a simple simultaneous equation
test_that("Newton-Raphson 2D tests",{
    # 3D test 
    tol <- .Machine$double.eps ^ 0.5   
    max_iters <- 50
    indep_min <- -1000
    indep_max <- 1000
    # linear equation
    A <- matrix(data=c(1, 2, 3, 2, 5, 9, 5, 7, 8), nrow=3, ncol=3, byrow=TRUE)    
    b <- matrix(data=c(20, 100, 200), nrow=3, ncol=1, byrow=FALSE)
    out <- round(solve(A, b), 3)
    initial <- c(1,1,1)
    fit <- test_NR_linear(c(1,1,1), cbind(A,-b), max_iters, indep_min, indep_max, tol) 
    expect_true(all(abs(fit$x - out) < tol))
    expect_true(fit$success_code == 1)
    # If put tight bounds on - interesting the others fit OK
    fit <- test_NR_linear(c(1,1,1), cbind(A,-b), max_iters, indep_min, 150, tol) 
    expect_true(fit$success_code == -3)
    # If put tight bounds on
    fit <- test_NR_linear(c(1,1,1), cbind(A,-b), max_iters, 0, indep_max, tol) 
    expect_true(fit$success_code == -2)
    # Too few iterations
    fit <- test_NR_linear(c(1,1,1), cbind(A,-b), 1, indep_min, indep_max, tol) 
    expect_true(fit$success_code == -1)
})

