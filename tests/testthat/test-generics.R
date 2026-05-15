test_that("FLasher generics are registered", {
  for (g in c("fwdControl", "target", "target<-", "iters<-", "fillchar", "stf", "FCB<-", "partialF")) {
    expect_true(methods::isGeneric(g), info = g)
  }
})
