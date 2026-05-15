context("S4 generics")

test_that("core FLasher generics are defined", {
  expect_true(isGeneric("fwdControl"))
  expect_true(isGeneric("target"))
  expect_true(isGeneric("target<-"))
  expect_true(isGeneric("iters<-"))
  expect_true(isGeneric("fillchar"))
  expect_true(isGeneric("stf"))
  expect_true(isGeneric("FCB<-"))
  expect_true(isGeneric("partialF"))
})
