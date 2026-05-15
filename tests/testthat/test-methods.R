# test-methods.R - DESC
# /test-methods.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# CONTEXT summary(fwdControl) {{{

#context("summary(fwdControl)")

#summary(fwdControl(data.frame(year=rep(2010:2015, each=2), quant=c("f", "catch"),
#  min=c(rbind(NA, 20000)), max=c(rbind(NA, 30000)),
#  value=c(rbind(seq(1, 1.3, length=6), NA)))))
#
#summary(fwdControl(list(year=2000:2005, quant="f", value=1.0012),
#  list(year=2003, quant="catch", value=290)))
#
#summary(fwdControl(list(year=2000:2005, quant="f", value=1.0012),
#  list(year=2003, quant="catch", value=seq(200, 400, length.out=10))))
#
#summary(fwdControl(
#  list(fishery=1, catch=1, biol=1, year=2000:2005, quant="f", value=1.0012),
#  list(fishery=1, catch=1, biol=1,year=2003, quant="catch", value=290)))
#
#summary(fwdControl(list(year=2000:2005, quant="f", value=1.0012),
#  list(year=2003, quant="catch", min=290, max=550)))
#
#summary(fwdControl(
#  list(fishery=1, catch=1, biol=1, year=2000:2005, relYear=1999:2004,
#       quant="f", value=1.0012),
#  list(fishery=1, catch=1, biol=1,year=2003, quant="catch", value=290)))
#
#summary(fwdControl(list(year=2000:2010, quant="catch", value=8,
#  biol=G("ple","sol"), relYear=1999:2009, relBiol=G("ple", "sol")),
#  FCB=FCB(c(F=1,C=1,B=1),c(F=1,B=2,C=2))))

context("fwdControl methods")

test_that("$ and [ accessors work on fwdControl", {
  ctrl <- fwdControl(data.frame(year = 2000:2002, quant = "catch", value = c(10, 20, 30)))

  expect_equal(ctrl$year, 2000:2002)
  expect_equal(ctrl$value, c(10, 20, 30))

  sub <- ctrl[2:3]
  expect_equal(sub$year, 2001:2002)
  expect_equal(dim(iters(sub))[1], 2)
})

test_that("replacement methods update target and iter values", {
  ctrl <- fwdControl(data.frame(year = 2000:2001, quant = "catch", value = c(10, 20)))

  ctrl$quant <- c("fbar", "fbar")
  expect_equal(as.character(ctrl$quant), c("fbar", "fbar"))

  ctrl$value <- c(0.2, 0.3)
  expect_equal(ctrl$value, c(0.2, 0.3))
})

test_that("propagate and merge work for fwdControl objects", {
  ctrl1 <- fwdControl(data.frame(year = 2000:2001, quant = "catch", value = c(10, 20)))
  ctrl2 <- fwdControl(data.frame(year = 2002:2003, quant = "f", value = c(0.2, 0.25)))

  pctrl <- propagate(ctrl1, iter = 3)
  expect_equal(dim(iters(pctrl))[3], 3)

  merged <- merge(ctrl1, ctrl2)
  expect_equal(nrow(target(merged)), 4)
  expect_equal(c(target(merged)$year), 2000:2003)
})

test_that("summary returns table-like output invisibly", {
  ctrl <- fwdControl(data.frame(year = 2000:2001, quant = "catch", value = c(10, 20)))
  out <- summary(ctrl)
  expect_true(is.data.frame(out))
  expect_true(any(grepl("value", names(out))))
})
