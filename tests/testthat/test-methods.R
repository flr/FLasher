# test-methods.R - DESC
# /test-methods.R

# Copyright European Union, 2017
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


# CONTEXT summary(fwdControl) {{{

context("summary(fwdControl)")

summary(fwdControl(data.frame(year=rep(2010:2015, each=2), quant=c("f", "catch"),
  min=c(rbind(NA, 20000)), max=c(rbind(NA, 30000)),
  value=c(rbind(seq(1, 1.3, length=6), NA)))))

summary(fwdControl(list(year=2000:2005, quant="f", value=1.0012),
  list(year=2003, quant="catch", value=290)))

summary(fwdControl(list(year=2000:2005, quant="f", value=1.0012),
  list(year=2003, quant="catch", value=seq(200, 400, length.out=10))))

summary(fwdControl(
  list(fishery=1, catch=1, biol=1, year=2000:2005, quant="f", value=1.0012),
  list(fishery=1, catch=1, biol=1,year=2003, quant="catch", value=290)))

summary(fwdControl(list(year=2000:2005, quant="f", value=1.0012),
  list(year=2003, quant="catch", min=290, max=550)))

summary(fwdControl(
  list(fishery=1, catch=1, biol=1, year=2000:2005, relYear=1999:2004,
       quant="f", value=1.0012),
  list(fishery=1, catch=1, biol=1,year=2003, quant="catch", value=290)))

summary(fwdControl(list(year=2000:2010, quant="catch", value=8,
  biol=G("ple","sol"), relYear=1999:2009, relBiol=G("ple", "sol")),
  FCB=FCB(c(F=1,C=1,B=1),c(F=1,B=2,C=2))))
