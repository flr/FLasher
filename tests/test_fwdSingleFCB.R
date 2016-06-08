# test_fwdSingleFCB.R - DESC
# /test_fwdSingleFCB.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(FLasher)
data(ple4)

fsr <- fmle(as.FLSR(ple4, model="bevholt"))

#
PLE=as(ple4, "FLBiol")
rec(PLE) <- predictModel(model=model(fsr), params=params(fsr))

biols <- FLBiols(PLE=PLE)

#
PLE=as(ple4, "FLCatch")
catch.q(PLE) <- FLPar(alpha=0.2, beta=0.3)

BT=FLFishery(name="BT", desc="BT", PLE=as(ple4, "FLCatch"))
effort(BT)[] <- 1
hperiod(BT)[1,] <- 0
hperiod(BT)[2,] <- 1

fisheries <- FLFisheries(BT=BT)
fisheries@desc <- "BT"

#
control <- fwdControl(data.frame(year=1980:1982, quant="catch", value=10000,
  minAge=2, maxAge=6))

FCB <- array(1, dim=c(1,3))
colnames(FCB) <- c("F","C","B")
control@FCB <- FCB

residuals <- FLQuants(PLE=log(rec(ple4)))

res <- fwd(biols, fisheries, control, residuals)

#  DEBUG
test_fwdBiols_as_wrap(biols)
test_fwdBiol_as_wrap(biols[[1]]$biols)
test_FLFishery_as_wrap(fisheries[[1]])
test_FLFisheries_as_wrap(fisheries)
test_as_wrap_fwdControl(control)
