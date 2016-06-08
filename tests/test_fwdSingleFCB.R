# test_fwdSingleFCB.R - DESC
# /test_fwdSingleFCB.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.


library(FLasher)
data(ple4)

biols <- FLBiols(PLE=as(ple4, "FLBiol"))

fsr <- fmle(as.FLSR(ple4, model="bevholt"))

rec(biols[[1]]) <- predictModel(model=fsr@model, params=fsr@params)

fisheries <- FLFisheries(BT=FLFishery(name="BT", desc="BT",
  PLE=as(ple4, "FLCatch")))
effort(fisheries[[1]])[] <- 1
fisheries[[1]]@hperiod[1,] <- 0
fisheries[[1]]@hperiod[2,] <- 1

name(fisheries[[1]]) <- "BT"
desc(fisheries[[1]]) <- "BT"
name(fisheries[[1]][[1]]) <- "PLE"
desc(fisheries[[1]][[1]]) <- "PLE"
name(biols[[1]]) <- "PLE"
desc(biols[[1]]) <- "PLE"
fisheries@desc <- "BT"

catch.q(fisheries[[1]][[1]]) <- FLPar(alpha=0.2, beta=0.3)

control <- fwdControl(data.frame(year=1980:1982, quant="catch", value=10000))

FCB <- array(1, dim=c(1,3))
colnames(FCB) <- c("F","C","B")
control@FCB <- FCB

control@target$minAge <- 2
control@target$maxAge <- 6
control@target$catch <- as.numeric(NA)
control@target$fishery <- control@target$biol <- 1

residuals <- FLQuants(PLE=log(rec(ple4)))

res <- fwd(biols, fisheries, control, residuals)

# 
test_fwdBiols_as_wrap(biols)
test_fwdBiol_as_wrap(biols[[1]]$biols)
test_FLFishery_as_wrap(fisheries[[1]])
test_FLFisheries_as_wrap(fisheries)
test_as_wrap_fwdControl(control)
