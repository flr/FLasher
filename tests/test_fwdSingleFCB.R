# test_fwdSingleFCB.R - DESC
# FLasher/tests/test_fwdSingleFCB.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(FLasher)
library(ggplotFL)
data(ple4)

fsr <- fmle(as.FLSR(ple4, model="bevholt"))

#
PLE=as(ple4, "FLBiol")
rec(PLE) <- predictModel(model=model(fsr), params=params(fsr))

biols <- FLBiols(PLE=PLE)

#



BTPLE=as(ple4, "FLCatch")
name(BTPLE) <- "PLE"
desc(BTPLE) <- "BTPLE"
catch.q(BTPLE) <- FLPar(alpha=c(harvest(ple4)[1,1] / catch.sel(BTPLE)[1,1]), beta=0)

BT=FLFishery(name="BT", desc="BT", PLE=BTPLE)
capacity(BT)[] <- 1
effort(BT)[] <- c((harvest(ple4) / (catch.q(BTPLE)['alpha',] * catch.sel(BTPLE)))[1,])
hperiod(BT)[1,] <- 0
hperiod(BT)[2,] <- 1

fisheries <- FLFisheries(BT=BT)
fisheries@desc <- "BT"

# HINDCASTING
control <- fwdControl(data.frame(year=2000:2008, quant="catch", value=c(catch(ple4)[,(44:52)]),
  minAge=2, maxAge=6))

control <- fwdControl(data.frame(year=2000:2008, quant="f", value=c(fbar(ple4)[,(44:52)]),
  minAge=2, maxAge=6))

# 
residuals <- FLQuants(PLE=window(residuals(fsr), start=1957))

res <- fwd(biols, fisheries, control, residuals)

#
plot(FLQuants(FWD=ssb(res$biols[[1]]), PLE=ssb(ple4)))

# DEBUG inside fwd()
test_fwdBiols_as_wrap(biolscpp)
test_fwdBiol_as_wrap(biolscpp[[1]]$biol)

# CHECK
test_FLCatch_as_wrap(fisheries[[1]][[1]])
test_FLFishery_as_wrap(fisheries[[1]])
test_FLFisheries_as_wrap(fisheries)

test_as_wrap_fwdControl(control)
