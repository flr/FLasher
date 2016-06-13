# test_fwdSingleFCB.R - DESC
# FLasher/tests/test_fwdSingleFCB.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

library(FLasher)
library(ggplotFL)

# STOCK
data(ple4)

# SR
fsr <- fmle(as.FLSR(ple4, model="bevholt"))

# BIOL
PLE=as(ple4, "FLBiol")
rec(PLE) <- predictModel(model=model(fsr), params=params(fsr))

# BIOLS
biols <- FLBiols(PLE=PLE)

# FISHERY
BT <- as(ple4, 'FLFishery')
names(BT) <- "PLE"

# FISHERIES
fisheries <- FLFisheries(BT=BT)

# HINDCASTING
control <- fwdControl(data.frame(year=2000:2008, quant="catch", value=c(catch(ple4)[,(44:52)]),
  minAge=2, maxAge=6))

control <- fwdControl(data.frame(year=2000:2008, quant="f", value=c(fbar(ple4)[,(44:52)]),
  minAge=2, maxAge=6))

# 
residuals <- FLQuants(PLE=window(residuals(fsr), start=1957))
residuals[[1]][,1]<-1

# FLB + FLF
res <- fwd(biols, fisheries, control, residuals)

res <- fwd(biols, fisheries[[1]], control, residuals)

res <- fwd(biols[[1]], fisheries, control, residuals)

res <- fwd(biols, fisheries, control)

res <- fwd(biols, fisheries, control, residuals=residuals)

#

res <- fwd(ple4, control=control, residuals=residuals[[1]], sr=fsr)

res <- fwd(ple4, control=control, sr=fsr)

res <- fwd(ple4, sr=fsr, catch=FLQuant(1000, dimnames=list(year=1990:1995)))

#
plot(FLQuants(FWD=ssb(res), PLE=ssb(ple4)))

# DEBUG inside fwd()
# test_fwdBiols_as_wrap(biolscpp)
# test_fwdBiol_as_wrap(biolscpp[[1]]$biol)

# CHECK
test_FLCatch_as_wrap(fisheries[[1]][[1]])
test_FLFishery_as_wrap(fisheries[[1]])
test_FLFisheries_as_wrap(fisheries)

test_as_wrap_fwdControl(control)


# FCB

data.frame(f=1,c=1,b=1)

data.frame(f=1,c=c(1,2),b=c(1,2))

inp <- list(list(f=1, c=1, b=2), list(f=2,c=2,b=1))
do.call(rbind, lapply(inp, as.data.frame))


