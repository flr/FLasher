# fwd.R - DESC
# /fwd.R

# Copyright European Union, 2016
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@jrc.ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# INPUT

library(FLasher)


tnp <- make_test_operatingModel1()


inp <- make_test_operatingModel0()

biol <- inp$biols

biol <- lapply(biol, function(x) {
  x$biol@rec@model <- do.call(x$srr_model, list())$model
  x$biol@rec@params <- as(x$srr_params, 'FLPar')
  x$biol@rec@.Data <- FLQuants()
  return(x)
})

residuals <- FLQuants(lapply(biol, '[[', 'srr_residuals'))
mult <- lapply(biol, '[[', 'srr_residuals_mult')

biol <- FLBiols(lapply(biol, '[[', 1))

fleet <- inp$fisheries

control <- inp$fwc
target(control) <- transform(target(control), year=year + 1970)
target(control)$maxAge <- 10

target(control)$fishery <- target(control)$fishery - 1



res <- fwd2(biol, fleet, control, residuals)

# --

data(ple4sex)

c1 <- c2 <- c3 <- as(ple4, "FLCatch")

landings.n(c2) <- landings.n(c2) / 2

f1 <- FLFishery(name="F1", A=c1, B=c2)

f2 <- FLFishery(name="F2", B=c2, C=c3)

data.frame(year=rep(


fwdControl(list(year=1980, quant="catch", value=23000))



# --


