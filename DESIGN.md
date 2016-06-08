
# CLASSES

- fwdControl

# ACCESSORS

- target, target<-
- iters, iters<-

# CONSTRUCTORS

- fwdControl(target=data.frame, iters=array)
- fwdControl(target=data.frame, iters=numeric)
- fwdControl(target=data.frame, iters=missing)
- fwdControl(target=list, iters=missing)
- fwdControl(target=missing, iters=missing)

# FWD

- fwd(object, control, ...)

- object: FLStock, FLBiols + FLFisheries, FLBiols + FLFishery, FLBiol + FLFisheries, FLBiol + FLFishery
- sr: FLSR, list
- control: fwdControl, FLQuants
- ...: FLQuant


# METHODS

- show
- [, [<-
- propagate

# PLOT

- plot(fwdControl)

# COERCE

- asFLStock(FLBiol, FLFishery)
- asFLBsFs(FLStock)
- asfwdControl(FLQuant)
- asfwdControl(FLQuants)
