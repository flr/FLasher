
# fwdControl

- WRITE validTargets() RCpp function, returns character
- ADD timestep & order check to fwdControl()

# fwd()

- WRITE checkFwdInputs()
- CODE fwd(FLBiols, FLFisheries, fwdControl, ...)
- CODE fwd(FLBiol, FLFishery, fwdControl, ...)
- COEDE fwd(FLStock, fwdControl, ...)
- CODE fwd(FLBiol, FLFisheries, fwdControl, ...)
- THINK about fwd() FLQuant-based interface, fwd(FLBiol, FLFishery, catch=FLQuant)

# Documentation

- ADD roxygen2 pages to all R code
- REVIEW Flash vs. Flasher document
- REVIEW user vignette
- REVIEW technical document

# Data

- FINALIZE SKJ dataset w/NB
- COMPILE data for a ple4+sol4 FLBiols/FLFisheries dataset (JJP)

# Tests

- WRITE testhat tests for fwdControl()
- WRITE testhat tests for fwdControl methods
- TEST fwdControl() w/ FLash data.frames

# Economics

- PROTOTYPE fishery feedback function, fff(fwdControl, ...)
