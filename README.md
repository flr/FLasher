# FLasher


install_github("flr/FLCore", ref='NewFLBiol')
install_github("iagomosqueira/FLFishery")
install_github("iagomosqueira/FLasher") 


FLasher provides the necessary classes and methods for carrying out
stochastic and deterministic projections of fisheries, both fish stocks
and fish population plus fleets.

This packages uses the excellent CppAD package for differentiation of C++ Algorithms: http://www.coin-or.org/CppAD/

- PROJECTION

- CLASSES

- METHODS

- MORE INFO

year quant season area unit relYear relSeason relFishery relCatch relBiol minAge maxAge fishery catch biol


min value max


target timestep

# Notes for Windows installation

FLasher is 64 bit only and uses features from C++11.

Use latest Rtools (e.g. for R 3.3.1 use Rtools34).
Put Rtools\bin and Rtools\mingw_64\bin in the path (e.g. add c:\Rtools\bin;c:\Rtools\mingw_64\bin to the path)

Build package as normal at the command line:

- RCMD build FLasher

If you chose 32 bit and 64 bit options during installation of R, when you install FLasher you need to specify 64 bit only using --no-multiarch flag:

- RCMD INSTALL --no-multiarch FLasher_XXX

