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

# Notes for distribution

The built package has a size of about 20 Mb making it rather large to distribute as well as producing a Note when R CMD check is run.
The package size can be reduced to about 10 Mb by removing the Cpp tests (unfortunately, it still triggers the Note from check).
To remove the Cpp tests prior to building and distributing you must:

* Inside the /src folder, remove all \*.cpp files that begin with *tests*, e.g. tests_FLQuant.cpp.
* Inside the /tests/testthat folder, remove all the \*.R files that begin with *test-cpp_*, e.g. test-cpp_FLQuant.R
* Open an R terminal and set the working directory to be the one that has the FLasher source code in it.
* Run these commands (assuming you have the **devtools** package):
    + library(devtools)
    + load_all("FLasher")
    + document("FLasher", roclets="rd")

You can now rebuild the package without the Cpp tests.

