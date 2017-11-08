# Function to return the location of the FLasher library
# Used by other packages that link to FLasher, for example FLasherTests
# Just need to include in Makevars:
#PKG_LIBS = `$(R_HOME)/bin/Rscript -e "FLasher:::FLasherLib()"` 
FLasherLib <- function(){
    location <- paste0(find.package('FLasher'), "/libs/", .Platform$r_arch,"/FLasher", .Platform$dynlib.ext)
   cat(location) 
}
