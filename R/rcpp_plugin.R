# Automatically registers a plugin for use with
# Rcpp::cppFunction
# See FLCpp vignette for how it's used

# I don't know how safe this location is - always uses the first
FLasher.so_location <- paste(.libPaths()[1], "/FLasher/libs/FLasher.so",sep="")
inlineCxxPlugin <- Rcpp::Rcpp.plugin.maker(
  include.before = "#include <FLasher.h>", 
  libs = FLasher.so_location,
  package = "FLasher"
)


