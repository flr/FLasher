# Automatically registers a plugin for use with
# Rcpp::cppFunction
# See FLCpp vignette for how it's used

# FIND dll/so file at pkg root
FLasher.so_location <- paste0(find.package('FLasher'), "/libs/FLasher",
	.Platform$dynlib.ext)

# REGISTER RCppPlugin
inlineCxxPlugin <- Rcpp::Rcpp.plugin.maker(
  include.before = "#include <FLasher.h>", 
  libs = FLasher.so_location,
  package = "FLasher"
)


