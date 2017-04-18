# Automatically registers a plugin for use with
# Rcpp::cppFunction
# See FLCpp vignette for how it's used

# FIND dll/so file at pkg root
.FLasher.so_location <- paste0(find.package('FLasher'), "/libs/", .Platform$r_arch,"/FLasher", .Platform$dynlib.ext)

# REGISTER RCppPlugin
#' Make the plug in for inline Cxx
#'
#' Make the plug in for inline Cxx
#' @param ... I have no idea.
inlineCxxPlugin <- Rcpp::Rcpp.plugin.maker(
  include.before = "#include <FLasher.h>", 
  libs = .FLasher.so_location,
  package = "FLasher"
)


