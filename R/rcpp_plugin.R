# Automatically registers a plugin for use with
# Rcpp::cppFunction

## REGISTER RCppPlugin
##' Make the plug in for inline Cxx
##'
##' Make the plug in for inline Cxx
##' @param ... I have no idea.
inlineCxxPlugin <- Rcpp::Rcpp.plugin.maker(
  include.before = "#include <FLasher.h>", 
    ## FIND dll/so file at pkg root
  libs = paste0(find.package('FLasher'), "/libs/", .Platform$r_arch,"/FLasher", .Platform$dynlib.ext),
  package = "FLasher"
)
