# plot.R - DESC
# /plot.R

# Copyright European Union, 2018
# Author: Iago Mosqueira (EC JRC) <iago.mosqueira@ec.europa.eu>
#
# Distributed under the terms of the European Union Public Licence (EUPL) V.1.1.

# plot(FLStock, fwdControl) {{{

#' plot method for FLStock, fwdControl
#'
#' @name plot
#' @aliases plot,FLStock,fwdControl-method
#' @docType methods
#' @section Generic function: plot(x, y)
#' @param x FlStock object to plot
#' @param y fwdControl from which to extract year ranges
#' @param fill Colour to fill projection years background
#' @param ... Any other argument to be passed to [ggplotFL::plot]
#' @examples
#' data(ple4)
#' control <- fwdControl(year=2008:2017, quant="f", value=0.3)
#' # No fwd projection took place, simply passing year range
#' plot(ple4, control)

setMethod("plot", signature(x="FLStock", y="fwdControl"),
  function(x, y, fill="#E69F00", ...) {
    
    yrs <- range(y$year)
    
    # CREATE standard plot
    p <- plot(x, ...)

    # GET x variable
    if(as.character(p$mapping$x) == "date") {
      yrs <- range(p$data[with(p$data, year %in% yrs), "date"])
    }
    
    p + geom_vline(xintercept=yrs[1], alpha=0.4) +
      geom_vline(xintercept=yrs[2], alpha=0.2) +
      annotate("rect", xmin = yrs[1], xmax = yrs[2], ymin = -Inf, ymax = Inf,
        fill = fill, alpha=0.1)
  }
) # }}}
