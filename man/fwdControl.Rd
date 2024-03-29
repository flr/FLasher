% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R, R/generics.R, R/utilities.R,
%   R/constructors.R
\docType{class}
\name{fwdControl}
\alias{fwdControl}
\alias{fwdControl-methods}
\alias{fwdControl-class}
\alias{G}
\alias{fwdControl,data.frame,array-method}
\alias{fwdControl,data.frame,numeric-method}
\alias{fwdControl,data.frame,missing-method}
\alias{fwdControl,list,missing-method}
\alias{fwdControl,list,list-method}
\alias{fwdControl,missing,missing-method}
\alias{fwdControl,FLQuant,missing-method}
\title{A class for the targets and limits of a fishery and stock projection.}
\usage{
fwdControl(target, iters, ...)

G(...)

\S4method{fwdControl}{data.frame,array}(target, iters, ...)

\S4method{fwdControl}{data.frame,numeric}(target, iters, ...)

\S4method{fwdControl}{data.frame,missing}(target, iters, ...)

\S4method{fwdControl}{list,missing}(target, iters, ...)

\S4method{fwdControl}{list,list}(target, iters, ...)

\S4method{fwdControl}{missing,missing}(target, iters, ...)

\S4method{fwdControl}{FLQuant,missing}(target, quant, ...)
}
\arguments{
\item{target}{The target. Can be a data.frame, a list or missing.}

\item{iters}{target The iters. Can be an array, a numeric or missing.}

\item{...}{Something}

\item{quant}{name of target to assign 'FLQuant' to, 'character'}
}
\description{
The desired targets, limits and time steps used in fishery projections can be
specified by creating an object of class \code{fwdControl}.

Constructor for fwdControl objects.
Bare bones man pages. Better to look at the vignettes and tutorials.
}
\details{
\link{...}
}
\section{Slots}{

\describe{
\item{\code{target}}{The table of quantities and time steps used as target, \emph{data.frame}.}

\item{\code{iters}}{The values and limits for each target quantity and time step, \emph{array}.}

\item{\code{FCB}}{The matrix describing which FLCatch of which FLFishery catches which FLBiol. A \emph{matrix} with 3 columns: F, C, and B.}
}}

\section{Validity}{
 \describe{
\item{VALIDITY}{Neque porro quisquam est qui dolorem ipsum.}
}
}

\section{Accessors}{

All slots in the class have accessor and replacement methods defined that
allow retrieving and substituting individual slots.

The values passed for replacement need to be of the class of that slot.
A numeric vector can also be used when replacing FLQuant slots, and the
vector will be used to substitute the values in the slot, but not its other
attributes.
}

\section{Constructor}{


A construction method exists for this class that can take named arguments for
any of its slots. All slots are then created to match the requirements of the
class validity. If an unnamed \code{FLQuant} object is provided, this is used
for sizing but not stored in any slot.
}

\examples{

# CREATE targets on fishing mortality ('f') by year

target <- data.frame(year=2000:2010, value=rlnorm(11), quant='f')

fwc <- fwdControl(target=target)

# INSPECT fwdControl object

show(fwc)

# Construct from data.frame and array
fcn <- fwdControl(data.frame(year=2000:2005, quant='f', value=0.5))
# Construct a fwdControl with some targets having multiple Biols, specified using the G() function
fwdControl(list(year=2000:2001, value=200, quant="catch", biol=G(1,2)),
  list(year=2002:2003, value=100, quant="catch", biol=c(1,2)))
# Vector of values by year
fwdControl(data.frame(year=2010:2015, quant="f", value=seq(1, 1.3, length=6)))
# Two targets, with ranges for one
fwdControl(data.frame(year=rep(2010:2015, each=2),
  quant=c("f", "catch"),
  min=c(rbind(NA, 20000)), max=c(rbind(NA, 30000)),
  value=c(rbind(seq(1, 1.3, length=6), NA))))
# Single target value
fwdControl(list(year=2010:2014, quant='catch', value=2900))  
# One value per target (year)
fwdControl(list(year=2010:2014, quant='catch', value=seq(2900, 3500, length=5)))  
# With 40 values (iters) in each target
fwdControl(list(year=2010:2014, quant='catch',
  value=rnorm(200, seq(2900, 3500, length=5))))
# lapply can be used to constructs a list
fwdControl(lapply(2005:2020, function(x) list(quant="catch",
  value=runif(1, 1e5, 1e6), year=x)))
fwdControl(lapply(2005, function(x) list(quant="catch",
  value=runif(1, 1e5, 1e6), year=x)))
# FLQuant, needs 'quant' name
fwdControl(FLQuant(0.2, dimnames=list(year=2000)), quant="fbar")
}
\seealso{
\link{data.frame}
}
\author{
Iago Mosqueira, Finlay Scott - EC JRC.
}
\keyword{classes}
