% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\name{compare,FLStock,fwdControl-method}
\alias{compare,FLStock,fwdControl-method}
\alias{compare,fwdControl,FLStock-method}
\alias{compare,FLBiol,fwdControl-method}
\title{Compare the result of a \code{fwd()} run with the defined targets.}
\usage{
\S4method{compare}{FLStock,fwdControl}(result, target, simplify = FALSE)

\S4method{compare}{fwdControl,FLStock}(result, target)

\S4method{compare}{FLBiol,fwdControl}(result, target, fishery, simplify = FALSE)
}
\arguments{
\item{result}{Object returned by the call to fwd()}

\item{target}{fwdControl object with required targets}

\item{simplify}{Return whole table or logical vector only, logical}

\item{fishery}{FLFishery oir FKFisheries object}
}
\value{
A table of comparisons, one for each target, of class data.frame.
}
\description{
A comparison between the objects or objects returned by \code{fwd()} and the
targets and limits set in the \code{fwdControl} object used to run, is returned
by this method.
}
\details{
A comparison is carried out for each row in a \code{fwdControl} object,
that is, for every target or limit.
A \code{data.frame} is returned with columns 'year', 'quant', 'season' and
'unit' if relevant, and 'achieved'. The last is of class \code{logical} and will
have value \code{TRUE} if the target or limits have been achieved for every
iteration, and \code{FALSE} otherwise.
Values are compared using \code{\link[base]{all.equal}}.
}
\examples{
data(ple4)
control <- fwdControl(
 list(quant="fbar", value=0.5, year=1990),
 list(quant="catch", value=1, year=1991, relYear=1990),
 list(quant="catch", min=10000, year=1993, max=100000))
run <- fwd(ple4, sr=predictModel(model=rec~a*ssb*exp(-b*ssb),
  params=FLPar(a=9.16, b=3.55e-6)), control=control)

# Returns the full comparison table
compare(run, control)
# Returns a logical vector
compare(run, control, simplify=TRUE)
}
\seealso{
\code{\link[base]{all.equal}}.
}
\author{
Iago Mosqueira (WMR)
}
\keyword{methods}
