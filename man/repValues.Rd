\name{repValues}
\alias{repValue}
\alias{repValues}
\title{ Reportable values }
\description{ Compute reportable value(s).}
\usage{
repValues(object, ...)
repValue(X)
}
\arguments{
  \item{object}{ A list of matrices (\code{X}):
               Computed by \code{\link{relFits}}. }
  \item{X}{ A matrix with "RecovPct":
            Computed by \code{\link{relFit}}. }
  \item{\dots}{ additional arguments. }
}
\details{
The geometric mean of the "Recovery procent" of the matrix \code{X}
with the "responces" within the "linear part" of the curve:

The geometric mean of \code{X[,"RecovPct"]}
with \code{X[,"ok.lb"]} and \code{X[,"ok.ub"]} equal one.
}
\seealso{ See also \code{\link{relFit}} and \code{\link{relFits}}. }
\value{A numeric or list of numerics.}
\author{ Jens Henrik Badsberg }
\keyword{ package }
% \examples{ }
