\name{fits2relFits}
\alias{fits2relFits}
\title{ Relative potency:
        Relative backfitted values from fits of Richards curves
        for several 'experiments' }
\description{
 Compute relative backfitted values (by \link{relFit}) from the data
 and the fits of Richards curves for several 'experiments'.
}
\usage{
fits2relFits(list, fits, refName = "Ref", ...)
}
\arguments{
  \item{list}{ a list of list of datasets. }
  \item{fits}{ a list of list of fits. }
  \item{refName}{ a text string, naming the \code{REFERENCE} in
                  the list of datasets for each item in \code{list}. }
  \item{\dots}{ \code{\dots} }
}
\details{
 Compute relative backfitted values from (by \link{relFit}) the data
 and the fits of Richards curves for several 'experiments'.
 The argument lists (and there sublists) \code{list} and \code{fits}
 are assumed to match.
}
\seealso{ See also \code{\link{relFit}}. }
\value{ List of matrices with relevant values.}
\author{ Jens Henrik Badsberg }
\keyword{ package }
