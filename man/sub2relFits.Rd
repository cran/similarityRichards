\name{sub2relFits}
\alias{sub2relFits}
\title{ Relative potency:
        Relative backfitted values from fits of Richards curves
        for one 'experiment' }
\description{
 Compute relative backfitted values (by \link{relFit}) from the data and
 the fits of Richards curves for one 'experiment'.
}
\usage{
sub2relFits(list, fits, refName = "Ref", ...)
}
\arguments{
  \item{list}{ a list of datasets. }
  \item{fits}{ a list of fits. }
  \item{refName}{ a text string, naming the \code{REFERENCE} in
                  the list of datasets for each item in \code{list}. }
  \item{\dots}{ additional arguments. }
}
\details{
 Compute relative backfitted values (by \link{relFit}) from the data and
 the fits of Richards curves for one 'experiment'.
 The argument lists \code{list} and \code{fits} are assumed to match.
}
\seealso{ See also \code{\link{relFit}}. }
\value{ List of matrices with relevant values.}
\author{ Jens Henrik Badsberg }
\keyword{ package }
