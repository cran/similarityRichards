\name{sub2simFits}
\alias{sub2simFits}
\title{ Relative backfitted values from fits of Richards curves
        for one 'experiment' }
\description{
 Compute relative backfitted values from the data and
 the fits of Richards curves for one 'experiment'.
}
\usage{
sub2simFits(list, fits, refName = "Ref", ...)
}
\arguments{
  \item{list}{ a list of datasets. }
  \item{fits}{ a list of fits. }
  \item{refName}{ a text string, naming the \code{REFERENCE} in
                  the list of datasets for each item in \code{list}. }
  \item{\dots}{ additional arguments. }
}
\details{
 Compute relative backfitted values from the data and
 the fits of Richards curves for one 'experiment'.
 The argument lists \code{list} and \code{fits} are assumed to match.
}
\value{ List of matrices with relevant values.}
\author{ Jens Henrik Badsberg }
\keyword{ package }
