\name{simFits}
\alias{simFits}
\title{ Relative backfitted values for a list of experiments }
\description{
 The relative backfitted values are calculated,
 with fitting the Richards curves to each sample,
 for a list of list of samples. }
\usage{
simFits(list, FUN,
              applyFUN2X = function(X, FUN, ...) FUN(X, ...),
              refName = "Ref", ...)
}
\arguments{
  \item{list}{ a list of list of datasets.
              (The double 'list of' is not a typo!) }
  \item{FUN}{ a function for estimating the model for each dataset. }
  \item{applyFUN2X}{ a function applied to each dataset of the
                     \code{REFERENCE}, retrieving starting value and fixing
                     outliers, and then calling the function \code{FUN} to
                     estimate the model for the modified dataset of the
                     \code{REFERENCE}. }
  \item{refName}{ a text string, naming the \code{REFERENCE} in
                  the list of datasets for each item in \code{list}. }
  \item{\dots}{ additional arguments, to \code{applyFUN2X}. }
}
\details{ 
The relative backfitted values are calculated
(with computation of the fits
of the Richards curves for the individual samples),
for a list of list of samples. 

The list of list is defined as follows:
a list of datasets (each for a sample),
in a list of experiments.

For each sample different from the \code{REFERENCE}
in an experiment (the sublist),
the sublist the item in the argument list,
the relative backfitted values are calculated by \link{simFit}.
}
\value{ List of matrices with relevant values, see \link{simFit}. }
\author{ Jens Henrik Badsberg }
\keyword{ package }
