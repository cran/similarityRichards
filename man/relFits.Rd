\name{relFits}
\alias{relFits}
\alias{relFits-class}
\title{ Relative potency:
        Relative backfitted values - for a list of experiments }
\description{
 For each item (experiment) in the argument list
 the following is computed for each sample in the experiment
 (by \link{relFit}):

 The backfitted values (by the \code{REFERENCE}-curve)
 for the mean of the dependent values of the sample
 at the independent value relative the 'actual' independent values.
 }
\usage{
relFits(object, FUN, 
                applyFUN2X = function(X, FUN, ...) FUN(X, ...), 
                refName = "Ref", ...)
}
\arguments{
  \item{object}{ a list of list of datasets.
                (The double 'list of' is not a typo!) }
  \item{FUN}{ a function for estimating the model for the \code{REFERENCE}
              of each dataset. }
  \item{applyFUN2X}{ a function applied
                     to the \code{REFERENCE} of each dataset,
                     retrieving starting value and fixing outliers, 
                     and then calling the function \code{FUN} to
                     estimate the model for the modified dataset of the
                     \code{REFERENCE}. }
  \item{refName}{ a text string, naming the \code{REFERENCE} in
                  the list of datasets for each item in \code{object}. }
  \item{\dots}{ additional arguments, to \code{applyFUN2X}. }
}
\details{ 
The relative backfitted values are calculated
(with computation of the fits
of the Richards curves for the \code{REFERENCE}),
for a list of list of samples. 

The list of list is defined as follows:
a list of datasets (each for a sample),
in a list of experiments.

For each sample different from the \code{REFERENCE}
in an experiment (the sublist),
the sublist the item in the argument list,
the relative backfitted values are calculated by \link{relFit}.
}
\seealso{ See also \code{\link{relFit}}. }
\value{ List of matrices with relevant values, see \link{relFit}. }
\author{ Jens Henrik Badsberg }
\keyword{ package }
