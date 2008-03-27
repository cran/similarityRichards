\name{simFit}
\alias{simFit}
\title{ Relative backfitted values for one sample in one experiment }
\description{
 Calculates the relative backfitted values for one sample
 in one 'experiment', fitted by Richards curves.
 }
\usage{
simFit(Smp = NULL, fitSmp = NULL, Ref = NULL, fitRef = NULL, 
       ny = 0, Sample = TRUE, atSample = TRUE, indepName = "ARGX", 
       minlogx = min(log(Smp[, indepName]) / log(g)),
       maxlogx = max(log(Smp[, indepName]) / log(g)), 
       g = exp(1), length.out = 11,
       LogX = seq(minlogx, maxlogx, length.out = length.out),
       FUN = NULL, applyFUN2X = function(X, FUN, ...) FUN(X, ...), ...) 
}
\arguments{
  \item{Smp}{ the dataset of the \code{SAMPLE} for \code{FUN}. }
  \item{FUN}{ a function estimating the model for the \code{SAMPLE}.
              This function is only called if the argument
              \code{fitSmp} is not given. }
  \item{fitSmp}{ an optional fit of the \code{SAMPLE}. }
  \item{Ref}{ the dataset of the \code{REFERENCE} for \code{FUN}. 
              This dataset have only to be given, if not \code{fitRef}
              is not given. }
  \item{fitRef}{ an optional fit of the \code{REFERENCE}. }
  \item{ny}{ a (optional) numerical value. This value has to given,
             if \code{ny} is not fitted in the model. }
  \item{Sample}{ a boolean, if \code{TRUE} then the \code{SAMPLE} curve
                 determins the values of the dependent variable to
                 find backfitted values for, else the \code{REFERENCE}. }
  \item{atSample}{ a boolean, if \code{TRUE} then the the 'curves' are 
                   'adjusted to intersect' at 'x50' for the \code{SAMPLE},
                   else at 'x50' for the \code{REFERENCE}. }
  \item{indepName}{ a text string with the name of the independent variable. }
  \item{minlogx}{ a numeric value with the minimum of the independent value. }
  \item{maxlogx}{ a numeric value with the maximum of the independent value. }
  \item{g}{ an optional an optional numeric value, the base of the logarithm. }
  \item{length.out}{ a integer value, the number of points to find
                     backfitted values for. }
  \item{LogX}{ an optional numeric vector,
              the values of the independent variable. }
  \item{applyFUN2X}{ a function applied to each dataset,
                     retrieving starting value and fixing outliers,
                     and then calling the function \code{FUN} to
                     estimate the model for the modified dataset. 
                     This function is only called if the arguments
                     \code{fitSmp} and \code{fitRef} are not given. }
  \item{\dots}{ additional arguments. }
}
\details{
The relative back fitted values are computed as follow
(for the one sample in one experiment):

The dependent variable (\code{'y'}) is computed by the estimated 
Richards function for the \code{SAMPLE} in points \code{'x'},
the exponential of \code{LogX}, possible \code{length.out} equidistant
points from \code{minlogx} to \code{maxlogx} for the logarithm
(with base \code{g}) of the independent variable (\code{indepName}).

(This is if \code{Sample} is \code{TRUE}, else the dependent variable is found
for the same points by the estimated Richards function for the \code{REFERENCE}.)

For each of these values \code{y} of the dependent variable
the relative backfitted value is then computed as follows:

The two values \code{'x.sample'} and \code{'x.reference'}
of the independent variable are found such that
the estimated Richards functions have values \code{y},
(by the inverse of the Richards functions).

The ratio \code{x.reference} by \code{x.sample} is then computed.

This ratio is then adjusted by the same ratio at \code{x50} for the
\code{SAMPLE} at \code{x50} for the \code{REFERENCE} if not \code{atSample}).
}
\value{Matrix with relevant values.}
\author{ Jens Henrik Badsberg }
\keyword{ package }
