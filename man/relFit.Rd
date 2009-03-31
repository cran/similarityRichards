\name{relFit}
\alias{relFit}
\alias{relFit-class}
\title{ Relative potency:
        Backfitted values relative 'actual concentration'
        (for one sample in one experiment) }
\description{
 Calculates the backfitted values (from an 'independent' Richards curve)
 relative the independent variable (for one sample in one 'experiment').

 For each value of the independent variable 
 the (reciprocal) ratio is computed
 of
 the (actual) independent value
 to
 the independent value - for which the value
 of the fitted curve (of an other sample) is equal 
 to the mean of the dependent values at the (actual) independent value.
 }
\usage{
relFit(X, fit = NULL, ny = 0, respName = "SIGNAL", indepName = "ARGX",
       FUN = NULL, applyFUN2X = function(X, FUN, ...) FUN(X, ...),
       Smin = -1, Smax = 10, ...)
}
\arguments{
  \item{X}{ the dataset with \code{indep} and \code{respName} 
            (and for \code{FUN}). }
  \item{fit}{ a (optional) fit. 
              \code{ summary(fit)$parameters[, "Estimate"]}
              should return the parameters of a Richards curve. }
  \item{ny}{ a (optional) numerical value. This value has to given,
             if \code{ny} is not fitted in the model. }
  \item{respName}{ a text string with the name of the dependent variable. }
  \item{indepName}{ a text string with the name of the independent variable. }
  \item{FUN}{ a function estimating a model for the dataset.
              This function is only called if the argument
              \code{fit} is not given. }
  \item{applyFUN2X}{ a function applied to the dataset,
                     retrieving starting value and fixing outliers,
                     and then calling the function \code{FUN} to
                     estimate the model for the modified dataset. 
                     This function is only called if the argument
                     \code{fit} is not given. }
  \item{Smin}{ a numeric: lower bound of "linear part". }
  \item{Smax}{ a numeric: upper bound of "linear part". }
  \item{\dots}{ additional arguments. }
}
\details{
The backfitted values relative the actual concentrations
are computed as follow (for the one sample in one experiment):

For each value of the independent variable (\code{indepName})
the observed mean \code{y} of the dependent variable (\code{respName})
is found from the dataset \code{X}.  

For each of these values \code{y} of the dependent variable
the backfitted value is then computed as follows from
the Richards curve with parameters of \code{fit}:

The independent values are found such that
the estimated Richards functions have values \code{y},
(by the inverse of the Richards functions).

The ratio of the backfitted values to the independent variable
(\code{indepName}) is then finally computed.

"ok.lb" and "ok.ub" are set to 1 if the 
independent variable (\code{indepName}) is respectively
above the lower limit or above the upper limit
of the linear part of a 4-parameter logistic curve.
}
\references{
J. L. Sebaugh, P. D. McCray:
Defining the linear portion of a sigmoid-shaped curve: bend points.
Pharmaceutical Statistics 2003, Volume 2 Issue 3, Pages 167 - 174.
}
\value{Matrix with relevant values.}
\author{ Jens Henrik Badsberg }
\keyword{ package }
