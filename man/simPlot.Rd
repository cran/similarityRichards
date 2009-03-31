\name{simPlot}
\alias{simPlot}
\title{ Plot relative backfitted values for Richards curves
 for several 'samples'}
\description{ Plot relative backfitted values for Richards curves
 for several 'samples', each 'sample' by \link{simPlotOne}. }
\usage{
simPlot(simFits = NULL, ...)
}
\arguments{
  \item{simFits}{ a list of list of \code{simFits}, see \link{simFit}. 
                  (The double 'list of' is not a typo!) }
  \item{\dots}{ additional arguments, to \link{matplot} and \link{simConf}. }
}
\details{ Backfitted values for Richards curves for several 'samples'
 are plotted, each 'sample' by \link{simPlotOne}, in severel plots. }
\value{ List of matrices with the plottet values.}
\author{ Jens Henrik Badsberg }
\keyword{ package }
