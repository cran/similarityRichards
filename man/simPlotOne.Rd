\name{simPlotOne}
\alias{simPlotOne}
\title{ Plot relative backfitted values for Richards curves for one 'sample' }
\description{ Plot relative backfitted values for Richards curves
 for one 'sample'.}
\usage{
simPlotOne(fits, name, indep2conc = function(x) x, ylim = c(min(Z), max(Z)),
           Conf = simConf, main = paste("Backfitted values: ", name),
           sub = paste("Above / below / NA: ",
                       paste("", c(length(which(Z > ylim[2])),
                                   length(which(Z < ylim[1])),
                                   length(which(is.na(Z)))),
                             collapse = " /")),
           xlab = "~ Log(concentration) [Dilution step]",
           ylab = "Relative backfitted concentrations",
           useFitNames = FALSE,
           doPlot = TRUE, ...)
}
\arguments{
  \item{fits}{ a list of list of \code{simFits}, see \link{simFit}. 
                  (The double 'list of' is not a typo!) }
  \item{name}{ a text string, naming the \code{SAMPLE} in
               the list of backfits for each item in \code{fits}. }
  \item{ylim}{ a numeric vector with the range of the y axis. }
  \item{indep2conc}{ a function for transforming the x axis. }
  \item{Conf}{ a function for adding confidence intervals,
                default \link{simConf}. }
  \item{main}{ an overall title for the plot: see \link{title}. }
  \item{sub}{ a sub title for the plot: see \link{title}. }
  \item{xlab}{ a title for the x axis: see \link{title}. }
  \item{ylab}{ a title for the y axis: see \link{title}. }
  \item{useFitNames}{ a boolean: for 'old' mode. }
  \item{doPlot}{ a boolean: if FALSE then no plot is made. }
  \item{\dots}{ additional arguments, to \link{matplot} and \code{Conf},
                see \link{matplot} and \link{simConf}. }

}
\details{ Relative backfitted values for Richards curves for one 'sample'
          are plottet in one plot. }
\value{ Matrix with the plottet values for the 'sample'.}
\author{ Jens Henrik Badsberg }
\keyword{ package }
