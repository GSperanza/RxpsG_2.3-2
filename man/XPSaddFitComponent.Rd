% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XPSAddFitComponent.r
\name{XPSaddFitComponent}
\alias{XPSaddFitComponent}
\title{XPSaddFitComponent adds a fit component to the XPSCoreLine}
\usage{
XPSaddFitComponent(
  Object,
  type = "Gauss",
  range_h = 5,
  range_mu = 1.5,
  peakPosition = NULL,
  ...
)
}
\arguments{
\item{Object}{XPSCoreLine object}

\item{type}{name of the fitting function used. Default "Gauss"}

\item{range_h}{the value used to determine \code{min} and \code{max} range
for \code{Intensity}}

\item{range_mu}{the value used to determine \code{min} and \code{max} range
for \code{xCenter}}

\item{peakPosition}{a list with x,y value corresponding the center and height
of the new component. Default is \code{NULL} then the position is asked
trough the plot with the cursor}

\item{...}{to add additional values such as FWHM if needed}
}
\value{
Usually the values \code{xCenter} and \code{Amplitude} are get trough
  a click with the mouse on the plot of the XPSCoreLine. All the parameters
  will be added to the slot \code{Components} of the XPSCoreLine.
}
\description{
XPSaddFitComponent() adds a fit component to the XPSCoreLine object. 
  This function is called after the definition of the baseline (\code{XPSbaseline}). 
  Different types of function could be added as component, see \code{\link{XPSfitAlgorithms}}
  for a list of the implemented functions.
}
\examples{
\dontrun{
	XPSdata[["C1s"]] <- XPSaddFitComponent(XPSdata[["C1s"]], type="Voigt", range_h=3)
}
}
\seealso{
\link{XPSfitAlgorithms}, \link{XPSbaseline}, \link{findXIndex}
}
