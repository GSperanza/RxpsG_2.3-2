% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XPSClass.r
\name{XPSbaseline}
\alias{XPSbaseline}
\title{XPSbaseline Baseline definition for XPSCoreLine object.}
\usage{
XPSbaseline(
  object,
  bgtype = c("linear", "shirley", "polynomial", "spline"),
  deg = NULL,
  Wgt = NULL,
  splinePoints = list(x = NULL, y = NULL),
  ...
)
}
\arguments{
\item{object}{XPSCoreLine object}

\item{bgtype}{the baseline type. For example linear type \code{"2P.Tougaard"}.}

\item{deg}{degree of the polynomial background}

\item{Wgt}{LinearPolynomial weigth in LPShirley, required parma in 3P. 4P.Tougaard}

\item{splinePoints}{numeric vector containing the points which will be connected by the spline}

\item{...}{other parameters.}
}
\value{
The Object slot \code{Baseline} will be filled with the selected Bsseline
  and it will be displyed. The baseline function returns an object of class
  \code{baseline}. The x is the same as \code{RegionToFit} x coord. The y coord
  are the baseline values.
}
\description{
Calculates the baseline for XPSCoreLine. There is a list of baseline shapes implemented:
  linear, polynomial, spline, Shirley, 2P.Shirley, 3P.Shirley, LP.Shirley,
  2P.Tougaard, 3p.Tougaard, 4P.Tougaard.
  Selection of the appropriate baseline have to be made upon spectral-data properties.
}
