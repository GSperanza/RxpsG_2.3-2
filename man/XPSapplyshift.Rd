% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XPSClass.r
\name{XPSapplyshift}
\alias{XPSapplyshift}
\title{XPSapplyshift}
\usage{
XPSapplyshift(object, shift = NULL)
}
\arguments{
\item{object}{XPSCoreLine}

\item{shift}{X-shift value}
}
\value{
'XPSapplyshift' returns an XPSCoreLine energy shifted in any of its components 
  (RegionToFit, BaseLine, Fit Components, Best fit)
}
\description{
Apply the shift value to the X axis of a XPSCoreLine.
  If \code{shift} is NULL the x-axis is set to the original values.
}
