% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XPSClass.r
\name{hasRegionToFit,XPSCoreLine-method}
\alias{hasRegionToFit,XPSCoreLine-method}
\title{hasRegionToFit}
\usage{
\S4method{hasRegionToFit}{XPSCoreLine}(object)
}
\arguments{
\item{object}{a Core_Line object of class \code{XPSCoreLine}}
}
\description{
method to verify an object of class 'XPSCoreLine'
}
\examples{
\dontrun{
 if (hasRegionToFit(test[["C1s"]])) print(length(test[["C1s"]]@RegionToFit$x))
}
}
