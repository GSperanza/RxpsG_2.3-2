% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XPSClass.r
\name{hasFit,XPSCoreLine-method}
\alias{hasFit,XPSCoreLine-method}
\title{hasFit}
\usage{
\S4method{hasFit}{XPSCoreLine}(object)
}
\arguments{
\item{object}{a Core_Line object of class \code{XPSCoreLine}}
}
\description{
method to verify an object of class 'XPSCoreLine'
}
\examples{
\dontrun{
 if (hasFit(test[["C1s"]])) print(length(test[["C1s"]]@Fit$y))
}
}
