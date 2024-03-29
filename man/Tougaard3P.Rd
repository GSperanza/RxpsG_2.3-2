% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XPSCustomBaseline.r
\name{Tougaard3P}
\alias{Tougaard3P}
\title{Tougaard3P three parameter Togaard Baseline}
\usage{
Tougaard3P(object, limits)
}
\arguments{
\item{object}{XPSSample CoreLine}

\item{limits}{limits of the XPSSample CoreLine}
}
\value{
returns the 3parameter Tougaard background.
}
\description{
Tougaard3P is the three parameter Tougaard Baseline
  Tougaard3p = Integral[ BT/[(C + T^2)^2 + D*T^2] * J(T)dT ]
  J(T) = the measured spectrum
  B*T/[(C + T^2)^2 + D*T^2] = Modified Universal Cross Section      
  T=energy loss = E'-E
  see S. Hajati, Surf. Sci. (2006), 600, 3015
  This function is called by XPSAnalysisGUI
}
