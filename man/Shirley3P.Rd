% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XPSCustomBaseline.r
\name{Shirley3P}
\alias{Shirley3P}
\title{Shirley3P three parameter Shirley background}
\usage{
Shirley3P(object, Wgt, limits)
}
\arguments{
\item{object}{XPSSample object}

\item{Wgt}{XPSSample object}

\item{limits}{limits of the XPSSample object}
}
\value{
returns the 3-parameter Shirley background.
}
\description{
Shirley3p is  described by three parameters 
  Bs=30eV^2, Cs=750eV^2, Ds=0.75
  which are needed to describe the
  Universal Cross Section function described by: B*T/(C + T^2)
  and an exponential decay L(E)= (1-exp(-Ds*T))
  where T = E'-E = energy loss
  The Shirley3P background is then described by
  Shirley(E) = Integral[ L(E)*K(E'-E) * J(E)dE' ]
  J(E) = acquired spectrum
  see J. Vegh  J.Electr.Spect.Rel.Phen. 151 (2006) 159
  This function is called by the XPSAnalysisGUI
}
