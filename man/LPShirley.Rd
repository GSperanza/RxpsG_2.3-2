% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/XPSCustomBaseline.r
\name{LPShirley}
\alias{LPShirley}
\title{LPShirley combination of a Shirley and linear Baselines}
\usage{
LPShirley(object, mm, limits)
}
\arguments{
\item{object}{CoreLine where to apply LPshirley function}

\item{mm}{coefficient of the linear BKG component}

\item{limits}{limits of the XPSSample CoreLine}
}
\value{
returns the LPShirley background
}
\description{
LPShirley generates a Baseline which is defined by
  LinearPolynomial * Shirley Baseline see Practical Surface Analysis Briggs and Seah Wiley Ed.
  Applies in all the cases where the Shirley background crosses the spectrum
  causing the Shirley algorith to diverge.
  In LPShirley firstly a linear background subtraction is performed to recognize 
  presence of multiple peaks. Then a Modified Bishop polynom weakens the Shirley cross section
  Bishop polynom:  PP[ii]<-1-m*ii*abs(dX)   dX = energy step, m=coeff manually selected
  in LPShirley the classical Shirley expression is multiplied by PP:
  S(E)i) =  Shirley(E) = Integral[ S(E)i-1)* PP(E)* J(E)dE' ]
  S(E)i), S(E)i-1) are the Shirley at step i and i-1
  J(E) = acquired spectrum
  This function is called by XPSAnalysisGUI
}
