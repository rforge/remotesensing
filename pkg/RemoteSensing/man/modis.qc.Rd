\name{modis_qc}

\alias{modis.qc250a}
\alias{modis.qc250b}
\alias{modis.qc250c}
\alias{modis.qc250d} 
\alias{modis.qc250e}
\alias{modis.qc250f}
\alias{modis.qc500a}
\alias{modis.qc500c}
\alias{modis.qc500d} 
\alias{modis.qc500e}

\title{ MODIS Quality Assessment Extractor }

\description{
 Makes Human-readable images of Quality Assessment binary bits from MOD09A and MOD09Q products.
}

\usage{
modis.qc250a(pixel)
modis.qc250b(pixel)
modis.qc250c(pixel, bandno) 
modis.qc250d(pixel) 
modis.qc250e(pixel)
modis.qc250f(pixel)
modis.qc500a(pixel)
modis.qc500c(pixel, bandno) 
modis.qc500d(pixel) 
modis.qc500e(pixel)
}

\arguments{
  \item{pixel}{ --- }
  \item{bandno}{ --- }
}

\details{

\tabular{rll}{
\tab \code{modis.qc250a} \tab returns MODLAND QA bits \cr
\tab \code{modis.qc250b} \tab returns Cloud State \cr
\tab \code{modis.qc250c} \tab returns BAND 1/2 data quality \cr
\tab \code{modis.qc250d} \tab returns Atmospheric Correction performed \cr
\tab \code{modis.qc250e} \tab returns Adjacency Correction performed \cr
\tab \code{modis.qc250f} \tab returns Different orbit from 500m \cr
\tab \code{modis.qc500a} \tab returns MODLAND QA bits \cr
\tab \code{modis.qc500c} \tab returns BAND 1/2/3/4/5/6/7 data quality \cr
\tab \code{modis.qc500d} \tab returns Atmospheric Correction performed \cr
\tab \code{modis.qc500e} \tab returns Adjacency Correction performed \cr
}
}


\references{ Vermote E.F., Kotchenova S.Y., Ray J.P. MODIS Surface Reflectance User's Guide. Version 1.2. June 2008. MODIS Land Surface Reflectance Science Computing Facility. }

\author{ Yann Chemin }


\note{ In MOD09A1: It seems that cloud related info is not filled properly in the standard QC (MOD09A1 in this module) since version 3, State-QA 500m images (MOD09A1s in this module) should be used (see Vermote et al., 2008).
}


\keyword{ spatial }

