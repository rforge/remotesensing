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
 Makes Human-readable images of Quality Assessment binary bits from MOD09AQ products.
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
 \item{modis.qc250a}{returns MODLAND QA bits}
 \item{modis.qc250b}{returns Cloud State}
 \item{modis.qc250c}{returns BAND 1/2 data quality}
 \item{modis.qc250d}{returns Atmospheric Correction performed}
 \item{modis.qc250e}{returns Adjacency Correction performed}
 \item{modis.qc250f}{returns Different orbit from 500m}
 \item{modis.qc500a}{returns MODLAND QA bits}
 \item{modis.qc500c}{returns BAND 1/2/3/4/5/6/7 data quality} 
 \item{modis.qc500d}{returns Atmospheric Correction performed} 
 \item{modis.qc500e}{returns Adjacency Correction performed}
 }
\value{
  
}
\references{ Vermote E.F., Kotchenova S.Y., Ray J.P. MODIS Surface Reflectance User's Guide. Version 1.2. June 2008. MODIS Land Surface Reflectance Science Computing Facility. }

\author{ Yann Chemin }

\note{ In MOD09A1: It seems that cloud related info is not filled properly in the standard QC (MOD09A1 in this module) since version 3, State-QA 500m images (MOD09A1s in this module) should be used (see Vermote et al., 2008).
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }

\examples{

}
\keyword{ spatial }

