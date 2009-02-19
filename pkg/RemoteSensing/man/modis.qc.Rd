\name{modis_qc}

\alias{modis.qc250a}
\alias{modis.qc250b}
\alias{modis.qc250c}
\alias{modis.qc250d} 
\alias{modis.qc250e}
\alias{modis.qc250f}

\title{ MODIS Quality Assessment Extractor }
\description{ Makes Human-readable images of Quality Assessment binary bits from MOD09AQ products.
}

\usage{

modis.qc250a(pixel)
modis.qc250b(pixel)
modis.qc250c(pixel, bandno) 
modis.qc250d(pixel) 
modis.qc250e(pixel)
modis.qc250f(pixel)

}


\arguments{
  \item{pixel}{ ~~Describe \code{nirchan} here~~ }
  \item{bandno}{ ~~Describe \code{chan} here~~ }

}

\details{
 \item{modis.qc250a} returns MODLAND QA bits
 \item{modis.qc250b} returns Cloud State
 \item{modis.qc250c} returns BAND 1/2 data quality
 \item{modis.qc250d} returns Atmospheric Correction performed
 \item{modis.qc250e} returns Adjacency Correction performed
 \item{modis.qc250f} returns Different orbit from 500m
 }
\value{
  
}
\references{ ~put references to the literature/web site here ~ }
\author{ Yann Chemin }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

}
\keyword{ spatial }

