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
\description{ Makes Human-readable images of Quality Assessment binary bits from MOD09AQ products.
}

\usage{

modis.qc250a(pixel)
modis.qc250b(pixel)
modis.qc250c(pixel,bandno) 
modis.qc250d(pixel) 
modis.qc250e(pixel)
modis.qc250f(pixel)
modis.qc500a(pixel) 
modis.qc500c(pixel,bandno)
modis.qc500d(pixel)
modis.qc500e(pixel)



}


\arguments{
  \item{pixel}{ ~~Describe \code{nirchan} here~~ }
  \item{bandno}{ ~~Describe \code{chan} here~~ }

}

\details{
modis.qc250a returns Cloud State
modis.qc250b returns Cloud shadow
modis.qc250c returns LAND/WATER FLAG
modis.qc250d returns AEROSOL QUANTITY
modis.qc250e returns CIRRUS DETECTED
modis.qc250f returns Internal Cloud Algorithm Flag
modis.qc500a returns Cloud State
modis.qc500c returns LAND/WATER FLAG
modis.qc500d returns AEROSOL QUANTITY
modis.qc500e returns CIRRUS DETECTED
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

