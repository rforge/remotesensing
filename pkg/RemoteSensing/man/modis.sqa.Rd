\name{modis sqa}

\alias{modis.sqa500a}
\alias{modis.sqa500b}
\alias{modis.sqa500c}
\alias{modis.sqa500d}
\alias{modis.sqa500e}
\alias{modis.sqa500f}
\alias{modis.sqa500g}
\alias{modis.sqa500h}
\alias{modis.sqa500i}
\alias{modis.sqa500j}
\alias{modis.sqa500k}


\title{ MODIS State Quality Assessment Extractor }
\description{
Makes Human-readable images of State Quality Assessment binary bits from MOD09A 500m products (Vermote et al., 2008), this is the proper place to find cloud related information.
Vermote E.F., Kotchenova S.Y., Ray J.P. MODIS Surface Reflectance User's Guide. Version 1.2. June 2008. MODIS Land Surface Reflectance Science Computing Facility. Homepage
}

\usage{
modis.sqa500a(pixel) 
modis.sqa500b(pixel) 
modis.sqa500c(pixel)
modis.sqa500d(pixel)
modis.sqa500e(pixel)
modis.sqa500f(pixel)
modis.sqa500g(pixel)
modis.sqa500h(pixel,bandno)
modis.sqa500i(pixel)
modis.sqa500j(pixel)
modis.sqa500k(pixel) 
}


\arguments{
  \item{pixel}{ ~~Describe \code{nirchan} here~~ }
  \item{bandno}{ ~~Describe \code{chan} here~~ }
}

\details{
 \item{modis.sqa500a}{Cloud State} 
 \item{modis.sqa500b}{cloud shadow} 
 \item{modis.sqa500c}{LAND/WATER FLAG} 
 \item{modis.sqa500d}{AEROSOL QUANTITY} 
 \item{modis.sqa500e}{CIRRUS DETECTED} 
 \item{modis.sqa500f}{Internal Cloud Algorithm Flag} 
 \item{modis.sqa500g}{Internal Fire Algorithm Flag} 
 \item{modis.sqa500h}{MOD35 snow/ice flag} 
 \item{modis.sqa500i}{Pixel adjacent to cloud}
 \item{modis.sqa500j}{BRDF correction performed}
 \item{modis.sqa500k}{Internal Snow Mask}
}
\value{
  
}
\references{ Vermote E.F., Kotchenova S.Y., Ray J.P. MODIS Surface Reflectance User's Guide. Version 1.2. June 2008. MODIS Land Surface Reflectance Science Computing Facility.}
\author{ Yann Chemin }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

}
\keyword{ spatial }

