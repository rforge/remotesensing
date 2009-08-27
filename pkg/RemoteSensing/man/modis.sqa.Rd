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
  \item{pixel}{  describe }
  \item{bandno}{ describe  }
}

\details{
\tabular{rll}{
\tab \code{modis.sqa500a} \tab Cloud State  \cr
 \tab \code{modis.sqa500b} \tab cloud shadow \cr 
 \tab \code{modis.sqa500c} \tab LAND/WATER FLAG \cr
 \tab \code{modis.sqa500d} \tab AEROSOL QUANTITY \cr
 \tab \code{modis.sqa500e} \tab CIRRUS DETECTED \cr
 \tab \code{modis.sqa500f} \tab Internal Cloud Algorithm Flag \cr
 \tab \code{modis.sqa500g} \tab Internal Fire Algorithm Flag \cr
 \tab \code{modis.sqa500h} \tab MOD35 snow/ice flag \cr
 \tab \code{modis.sqa500i} \tab Pixel adjacent to cloud \cr
 \tab \code{modis.sqa500j} \tab BRDF correction performed \cr
 \tab \code{modis.sqa500k} \tab Internal Snow Mask \cr
 }
 
}
\value{
  
}
\references{ Vermote E.F., Kotchenova S.Y., Ray J.P. MODIS Surface Reflectance User's Guide. Version 1.2. June 2008. MODIS Land Surface Reflectance Science Computing Facility.}
\author{ Yann Chemin }
\note{ 
}
\seealso{  }
\examples{

}
\keyword{ spatial }

