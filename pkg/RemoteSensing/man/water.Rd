\name{water}

\alias{nddi}
\alias{ndwi}
\alias{drought}
\alias{lswi}
\alias{water}
\alias{waterModis}


\title{ Water indices }

\description{
  Identify the presence of water, and related indices from remote sensing data. 
}

\usage{
ndwi(nir, swir2) 
nddi(ndvi, ndwi)
drought(ndvi, ndwi)
lswi(nir, swir) 
water(ndvi, albedo) 
waterModis(ndvi, band7) 
}

\arguments{
  \item{nir}{ Value of the \code{NIR} satellite band }
  \item{swir}{ Value of the \code{SWIR} satellite band }
  \item{swir2}{ Value of the \code{SWIR2} satellite band }
  \item{ndvi}{ NDVI values }
  \item{ndwi}{ NDWI values }
  \item{band7}{ MODIS \code{Band7} satellite band values}
  \item{albedo}{ albedo layer }
}

\value{
  Areas that are likely covered with water
}

\details{
 lswi is the Land Surface Water Index
}

\references{ 
 Roy D.P., Jin Y., Lewis P.E., Justice C.O. (2005). Prototyping a global algorithm for systematic fire-affected area mapping using MODIS time series data.  Remote Sensing of Environment 97:137-162.
 Xiao X., Boles S., Liu J., Zhuang D., Frokling S., Li C.,  Salas W., Moore III B. (2005). Mapping paddy rice agriculture in southern China using multi-temporal MODIS images. Remote Sensing of Environment 95:480-492.
}

\author{ Yann Chemin }

\examples{
	nir <- runif(10)
	swir <- runif(10)
	ndvi <- runif(10)
	band7 <- runif(10)
	albedo <- runif(10)
	
	water1 <- lswi(nir, swir) 
	water2 <- water(ndvi, albedo) 
	water3 <- waterModis(ndvi, band7) 
}

\keyword{ spatial }

