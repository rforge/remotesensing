\name{removeClouds}
\alias{applyMask}
\alias{cloudMask}

\title{Remove clouds}
\description{
  Create cloud mask for Landsat TM/ETM+ images.
}

\usage{
cloudMask (reflectance, temperature) 
applyMask2Stack (raster, mask)
removeClouds(reflectance, temperature)
}


\arguments{
  \item{reflectance}{RasterStack of reflectance values}
  \item{temperature}{Band61 for Landsat ETM+, Band 6 for Landsat TM}
  \item{raster}{raster layer to be masked}
  \item{mask}{mask}
}

\value{
}

\author{Alice Laborte}

\examples{

}

\keyword{spatial}

