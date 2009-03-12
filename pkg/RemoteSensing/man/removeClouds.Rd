\name{removeClouds}
\alias{applyMask2Stack}
\alias{cloudMask}
\alias{removeClouds}

\title{Remove clouds}
\description{
  Create cloud mask and remove clouds from  Landsat TM/ETM+ images.
}

\usage{
removeClouds(refstack, traster, filename)
cloudMask (refstack, traster) 
applyMask2Stack (rstack, mask, filename)
}


\arguments{
  \item{refstack}{RasterStack of reflectance values}
  \item{traster}{Temperature raster: Band61 for Landsat ETM+, Band 6 for Landsat TM calibrated to temperature values in K}
  \item{rstack}{RasterStack to be masked}
  \item{mask}{mask}
  \item{filename}{output filename}
}

\value{
}

\author{Alice Laborte}

\examples{

}

\keyword{spatial}

