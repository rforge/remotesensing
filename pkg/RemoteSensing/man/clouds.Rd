\name{removeClouds}
\alias{applyMask2Stack}
\alias{cloudMask}

\title{Remove clouds}
\description{
  Create cloud mask for Landsat TM/ETM+ images.
}

\usage{
cloudMask(reflectance, temperature) 
applyMask2Stack(rstack, mask, outfilename)
}


\arguments{
  \item{reflectance}{RasterStack of reflectance values}
  \item{temperature}{Band61 for Landsat ETM+, Band 6 for Landsat TM}
  \item{rstack}{RasterStack to be masked}
  \item{mask}{mask}
 \item{outfilename}{---}
   
}

\value{
}

\author{Alice Laborte}

\examples{

}

\keyword{spatial}

