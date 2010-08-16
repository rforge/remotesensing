\name{removeClouds}

\alias{removeClouds}

\title{Remove clouds}
\description{
Remove clouds from a Landsat TM/ETM+ image.
}

\usage{
removeClouds(x, filename='', ...)
}

\arguments{
  \item{x}{Landsat object with callibrated reflectance and thermal values}
  \item{filename}{output filename (optional) }
  \item{...}{additional arguments passed on to writeRaster}  
}


\references{Irish, R.R., undated, Landsat 7 automatic cloud cover assessment. 
\url{http://landsathandbook.gsfc.nasa.gov/handbook/pdfs/ACCA_SPIE_paper.pdf} 
}

\value{
RasterLayer
}

\author{Alice Laborte}

\examples{

#create Landsat object from metadata file (that has the necessary parameters)
metafile <- system.file("external/L71129046_04620080329_MTL.txt", package="RemoteSensing")
img <- landsat(metafile)

#calculate reflectance
img <- dn2ref(img)
#calculate temperature
img <- dn2temp(img)

#mask out clouds
masked <- removeClouds(img)
}

\keyword{spatial}

