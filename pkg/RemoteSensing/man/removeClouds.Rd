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


\references{Irish, R.R., undated, Landsat 7 automatic cloud cover assessment. 
\url{http://landsathandbook.gsfc.nasa.gov/handbook/pdfs/ACCA_SPIE_paper.pdf} 
}

\author{Alice Laborte}

\examples{
setwd(dirname(system.file("external/.", package="RemoteSensing")))
metafile <- "L71129046_04620080329_MTL.txt"

#read Landsat metadata file and get parameters
img <- landsat(metafile)

#calculate reflectance and save output in ref.stk; individual bands are stored as ref_BAND1, ref_BAND2, etc
ref <- dn2ref(img, "ref")

#calculate temperature from thermal bands; output is saved on disk
temp <- dn2temp(img, "temp")
temp1 <- raster(temp,1)

#mask out clouds
newref <- removeClouds (ref, temp1, "masked") 

}

\keyword{spatial}

