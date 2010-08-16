\name{calibration}

\alias{dn2ref}
\alias{dn2rad}
\alias{dn2temp}

\title{Radiometric calibration}

\description{
Convert raw Landsat or Aster satellite image values (the "digital number"), to radiance, reflectance or temperature.
}

\usage{
dn2ref(x, filename='', ...)
dn2temp(x, filename='', ...)
dn2rad(x, filename='', ...)
}


\arguments{
  \item{x}{Satellite image object inherting from class 'Landsat'}
  \item{filename}{Optional. Filename for output file of radiance, reflectance or temperature values. 
  If not provided these may be stored in a temporary file}
  \item{...}{Additional arguments to writeRaster, such as the file format, and \code{overwrite=TRUE}}
}

\value{
A callibrated Landsat object.
}

\author{Alice Laborte, Yann Chemin, Robert Hijmans}

\examples{
metafile <- system.file("external/L71129046_04620080329_MTL.txt", package="RemoteSensing")

#Create Landsat object
img <- landsat(metafile)

#calculate reflectance and save output as a RasterStack (ref.stk);
#individual bands are stored as RasterLayers ref_BAND1, ref_BAND2, etc
ref <- dn2ref(img, filename="reflection.tif")

# true color:
# plotRGB(ref, 3,2,1, scale=0.36)
# false color:
# plotRGB(ref, 4,3,2, scale=0.45)

#calculate temperature from thermal bands 
temp <- dn2temp(img, "temp")


}

\keyword{spatial}

