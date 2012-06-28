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
  If not provided these may be stored in a temporary file. You can also provide \code{filename='default.tif'}, this will substitute 'default' with filename using metadata in the specified format}
  \item{...}{Additional arguments to writeRaster, such as the file format, and \code{overwrite=TRUE}}
}

\value{
A callibrated Landsat object.
}

\author{Alice Laborte, Yann Chemin, Robert Hijmans, Matteo Mattiuzzi}

\examples{
\dontrun{
metafile <- system.file("external/L71129046_04620080329_MTL.txt", package="RemoteSensing")

#Create Landsat object
img <- landsat(metafile)

# calculate reflectance
ref <- dn2ref(img)
## calculate reflectance and save output as a RasterBrick;
require(rgdal)
# whith default name
ref <- dn2ref(img,filename='default.tif')
ref <- dn2ref(img,filename='default.img')
# list.files()
# with user specified name
ref <- dn2ref(img,"ref.img")
# unlink("ref.img")

# true color:
# plotRGB(ref, 3,2,1, scale=0.36)
# false color:
# plotRGB(ref, 4,3,2, scale=0.45)

# calculate temperature (LST) from thermal bands 
temp <- dn2temp(img)
# plot(temp@thermal)

# or for the generation of a fully callibrated image
ref   <- dn2ref(img)
calib <- dn2temp(ref)
calib # reflectance bands
calib@thermal # LST bands

}

}

\keyword{spatial}

