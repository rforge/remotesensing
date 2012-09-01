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
  If not provided these may be stored in a temporary file. You can also provide \code{filename='default.tif'}, this will substitute 'default' with filename using the scene ID.}
  \item{...}{Additional arguments to writeRaster, such as the file format, and \code{overwrite=TRUE}}
}

\value{
A calibrated Landsat object.
}

\author{Alice Laborte, Yann Chemin, Robert Hijmans, Matteo Mattiuzzi}

\examples{
\dontrun{
metafile <- system.file("external/L71129046_04620080329_MTL.txt", package="RemoteSensing")

#Create Landsat object
img <- landsat(metafile)

# calculate reflectance
ref <- dn2ref(img)
## calculate reflectance and save output as a tmp RasterBrick;
require(rgdal)
# whith filename='default.XXX' name
ref <- dn2ref(img,filename='default.tif') # to current directory
ref <- dn2ref(img,filename='default.img') # to current directory
ref <- dn2ref(img,filename='/path/to/default.tif') # to specified directory

# with user specified name
ref <- dn2ref(img,"ref.img")
# unlink("ref.img")

# true color:
# plotRGB(ref, 3,2,1, scale=0.36)
# false color:
# plotRGB(ref, 4,3,2, scale=0.45)

# calculate temperature (LST) from thermal bands
# here you can also apply the filename='default.tif'
temp <- dn2temp(img)
# plot(temp@thermal)

# or for the generation of a fully calibrated image
ref   <- dn2ref(img)
calib <- dn2temp(ref)
calib # reflectance bands
calib@thermal # LST bands

}

}

\keyword{spatial}

