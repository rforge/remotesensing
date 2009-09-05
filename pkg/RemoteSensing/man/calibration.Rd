\name{calibration}

\alias{dn2ref}
\alias{dn2rad}
\alias{dn2temp}
\alias{rad2ref}
\alias{rad2temp}
\alias{GainBias}
\alias{esun}
\alias{K_landsat}
\alias{getDS}
\alias{getGainBias}

\title{Radiometric calibration}

\description{
  Convert the raw satellite image value (the "digital number"), to radiance, reflectance or temperature.
}

\usage{
dn2ref (SatImgObject, filename)
dn2temp (SatImgObject, filename)
dn2rad(DN, gain, bias)
rad2ref(radiance, ds, sun_elevation, ESUN ) 
rad2temp(radiance_thermal, K)
GainBias(spacecraft, sensor, acquisition_date, product_creation_date) 
esun(spacecraft, sensor) 
K_landsat(spacecraft, sensor)
getDS(doy)
getGainBias(SatImgObject)
}


\arguments{
  \item{SatImgObject}{Satellite image object}
  \item{filename}{filename to output raster stack of reflectance or temperature values}
  \item{DN}{Digital number}
  \item{sun_elevation}{sun elevation in degrees}
  \item{doy}{day of the year}
  \item{ds}{earth to sun distance in astronomical units}
  \item{gain}{band-specific rescaling gain factor, W / (m2 * sr * micrometer) /DN }
  \item{bias}{band-specific rescaling bias factor, W / (m2 * sr * micrometer) }
  \item{radiance}{spectral radiance at the sensor's aperture, W/(m2 * sr * micrometer)}
  \item{ESUN}{mean solar exoatmospheric irradiances, W/(m2 * sr * micrometer)}
  \item{radiance_thermal}{thermal band spectral radiance at the sensor's aperture,  W / (m2 * sr * micrometer)}
  \item{spacecraft}{space craft id: Landsat1, Landsat2, Landsat3, Landsat4, Landsat5, Landsat7, Terra}
  \item{sensor}{sensor: MSS, TM, ETM+, Aster}  
  \item{acquisition_date}{image acquisition date}
  \item{product_creation_date}{image processing date}
  \item{K}{  }
}

\value{

}

\author{Alice Laborte and Yann Chemin}

\examples{

setwd(dirname(system.file("external/.", package="RemoteSensing")))
metafile <- "L71129046_04620080329_MTL.txt"

#read Landsat metadata file and get parameters
img <- landsat(metafile)

#calculate reflectance and save output in ref.stk; individual bands are stored as ref_BAND1, ref_BAND2, etc
ref <- dn2ref(img, "ref")

#calculate temperature from thermal bands; output is saved on disk
temp <- dn2temp(img, "temp")

}

\keyword{spatial}

