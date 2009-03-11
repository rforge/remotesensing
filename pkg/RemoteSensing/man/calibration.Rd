\name{calibration}

\alias{dn2ref}
\alias{dn2rad}
\alias{dn2temp}
\alias{rad2ref}
\alias{rad2temp}
\alias{GainBias}
\alias{esun}
\alias{K_landsat}

\title{Radiometric calibration}

\description{
  Convert the raw satellite image value (the "digital number"), to radiance, reflectance or temperature.
}

\usage{
dn2ref (SatImgObject, outfilename)
dn2temp (SatImgObject, outfilename)
dn2rad(DN, gain, bias, lmax, lmin, qcalmax, qcalmin)
rad2ref(radiance, doy, sun_elevation, ESUN ) 
rad2temp(radiance_thermal, SatImgObject)
GainBias(spacecraft, sensor, acquisition_date, product_creation_date) 
esun(spacecraft, sensor) 
K_landsat(spacecraft, sensor)
}


\arguments{
  \item{SatImgObject}{Satellite image object}
  \item{outfilename}{filename to output raster stack of reflectance or temperature values}
  \item{DN}{Digital number}
  \item{gain}{band-specific rescaling gain factor, W/(m\^2 * sr * µm)/DN }
  \item{bias}{band-specific rescaling bias factor, W/(m2 * sr * µm) }
  \item{lmin, lmax}{spectral radiance that is scaled to qcalmin/qcalmax, W/(m2 * sr * µm) }
  \item{qcalmin, qcalmax}{minimum/maximum quantized calibrated pixel value corresponding to lmin/lmax}
  \item{radiance}{spectral radiance at the sensor's aperture, W/(m2 * sr * µm)}
  \item{doy}{day of the year}
  \item{sun_elevation}{sun elevation in degrees}
  \item{ESUN}{mean solar exoatmospheric irradiances, W/(m2 * sr * µm)}
  \item{radiance_thermal}{thermal band spectral radiance at the sensor's aperture,  W/(m2 * sr * µm)}
%-  \item{K}{vector of thermal band calibration constants: K1 is the calibration constant in W/(m2 * sr * µm) , K2 in Kelvin}
  \item{spacecraft}{space craft id: Landsat1, Landsat2, Landsat3, Landsat4, Landsat5, Landsat7, Terra}
  \item{sensor}{sensor: MSS, TM, ETM+, Aster}  
  \item{acquisition_date}{image acquisition date}
  \item{product_creation_date}{image processing date}
}

\value{

}

\author{Alice Laborte and Yann Chemin}

\examples{

#read Landsat metadata file and get parameters
#metafile<-"L71129046_04620080329_MTL.txt"
#LandsatImg <- landsat(metafile)

#convert Band 1 DN to reflectance
#ref <- dn2ref (LandsatImg, 1) 

#convert Band61 DN to temperature in kelvin
#b <- "BAND61"
#temp <- dn2temp(LandsatImg, b)
}

\keyword{spatial}

