\name{Vegetation indices}
\alias{arvi}
\alias{wdvi}
\alias{sr}
\alias{savi}
\alias{pvi}
\alias{ndvi}
\alias{msavi}
\alias{msavi2}
\alias{ipvi}
\alias{gvi}
\alias{gemi}
\alias{gari}
\alias{evi}
\alias{dvi}
\alias{vari}

\title{Vegetation indices}
\description{
  Compute vegetation indices from remote sensing data. 
}

\usage{
arvi(red, nir, blue)
dvi(red, nir)
evi(blue, red, nir)
gvi(blue, green, red, nir, ch5, ch7)
gemi(red, nir)
gari(red, nir, blue, green)
ipvi(red, nir)
msavi(red, nir)
msavi2(red, nir)
ndvi(red, nir)
pvi(red, nir)
sr(red, nir)
savi(red, nir)
vari(blue, green, red)
wdvi(red, nir, slslope = 1)
}


\arguments{
  \item{red}{red channel (band) data}
  \item{nir}{nir channel (band) data}
  \item{blue}{blue channel (band) data}
  \item{green}{green channel (band) data}
  \item{ch5}{fifth channel (band) data}
  \item{ch7}{seventh channel (band) data}
  \item{slslope}{value of soil line slope}  
}

\value{
  vegetation index
}

\author{Yann Chemin}

\examples{
#MODIS NDVI processing example
#Create RasterLayer objects
red <- rasterFromFile( system.file("external/MOD09A1_b1.tif", package="RemoteSensing"))
nir <- rasterFromFile( system.file("external/MOD09A1_b2.tif", package="RemoteSensing"))

#Set NA values before reading
red <- setNAvalue(red, -28672)
nir <- setNAvalue(nir, -28672)

NDVI <- overlay(red, nir, fun=ndvi)

#Plot the raster output
plot(NDVI, col=grey(0:255/255))

#add Philippines country administrative boundary
#require(geodata)
#plot(adm("PHL"), add=T, border="red")
}

\keyword{spatial}

