\name{landsat}

\alias{landsat}
\alias{getLandsatCPF}

\title{Create a Landsat type object from metadata}

\description{
\code{landsat} reads Landsat 5/7 metadata text files to retrieve sensor related parameters (date/time of satellite overpass, 
sun elevation angle, sun azimuth angle, etc).
}

\usage{
landsat(filename) 
getLandsatCPF(sensor, filename)
}

\arguments{
  \item{sensor}{....}
  \item{filename}{metadata or calibration parameter file (CPF) filename}
}

\value{
  landsat: LandsatImage object
  readLandsatCPF: file
}

\author{Alice Laborte}

\examples{

metafile <- system.file("external/L71129046_04620080329_MTL.txt", package="RemoteSensing")
#read Landsat metadata file and get parameters
img <- landsat(metafile)

}

\keyword{spatial}

