\name{readMetadata}

\alias{readMetadata}
\alias{readLandsatCPF}
\alias{landsat}

\title{Read Landsat 5/7 metadata text files}
\description{
  Read metadata text file and retrieve sensor related parameters (date/time of satellite overpass, sun elevation angle, sun azimuth angle, etc).
}

\usage{
readMetadata(filename)
readLandsatCPF(sensor, filename)
landsat(filename) 
}


\arguments{
  \item{sensor}{....}
  \item{filename}{metadata or calibration parameter file (CPF) filename}
}

\value{
  readMetadata and readLandsatCPF: matrix of names and values	
  landsat: LandsatImage object
}

\author{Alice Laborte}

\examples{

metafile <- system.file("external/L71129046_04620080329_MTL.txt", package="RemoteSensing")

#read Landsat metadata file and get parameters
img <- landsat(metafile)

}

\keyword{spatial}

