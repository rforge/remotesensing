\name{readMetadata}

\alias{readMetadata}
\alias{readLandsatCPF}
\alias{landsat}

\title{Read metadata}
\description{
  Read metadata text file and retrieve parameters.
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

setwd(dirname(system.file("external/.", package="RemoteSensing")))
metafile <- "L71129046_04620080329_MTL.txt"

#read Landsat metadata file and get parameters
img <- landsat(metafile)

}

\keyword{spatial}

