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
getLandsatCPF(filename)
}

\arguments{
  \item{filename}{metadata file (MTL)}
}

\value{
  landsat: LandsatImage object
  readLandsatCPF: file
}

\author{Alice Laborte}

\examples{
\dontrun{
metafile <- system.file("external/L71129046_04620080329_MTL.txt", package="RemoteSensing")

cpf <- getLandsatCPF(metafile)
head(cpf)
#read Landsat metadata file and get parameters
img <- landsat(metafile)
}
}

\keyword{spatial}

