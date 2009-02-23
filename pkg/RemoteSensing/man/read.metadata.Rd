\name{read.Metadata}
\alias{readMetadata}
\alias{readLandsatCPF}
\alias{getLandsatParameters}

\title{Read metadata}
\description{
  Read metadata text file and retrieve parameters.
}

\usage{
readMetadata(filename)
readLandsatCPF(filename)
getLandsatParameters(filename) 
}


\arguments{
  \item{filename}{metadata or calibration parameter file (CPF) filename}
}

\value{
  matrix of names and values	for readMetadata and readLandsatCPF
  LandsatImage class		for getLandsatParameters
}

\author{Alice Laborte}

\examples{
metafilename <- "L71129046_04620080329_MTL.txt"
x <- readMetadata(metafilename)
LandsatClass <- getLandsatParameters(metafilename)

}

\keyword{spatial}

