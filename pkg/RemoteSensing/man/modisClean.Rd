\name{modisClean}

\alias{modisClean}

\title{ Clean Modis files}

\description{
Remove cloud cover, snow and permanent water and missing values from raw Modis files using the MODIS quality control flags 
}

\usage{
modisClean(inpath, outpath, overwrite = TRUE)
}

\arguments{
  \item{inpath}{ path with modis files }
  \item{outpath}{ output path }
  \item{overwrite}{ logical. If \code{TRUE}, files will be overwritten if they exist  }
}

\details{
  Clouds, snow and permanent water are removed to avoid confusion when classifying rice areas.
}

\value{
  nothing. only the side effect of new files on disk
  
}

\author{ Sonia Asilo }


\examples{
}

\keyword{ spatial }

