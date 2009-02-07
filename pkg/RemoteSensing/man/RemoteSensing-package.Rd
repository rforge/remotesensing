\name{RemoteSensing-package}
\alias{RemoteSensing-package}
\alias{RemoteSensing}
\docType{package}
\title{
Remote sensing functions
}
\description{
Analysis of satellite images. 

}
\details{
\tabular{ll}{
Package: \tab RemoteSensing\cr
Type: \tab Package\cr
Version: \tab 0.1.1\cr
Date: \tab 2008-10-29\cr
License: \tab GPL v3\cr
LazyLoad: \tab yes\cr
}
Functions are:
- albedo.*: with * = modis, aster, landsat, avhrr
- vi.*: with * = dvi, evi, ndvi, gari, gemi, vari, wdvi... (14 of them)
- modis.qc*: with * = 250[a-f] and 500[a,c,d,e] is Quality Assessment flags
- modis.sqa*: with * = 500[a-k] is state Quality Assessment (clouds/fire/land&water...)
}
\author{
Yann Chemin

Maintainer: Who to complain to <yann.chemin@gmail.com>
}
\references{
~~ Literature or other references for background information ~~
}
\keyword{ package }
\keyword{ spatial }

\seealso{
\code{\link[<raster>:<raster>-package]{<raster>}}
}
\examples{
}
