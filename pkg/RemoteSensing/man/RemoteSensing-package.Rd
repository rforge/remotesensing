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
Date: \tab 2009-12-29\cr
License: \tab GPL v3\cr
LazyLoad: \tab yes\cr
}
Functions include:

- albedo.*: with * = modis, aster, landsat, avhrr

- vi.*: with * = dvi, evi, ndvi, gari, gemi, vari, wdvi... (14 of them)

- modis.qc*: with * = 250[a-f] and 500[a,c,d,e] is Quality Assessment flags

- modis.sqa*: with * = 500[a-k] is state Quality Assessment (clouds/fire/land&water...)

- Landsat data preparation

- Modis rice mapping
}

\author{
Alice Laborte, Yann Chemin, Robert Hijmans

Maintainer: r.hijmans@gmail.com
}

\keyword{ package }
\keyword{ spatial }

