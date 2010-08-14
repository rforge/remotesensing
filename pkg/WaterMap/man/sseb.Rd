\name{Simple Surface Energy Balance}
\alias{sseb_eta}

\title{Simple Surface Energy Balance}
\description{
  Computes Energy Balance according to Senay et al., 2007 (see below). 
  Input required are daily Penman-Monteith ETo (FAO56), LST, LST_hot and LST_cold. Computes Daily ETa.
}

\usage{
sseb_eta(lst_h, lst_c, lst, et0PM)
}


\arguments{
  \item{lst_h}{hot pixel value}
  \item{lst_c}{cold pixel value}
  \item{lst}{Land Surface Temperature (band) data}
  \item{et0PM}{ETo PM FAO56 (band) data}
}

\value{
  Simple Surface Energy Balance
}

\details{
Senay et al. found a correlation coefficient of 0.95 with METRIC and correlation coefficients between 0.55-0.79 with SEBAL.
}

\references{
Senay, G.B, M. Budde, J.P. Verdin, A.M. Melesse (2007). A Coupled Remote Sensing and Simplified Energy Balance Approach to Estimate Actual Evapotranspiration from Irrigated Fields. Sensors 7: 979-1000.
}

\author{Yann Chemin, Umair Rabbani}


\keyword{spatial}

