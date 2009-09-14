\name{Simple Surface Energy Balance}
\alias{sseb_eta}

\title{Senay et al., 2007 Simple Surface Energy Balance}
\description{
  Computes Energy Balance according to Senay et al., 2007.
  Input required are daily Penman-Monteith ETo (FAO56), LST, LST_hot and LST_cold.
  Computes Daily ETa.
  The paper mentions 0.95+ correlation with METRIC and 0.55-0.79 correlation with SEBAL.
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

\author{Yann Chemin, Umair Rabbani}

\examples{
}

\keyword{spatial}

