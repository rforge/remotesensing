\name{albedo}

\alias{albedoAster}
\alias{albedoLandsat}
\alias{albedoModis}
\alias{albedoAvhrr}


\title{ Albedo }

\description{
Compute albedo from remote sensing data. 
}

\usage{
albedoAster(green, nir, swir2, swir3, swir5, swir6) 
albedoLandsat(blue, green, red, nir, chan5, chan7)
albedoModis(red, nir, blue, green, swir1, swir2, swir3) 
albedoAvhrr(red, nir)
}


\arguments{
  \item{red}{ red wavelength channel (band) }
  \item{nir}{ near infra-red wavelength channel (band) }
  \item{blue}{ blue wavelength channel (band) }
  \item{green}{ green wavelength channel (band)}
  \item{swir1}{ short wave infrared channel 1 }  
  \item{swir2}{ short wave infrared channel 2 }
  \item{swir3}{ short wave infrared channel 3 }
  \item{swir5}{ short wave infrared channel 5 }
  \item{swir6}{ short wave infrared channel 6 }  
  \item{chan5}{ channel 5 }
  \item{chan7}{ channel 7 }
}
  
\value{
 albedo
}


\author{ Yann Chemin }

\examples{
albedoAvhrr(200, 100)
}

\keyword{ spatial }

