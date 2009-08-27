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
  \item{swir1}{ ~~Describe \code{chan} here~~ }  
  \item{swir2}{ ~~Describe \code{chan} here~~ }
  \item{swir3}{ ~~Describe \code{chan} here~~ }
  \item{swir5}{ ~~Describe \code{han} here~~ }
  \item{swir6}{ ~~Describe \code{chan} here~~ }  
  \item{chan5}{ ~~Describe \code{chan5} here~~ }
  \item{chan7}{ ~~Describe \code{chan7} here~~ }
}

\details{
  }
  
\value{
  albedo
}

\references{  }

\author{ Yann Chemin }

\note{ 
 
}
\seealso{ }
\examples{

}
\keyword{ spatial }

