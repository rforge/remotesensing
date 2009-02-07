\name{eb}

\alias{deltat_wim}
\alias{solar_day}
\alias{rnet_day}
\alias{etpot_day}
\alias{rnet}
\alias{g0}
\alias{dtair0}
\alias{dtair}
\alias{dtair_desert}
\alias{h0}
\alias{psi_h}
\alias{rah0}
\alias{rah1}
\alias{rohair0}
\alias{rohair}
\alias{U0}
\alias{ustar}
\alias{zom0}
\alias{sensih}
\alias{evapfr}
\alias{eta}
\alias{soilmoisture}


\title{ Energy-Balance functions }
\description{
  Compute the Energy Balance elements. 
}

\usage{
deltat_wim(surface_temperature)
solar_day(latitude, doy, tsw )
rnet_day( bbalb,  solar,  tsw )
etpot_day( rnetd,  tempk,  roh_w )
rnet( bbalb,  ndvi,  tempk,  dtair,   e0,  tsw,  doy,  utc,  sunzangle )
g0(  bbalb,  ndvi,  tempk,  rnet,  time=11.0, roerink=FALSE)
dtair0( t0_dem,  tempk_water=298.0,  tempk_desert=333.0)
dtair( t0_dem,  tempk_water=298.0,  tempk_desert=333.0,  dtair_desert=13.0)
dtair_desert( h_desert, roh_air_desert=1.12, rah_desert=10.0)
h0( roh_air, rah, dtair)
psi_h( t0_dem, h, U_0, roh_air)
rah0( zom_0, u_0)
rah1( psih, ustar)
rohair0(tempk)
rohair( dem, tempk, dtair)
U0( zom0, u2m)
ustar( t0_dem, h, ustar, roh_air, zom, u2m)
zom0( ndvi, ndvi_max)
sensih( iteration, tempk_water, tempk_desert, t0_dem, tempk, ndvi, ndvi_max, dem, rnet_desert, g0_desert, t0_dem_desert, u2m, dem_desert)
evapfr(rnet,g0,h0)
soilmoisture(evapfr) 
eta(rnet_day,evapfr,surface_temperature)
}

\arguments{
  \item{ndvi}{ NDVI layer from \code{vi.ndvi} }
  \item{bbalb}{ albedo layer from \code{albedo.*}  }
  \item{surface_temperature}{}
  \item{latitude}{} 
  \item{doy}{} 
  \item{tsw}{} 
  \item{solar}{} 
  \item{rnetd}{} 
  \item{tempk}{} 
  \item{roh_w}{} 
  \item{dtair}{} 
  \item{e0}{}
  \item{utc}{} 
  \item{sunzangle}{} 
  \item{rnet}{} 
  \item{time}{} 
  \item{roerink}{} 
  \item{t0_dem}{} 
  \item{tempk_water}{} 
  \item{tempk_desert}{}
  \item{dtair_desert}{} 
  \item{h_desert}{} 
  \item{roh_air_desert}{} 
  \item{rah_desert}{} 
  \item{roh_air}{} 
  \item{rah}{} 
  \item{h}{}  
  \item{U_0}{}
  \item{zom_0}{} 
  \item{u_0}{}  
  \item{psih}{} 
  \item{ustar}{} 
  \item{dem}{} 
  \item{zom0}{} 
  \item{u2m}{} 
  \item{zom}{} 
  \item{ndvi_max}{} 
  \item{iteration}{} 
  \item{rnet_desert}{}
  \item{g0_desert}{} 
  \item{t0_dem_desert}{} 
  \item{dem_desert}{} 
  \item{g0}{} 
  \item{h0}{} 
  \item{evapfr}{} 
  \item{rnet_day}{}
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  Water Mapping
}
\references{ 
Bastiaanssen, W.G.M. 1995. SEBAL.
}
\author{ Yann Chemin }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

}
\keyword{ spatial }

