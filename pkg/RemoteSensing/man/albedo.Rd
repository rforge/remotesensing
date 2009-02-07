\name{albedo}

\alias{albedo.aster}
\alias{albedo.landsat}
\alias{albedo.modis}
\alias{albedo.noaa}


\title{ Albedo }
\description{
  Compute albedo from remote sensing data. 
}

\usage{
albedo.aster(greenchan, nirchan, swirchan2, swirchan3, swirchan5, swirchan6) 
albedo.landsat(bluechan, greenchan, redchan, nirchan, chan5, chan7)
albedo.modis(redchan, nirchan, chan3, chan4, chan5, chan6, chan7) 
albedo.noaa(redchan, nirchan)
}


\arguments{
  \item{redchan}{ ~~Describe \code{redchan} here~~ }
  \item{nirchan}{ ~~Describe \code{nirchan} here~~ }
  \item{bluechan}{ ~~Describe \code{bluechan} here~~ }
  \item{greenchan}{ ~~Describe \code{bluechan} here~~ }
  \item{swirchan2}{ ~~Describe \code{chan} here~~ }
  \item{swirchan3}{ ~~Describe \code{chan} here~~ }
  \item{swirchan5}{ ~~Describe \code{han} here~~ }
  \item{swirchan6}{ ~~Describe \code{chan} here~~ }  
  \item{chan3}{ ~~Describe \code{chan3} here~~ }
  \item{chan4}{ ~~Describe \code{chan4} here~~ }
  \item{chan5}{ ~~Describe \code{chan5} here~~ }
  \item{chan6}{ ~~Describe \code{chan6} here~~ }
  \item{chan7}{ ~~Describe \code{chan7} here~~ }

}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  albedo
}
\references{ ~put references to the literature/web site here ~ }
\author{ Yann Chemin }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{

}
\keyword{ spatial }

