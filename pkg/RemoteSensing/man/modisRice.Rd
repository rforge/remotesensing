\name{modisRice}

\alias{modisRice}

\title{ Classification based on NDVI, EVI and LSWI thresholds }

\description{
Third function based on a series of four functions developed for mapping rice areas using MODIS data. 
Generates six classes based on NDVI, EVI, LSWI thresholds. 
}

\usage{
modisRice(inpath, tileNumber)
}

\arguments{
  \item{inpath}{ path with vegetation indices files }
  \item{tileNumber}{ vertical and horizontal tile number of MODIS, tileNumber should follow the format in the MODIS filename, e.g. "h29v07"  }
}

\details{
The algorithm uses thresholds based on NDVI, EVI, and LSWI to classify rice and non rice pixels. It generates the following classes: 

\tabular{rll}{
\tab Class \tab detection method \cr 
\tab Flooding/transplanting \tab (lswi + 0.05 >= evi) | (lswi + 0.05 >= ndvi); LSWI either approaches or is higher than NDVI or EVI may signal flooding in paddy rie fields \cr 
\tab Permanent water \tab (ndvi < 0.1) & (ndvi < lswi); thse are rivers, lakes, ocean \cr
\tab Forest \tab NDVI values >= 0.7 over atleast 20 8-day composites during the year. \cr
\tab Shrub \tab LSWI values >= 0.10; these are evergreen shrublands and woodlands \cr
\tab notrice \tab combined all classes that were not classified as rice; these are permanent water, forest and shrub \cr
\tab Perhapsrice \tab flooding/transplanting pixels and those that were not classified as notrice \cr
}
}

\references{Xiao, X., Boles, S., Frolking, S. et.al. 2006. Mapping paddy rice agriculture in Southeast Asia using multi-temporal MODIS images.  Remote Sensing of Environment 100, pages, 95-113. }

\seealso{\code{\link{modisClean}}, \link{modisVeg}, \link{vi} }

\author{Sonia Asilo }



