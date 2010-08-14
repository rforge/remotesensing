\name{modisVegIndices}

\alias{modisVeg}

\title{ Calculate vegetation indices and land-water index }

\description{
Calculates vegetation indices: Enhanced Vegetation Index (EVI) and Normalized Difference Vegetation Index (NDVI) and
land-water index: Land Surface Water Index (LSWI). 
Identifies flooded pixels and permanent water using thresholds derived from vegetation and land-water indices.
}

\usage{
modisVeg(inpath, informat, outformat="raster", tiles="all")
}

\arguments{
  \item{inpath}{ path with cleaned modis files }
  \item{informat}{ File format of the input files can either be "raster" (.grd) or "GTiff" (.tif). }
  \item{outformat}{ File format desired by user can either be "raster" (.grd) or "GTiff" (.tif). }
  \item{tiles}{ vertical and horizontal tile number of MODIS, tiles should follow the format in the MODIS filename, e.g. "h29v07"  }
  }

\details{
  Uses NIR, RED, BLUE, SWIR bands that has been cleaned using modisClean as input in calculating the indices. 
  }

\seealso{ \code{\link{modisClean}} }

\references{Xiao, X., Boles, S., Frolking, S. et.al. 2006. Mapping paddy rice agriculture in Southeast Asia using multi-temporal MODIS images.  Remote Sensing of Environment 100, pages, 95-113. }

\author{ Sonia Asilo }


