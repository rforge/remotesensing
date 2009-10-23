\name{modisVegIndices}

\alias{modisVeg}

\title{ Calculate vegetation indices and land-water index }

\description{
Calculates vegetation indices: Enhanced Vegetation Index (EVI) and Normalized Difference Vegetation Index (NDVI) and
land-water index: Land Surface Water Index (LSWI). 
Identifies flooded pixels and permanent water using thresholds derived from vegetation and land-water indices.
}

\usage{
modisVeg(inpath, tileNumber)
}

\arguments{
  \item{inpath}{ path with cleaned modis files }
  \item{tileNumber}{ vertical and horizontal tile number of MODIS, tileNumber should follow the format in the MODIS filename, e.g. "h29v07"  }
  }

\details{
  Uses NIR, RED, BLUE, SWIR bands that has been cleaned using modisClean as input in calculating the indices. 
  }

\author{ Sonia Asilo }

\seealso{ \code{\link{modisClean}}, \link{vi}, \link{water} }

\examples{ 
  setwd(dirname(system.file("external/.", package="RemoteSensing")))
  ## RED band
  b1 <- "MOD09A1_b1.tif"
  red <- raster(b1) 
  
  ## NIR band
  b2 <- "MOD09A1_b2.tif"
  nir <- raster(b2)

  ## BLUE band
  b3 <- "MOD09A1_b3.tif" 	
  blue <- raster(b3)
  
  ## SWIR band
  b6 <- "MOD09A1_b6.tif" 	
  swir <- raster(b6)
  
  ## Calculate NDVI, EVI, LSWI, flooded and persistent water
  NDVI <- overlay(red, nir, fun= ndvi)
  EVI <- overlay(blue, red, nir, fun= evi)
  LSWI <- overlay(nir, swir, fun = lswi)
  floodedpix <- overlay(LSWI, NDVI, EVI, fun= flooded)
  pwater <- overlay(NDVI, LSWI, fun= persistentwater)
 }


