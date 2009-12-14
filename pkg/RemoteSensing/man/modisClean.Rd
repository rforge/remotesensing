\name{modisClean}

\alias{modisClean}

\title{ Clean Modis files}

\description{
Remove cloud cover, snow, permanent water and missing values from raw Modis files using the MODIS quality control flags. 
Requires the bitops package
}

\usage{
modisClean(inpath, tileNumber)
}

\arguments{
  \item{inpath}{ path with modis files in Geotiff format }
  \item{tileNumber}{ vertical and horizontal tile number of MODIS, tileNumber should follow the format in the MODIS filename, e.g. "h29v07"  }
  }

\details{
  Clouds, snow and permanent water are removed to avoid confusion when classifying rice areas. 
  Modis state quality control flags are used to extract the pixels with clouds, snow and permanent water.
  Thresholds were used for identifying cloudy pixels that were not included in the cloud quality flag. Following Xiao's (2006) methodology, blue band reflectance >= 0.2 were masked as cloudy pixels.
  Missing values in bands 1-7 (i.e. -28762) were also removed. 	
 }

\value{
  nothing. only the side effect of new files on disk
  
}

\references{Xiao, X., Boles, S., Frolking, S. et.al. 2006. Mapping paddy rice agriculture in Southeast Asia using multi-temporal MODIS images.  Remote Sensing of Environment 100, pages, 95-113. }

\author{ Sonia Asilo }
    
\examples{ 
  require(bitops) 	
  setwd(dirname(system.file("external/.", package="RemoteSensing")))
  ## NIR band
  NIR <- raster("MOD09A1_b2.tif")
    
  ## Scaling
  NIRscaled <- NIR/10000
  
  ## quality control flags
  sqa <- raster("MOD09A1_sqa.tif")
      
 ## Extract cloudy and water pixels using the modis.sqa500f(internal cloud flag) 
 ## and modis.sqa500c(land/water flag)
 cloudmask <- calc(sqa, fun= modis.sqa500f) # where pixel values: 0 = no clouds, and 1 = with clouds
 cloudmask <- cloudmask==0      
 watermask <- calc(sqa, fun= modis.sqa500c) # where pixel values: 1 = land, and other values = water
 watermask <- watermask==1
 
 ## Water and clouds removed
 cleaned <- NIRscaled*cloudmask*watermask
 }

 

