# Author: Yann Chemin
# License GPL3
# Version 1, October 2008


arvi <- function(red,nir,blue) {
#ARVI: Atmospheric Resistant Vegetation Index
# ARVI is resistant to atmospheric effects (in comparison to the NDVI) and is accomplished by a self correcting process for the atmospheric effect in the red nel, using the difference in the radiance between the blue and the red nels. (Kaufman and Tanre 1996) 
	result <- (nir - (2 * red - blue)) / (nir + (2 * red - blue))
	return(result)
}

dvi<-function(red, nir) {
#DVI: Difference Vegetation Index
	result <- (nir - red)
	return(result)
}


evi<-function(blue, red, nir) {
#EVI: Enhanced Vegetation Index 
# Huete A.R., Liu H.Q., Batchily K., vanLeeuwen W. (1997)
# A comparison of vegetation indices global set of TM images for EOS-MODIS
# Remote Sensing of Environment, 59:440-451.
	result <- 2.5 * ( ( nir - red ) / ( nir + ( 6 * red ) - ( 7.5 * blue )  + 1 ) )
	result[is.infinite(result)] <- NA
	result[result < -1] <- -1
	result[result > 1] <- 1
	return(result)
} 


gari<-function(red, nir, blue, green) {
#GARI: green atmospherically resistant vegetation index 
	result <- (nir - (green - (blue - red))) / (nir - (green - (blue - red)))
	return(result)
}


gemi<-function(red,nir) {
#GEMI: Global Environmental Monitoring Index
	result <- (((2 * ((nir * nir) - (red * red)) + 1.5 * nir + 0.5 * red) / (nir + red + 0.5)) * (1 - 0.25 * (2 * ((nir * nir) - (red * red)) + 1.5 * nir + 0.5 *  red) /  (nir + red + 0.5))) - ((red - 0.125) / (1 - red))
	return(result)
}


gvi<-function(blue, green, red, nir, ch5, ch7) {
#Green Vegetation Index
	result <- (-0.2848 * blue - 0.2435 * green - 0.5436 * red + 0.7243 * nir + 0.0840 * ch5 - 0.1800 * ch7)
	return(result)
}


ipvi<-function(red, nir) {
#IPVI: Infrared Percentage Vegetation Index
	result <- (nir) / (nir + red)
	return(result)
}


msavi2<-function(red, nir) {
#MSAVI2: second Modified Soil Adjusted Vegetation Index
	result <- (1 / 2) * (2 * (nir + 1) - sqrt((2 * nir + 1) * (2 * nir + 1)) - (8 * (nir - red)))
	return(result)
}


msavi<-function(red, nir) {
#MSAVI: Modified Soil Adjusted Vegetation Index
# MSAVI = s(NIR-s*red-a) / (a*NIR+red-a*s+X*(1+s*s))     
# where a is the soil line intercept, s is the soil line slope, and X  is an adjustment factor which is set to minimize soil noise (0.08 in original papers).
	result <- (1 / 2) * (2 * (nir + 1) - sqrt((2 * nir + 1) * (2 * nir + 1)) - (8 * (nir - red)))
	return(result)
}

ndvi<-function(red, nir) {
#NDVI: Normalized Difference Vegetation Index
	result<- (nir - red) / (nir + red)
	result[is.infinite(result)] <- NA
	result[result < -1] <- -1
	result[result > 1] <- 1
	return(result)
}


pvi <- function(red, nir) {
#PVI: Perpendicular Vegetation Index 
# PVI = sin(a)NIR-cos(a)red
# for a  isovegetation lines (lines of equal vegetation) would all be parallel to the soil line therefore a=1
	result <- (sin(1) * nir) / (cos(1) * red)
	return(result)
}

 
savi <- function(red, nir) {
#SAVI: Soil Adjusted Vegetation Index
	result <- ((1 + 0.5) * (nir - red)) / (nir + red + 0.5)
	return(result)
}


sr <- function(red, nir) {
#SR: Simple Vegetation ratio
	result <- (nir / red)
	return(result)
}


vari <- function(blue, green, red) {
#VARI: Visible Atmospherically Resistant Index
# VARI is the Visible Atmospherically Resistant Index, it was 
# designed to introduce an atmospheric self-correction 
# Gitelson A.A., Kaufman Y.J., Stark R., Rundquist D., 2002.
# Novel algorithms for estimation of vegetation fraction
# Remote Sensing of Environment (80), pp76-87.  
	result <- (green - red ) / (green + red - blue)
	return(result)
}

wdvi<-function(red, nir, slslope = 1) {
#WDVI: Weighted Difference Vegetation Index
	slslope #slope of soil line
	result <- (nir - slslope * red)
	return(result)
}

