# Author: Yann Chemin & Aileen Maunahan, Sonia Asilo, Jorrel Khalil S. Aunario
# IRRI
# License GPL3
# Version 2, March 2009


nddi <- function(ndvi, ndwi) {
# NDDI: Normalized Difference Drought Index {
	result<- (ndvi - ndwi) / (ndvi + ndwi)
	result[is.infinite(result)] <- NA
	result[result < 0] <- 0
	result[result > 2] <- 2
	return(result)
}


drought <- function(ndvi, ndwi) {
# DROUGHT where drought = 1, no drought=0
	res <- ((ndvi < 0.5 & ndwi < 0.3)*2) + ((ndvi > 0.6 & ndwi > 0.4)*1) - 1
	return(! res)
}



ndwi <- function(nir, swir2) {
# NDWI: Normalized Difference Water Index
	result<- (nir - swir2) / (nir + swir2)
	result[is.infinite(result)] <- NA
	result[result < -1] <- -1
	result[result > 1] <- 1
	return(result)
}


lswi<-function(nir, swir) {
 #LSWI: Land Surface Water Index
	result <- (nir - swir) / (nir + swir)
	result[is.infinite(result)] <- NA
	result[result < -1] <- -1
	result[result > 1] <- 1
	return(result)
}


water<-function(ndvi, albedo) {
 #water: generic water mapping tool
	return( (ndvi < 0.1) & (albedo < 0.1) )
}


waterModis<-function(ndvi, band7) {
 #water.modis: Terra-MODIS water mapping tool
 #Xiao X., Boles S., Liu J., Zhuang D., Frokling S., Li C., Salas W., Moore III B. (2005). 
 #Mapping paddy rice agriculture in southern China using multi-temporal MODIS images. 
 #Remote Sensing of Environment 95:480-492.
 #
 #Roy D.P., Jin Y., Lewis P.E., Justice C.O. (2005). 
 #Prototyping a global algorithm for systematic fire-affected
 #area mapping using MODIS time series data. 
 #Remote Sensing of Environment 97:137-162.
 
	result<- ndvi * band7
	return( (ndvi < 0.1) & (band7 < 0.04) )	
	
}

