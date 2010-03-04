# Author: Yann Chemin & Aileen Maunahan, Sonia Asilo, Jorrel Khalil S. Aunario
# IRRI
# License GPL3
# Version 2, March 2009


lswi<-function(nir, swir)
 #LSWI: Land Surface Water Index
{
	result <- (nir - swir) / (nir + swir)
	result[is.infinite(result)] <- -9999
	result[result < -1] <- -1
	result[result > 1] <- 1
	return(result)
}


water<-function(ndvi, albedo) 
 #water: generic water mapping tool
{
	return( (ndvi < 0.1) & (albedo < 0.1) )
}


waterModis<-function(ndvi, band7)
 #water.modis: Terra-MODIS water mapping tool
 #Xiao X., Boles S., Liu J., Zhuang D., Frokling S., Li C., Salas W., Moore III B. (2005). 
 #Mapping paddy rice agriculture in southern China using multi-temporal MODIS images. 
 #Remote Sensing of Environment 95:480-492.
 #
 #Roy D.P., Jin Y., Lewis P.E., Justice C.O. (2005). 
 #Prototyping a global algorithm for systematic fire-affected
 #area mapping using MODIS time series data. 
 #Remote Sensing of Environment 97:137-162.
{
	result<- ndvi * band7
	return( (ndvi < 0.1) & (band7 < 0.04) )	
	
}


flooded <- function(lswi, ndvi, evi) { 
#Xiao X., Boles S., Liu J., Zhuang D., Frokling S., Li C., Salas W., Moore III B. (2005). 
 #Mapping paddy rice agriculture in southern China using multi-temporal MODIS images. 
 #Remote Sensing of Environment 95:480-492.
    res <- rep(0, length(lswi))
    res[(lswi+0.05 >= evi) | (lswi+0.05 >= ndvi)] <- 1
    return(res)
}


persistentwater <- function(ndvi,lswi){ 
#Xiao X., Boles S., Liu J., Zhuang D., Frokling S., Li C., Salas W., Moore III B. (2005). 
#Mapping paddy rice agriculture in southern China using multi-temporal MODIS images. 
#Remote Sensing of Environment 95:480-492.
    res <- rep(0, length(ndvi))
    res[(ndvi < 0.10) & (ndvi < lswi)] <- 1	
	return(res)
}

