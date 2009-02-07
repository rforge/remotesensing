# Author: Yann Chemin
# IRRI
# License GPL3
# Version 1, October 2008

lswi<-function(nir, swir)
 #LSWI: Land Surface Water Index
{
	result <- (nir - swir) / (nir + swir)
	result[is.infinite(result)] <- NA
	result[result < -1] <- -1
	result[result >1] <- 1
	return(result)
}

water<-function(ndvi, albedo)
 #water: generic water mapping tool
{
	ndvi[ndvi<0.1]<-1
	ndvi[ndvi>0.1]<-0
	albedo[albedo<0.1]<-1
	albedo[albedo>0.1]<-0
	result<- ndvi * albedo
#	result[is.nan(result)] <- NA
	return(result)
}

waterModis<-function(ndvi, band7)
 #water.modis: Terra-MODIS water mapping tool
 #Xiao X., Boles S., Liu J., Zhuang D., Frokling S., Li C., 
 #Salas W., Moore III B. (2005). 
 #Mapping paddy rice agriculture in southern China using 
 #multi-temporal MODIS images. 
 #Remote Sensing of Environment 95:480-492.
 #
 #Roy D.P., Jin Y., Lewis P.E., Justice C.O. (2005). 
 #Prototyping a global algorithm for systematic fire-affected
 #area mapping using MODIS time series data. 
 #Remote Sensing of Environment 97:137-162.
{
	ndvi[ndvi<0.1]<-1
	ndvi[ndvi>0.1]<-0
	band7[band7<0.04]<-1
	band7[band7>0.04]<-0
	result<- ndvi * band7
#	result[is.nan(result)] <- NA
	return(result)
}


