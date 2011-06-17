# Author: Sonia Asilo, Robert Hijmans, Jorrel Khalil S. Aunario, Andy Nelson
# IRRI
# License GPL3
# Version 2, March 2009

dvel <- function(evi, lswi){
# DVEL: Difference in the Values of EVI and LSWI
    return(evi-lswi)
}

flooded1 <- function(lswi, ndvi, evi) { 
#Xiao X., Boles S., Liu J., Zhuang D., Frokling S., Li C., Salas W., Moore III B. (2005). 
#Mapping paddy rice agriculture in southern China using multi-temporal MODIS images. 
#Remote Sensing of Environment 95:480-492.
    res <- rep(0, length(lswi))
    res[(lswi+0.05 >= evi) | (lswi+0.05 >= ndvi)] <- 1
    return(res)
}

flooded2 <- function(evi, lswi){
#Yan-Er Yan, Zu-Tao Ouyangm Hai-Qiang Guo, Shu-Song Jin, Bin Zhao (2010)
#Detecting the spatiotemporal changes of tidal flood in the estuarine wetland by using MODIS time series data
#Journal of Hydrology 384:156-163
    flood <- rep(NA,length(evi))
    dvl <- dvel(evi,lswi)
    flood[evi>0.2] <- 0
    flood[!(evi<=0.2 & dvl<=0.05)] <- 0
    wrp <- which(evi<=0.2 & dvl<=0.05) #water-related points
    flood[wrp[evi[wrp]<=0.1]] <- 1
    flood[wrp[lswi[wrp]>0 & lswi[wrp]<0.2]] <- 0
    
    return(flood)
}

flooded3 <- function(evi,lswi){
    flood <- rep(NA, length(evi))
    dvl <- dvel(evi,lswi)
    flood[evi>0.3] <- 0
    flood[!(evi<=0.3 & dvl<=0.05)] <- 0
    wrp <- which((evi<=0.3 & dvl<=0.05)|(evi<=0.05 & lswi<=0)) #water-related points
    flood[wrp[evi[wrp]<=0.1]] <- 1
    return(flood)     
}

persistentwater <- function(ndvi,lswi){ 
#Xiao X., Boles S., Liu J., Zhuang D., Frokling S., Li C., Salas W., Moore III B. (2005). 
#Mapping paddy rice agriculture in southern China using multi-temporal MODIS images. 
#Remote Sensing of Environment 95:480-492.
    res <- rep(0, length(ndvi))
    res[(ndvi < 0.10) & (ndvi < lswi)] <- 1	
	return(res)
}

drought <- function(NDVI, NDWI) {
	res <- ((NDVI < 0.5 & NDWI < 0.3)*2) + ((NDVI > 0.6 & NDWI > 0.4)*1)
	return(res)
}
