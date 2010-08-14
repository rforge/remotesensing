# Author: Sonia Asilo, Robert Hijmans, Jorrel Khalil S. Aunario
# IRRI
# License GPL3
# Version 2, March 2009


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

