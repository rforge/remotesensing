# Author: Jorrel Khalil Aunario 
# International Rice Research Institute
# Date : 21 May 2010
# Version 0,1
# Licence GPL v3
# DEPRECATE

raster2SGDF <- function(baseraster, vals=NULL){
	if (!is.null(vals)) {
		baseraster <- setValues(baseraster, vals)
	}
	baseraster <- as(baseraster, 'SpatialGridDataFrame')
	return(baseraster)
}

formatExt <- function(myformat){
    ext <- rep(NA, length(myformat))
    ext[tolower(myformat)=="raster"] <- "grd"
    ext[tolower(myformat)=="gtiff"] <- "tif" 
    ext[tolower(myformat)=="hdf"] <- "hdf"
    return(ext)    
}

extFormat <- function(filext){
	formt <- rep(NA,length(filext))
	formt[tolower(filext)=="tif"] <- "GTiff"
	formt[tolower(filext)=="grd"] <- "raster"
	return(formt)
}

bandnames <- function(bandnum, ref="ricemap"){
	bands <- as.data.frame(cbind(c("red", "nir1", "blue", "green", "nir2", "swir1", "swir2"), c("red", "nir", "blue", "green", NA, "swir1", "swir2")),stringsAsFactors=FALSE)
	colnames(bands) <- c("default","ricemap")
	return(bands[bandnum,ref])
} 

bandnumber <- function(bandname, ref="ricemap", asString=TRUE){
	bands <- cbind(c("red", "nir1", "blue", "green", "nir2", "swir1", "swir2"), c("red", "nir", "blue", "green", NA, "swir1", "swir2"))
	colnames(bands) <- c("default","ricemap")
	if (asString) result <- paste("b",gsub(" ",0,format(which(bands[,ref] %in% bandname),width=2)),sep="") else result <- which(bands[,ref] %in% bandname)
	return(result)
} 

validFolders <- function(styear=2000, enyear=as.numeric(format(Sys.Date(),"%Y"))){
	valid <- vector()
	for (y in styear:enyear){
		st <- ifelse(y==2000,paste(y,"2","18",sep="-"),paste(y,"1","1",sep="-"))
		valid <- c(valid,format(seq(from=as.Date(st), to=as.Date(paste(y,"12","31",sep="-")), by=8),"%Y.%m.%d"))
	}
	return(valid)
}

subFolderFromDoy <- function(doy,year){
	valid <- validFolders()
	dirdate <- format(as.Date(paste(doy, year),"%j %Y"),"%Y.%m.%d")
	dirdate[dirdate %in% valid] 
	return(dirdate[dirdate %in% valid])
}