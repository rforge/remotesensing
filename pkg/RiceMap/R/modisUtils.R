# Author: Jorrel Khalil Aunario 
# International Rice Research Institute
# Date : 21 May 2010
# Version 0,1
# Licence GPL v3

properPath <- function(path, changeBS=TRUE){
    if (changeBS){
        path <- gsub("\\\\", "/", path)
    }
    lastchar <- substr(path,nchar(path),nchar(path))
    if (lastchar=="/"){
        path <- substr(path,1,nchar(path)-1)
    }
    return(path)
}

raster2SGDF <- function(baseraster, vals=NULL){
	if (!is.null(vals)) {
		baseraster <- setValues(baseraster, vals)
	}
	baseraster <- as(baseraster, 'SpatialGridDataFrame')
	return(baseraster)
}


rescale <- function(x, oldmin, oldmax, newmin, newmax){
	y <- newmin + (newmax * ((x-oldmin)/(oldmax-oldmin)))
	return(y)
}

formatExt <- function(myformat){
    ext <- rep(NA, length(myformat))
    ext[tolower(myformat)=="raster"] <- ".grd"
    ext[tolower(myformat)=="gtiff"] <- ".tif"
    return(ext)    
}

.rsMessage <- function(msg, newln=FALSE){
    if (newln){
        cat(msg, "\n")
        flush.console()    
    } else {
        cat(rep(" ", getOption("width")),"\r", sep="")        
        cat(msg, "\r")
        flush.console()
    }    
}
