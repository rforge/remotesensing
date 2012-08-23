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


rescale <- function(x, oldmin, oldmax, newmin, newmax){
	y <- newmin + (newmax * ((x-oldmin)/(oldmax-oldmin)))
	return(y)
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
force.directories <- function(path,...){
    
    if(!file.exists(path)){
        success <- dir.create(path,...)  
    } else success <- TRUE
    return(success)
}

show.message <- function(..., eol=""){
	if (eol=="\r") cat(rep(" ", options("width")),eol,sep="")
	cat(...,eol,sep="")
	flush.console()
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

withRetry <- function(expr, retries=10, initpause=30, failtime=10,verbose=FALSE){
	tries <- 0
	success <- FALSE
	while(success==FALSE & (tries<retries | failtime>(initpause*tries))){
		items <- try(expr,silent= !verbose) 
		if (class(items)=="try-error"){
			tries <- tries+1
			show.message("Timeout? trying again in ", (initpause*tries) ," secs...", eol="\n")
			Sys.sleep(initpause*tries)
		} else {
			success <- TRUE
		}		
	}
	if (!success) items <-c() # return an empty vector if the expr fails to return anything or times out to the limit 
	return(items)
}
