# Author: Jorrel Khalil Aunario 
# International Rice Research Institute
# Date : 21 May 2010
# Version 0,1
# Licence GPL v3

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

withRetry <- function(expr, retries=50, pause=1){
	tries <- 0
	success <- FALSE
	while(success==FALSE & tries<retries){
		items <- try(expr,silent=TRUE)
		if (class(items)=="try-error"){
			tries <- tries+1
			message("Timeout? trying again in 10 secs...")
			Sys.sleep(pause)
		} else {
			success <- TRUE
		}
	}
	return(items)
}
