# Authors: Sonia Asilo, Robert J. Hijmans, Jorrel Khalil S. Aunario
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


modisFiles <- function(path, pat) {
	f <- list.files(path=path, pattern=pat)
	# f <- list.files(path=path, pattern='.tif')
	x <- strsplit(f, '_')
	try(m <- cbind((matrix(unlist(x), ncol=length(x[[1]]), nrow=length(f), byrow=TRUE)),f))
	if (ncol(m) != 9) { 
        return(FALSE)
        stop('oops, non standard filenames found') 
    }
	m <- as.data.frame(m[,-c(4,6,8)], stringsAsFactors=FALSE)
	colnames(m) <- c('prod1', 'date', 'zone', 'prod2', 'band', 'filename')
	m$band <- substr(m$band, 10, 12)
	return(m)
}



modisFilesClean <- function(path,pat) {
	f <- list.files(path=path, pattern=pat)
	# f <- list.files(path=path, pattern='.grd')
	x <- strsplit(f, '_')
	#m <- matrix(, length(x), length(x[[1]]))
	#m <- matrix(unlist(x), ncol=length(x[[1]]), nrow=length(f))
    try(m <- (matrix(unlist(x), ncol=length(x[[1]]), nrow=length(f), byrow=TRUE)))
	m <- as.data.frame(m[,-4],stringsAsFactors=FALSE)
	m <- cbind(m,f)
	colnames(m) <- c('date', 'zone', 'band', 'filename')	
	#m$band <- strsplit(as.vector(m$band), '\\.grd' )
	
	m$year <- as.integer(substr(m$date, 2, 5))
	m$doy <- as.integer(substr(m$date, 6, 8))
	m$caldate <- as.Date(m$doy, origin=paste(m$year-1, "-12-31", sep=''))
	return(m)
}
