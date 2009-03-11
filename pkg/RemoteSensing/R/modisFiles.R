# Authors: Sonia Asilo, Robert J. Hijmans
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


modisFiles <- function(path) {
	f <- list.files(path=path, pattern='.tif')
	x <- strsplit(f, '\\.')
	m <- matrix(, length(x), length(x[[1]]))
	for(i in 1:length(x)) { m[i,] <- x[[i]] }
	m <- cbind(m, f)
	if (dim(m)[2] != 9) { stop('oops, non standard filenames found') }
	m <- m[,-8]
	m <- m[,-6]
	m <- m[,-4]
	colnames(m) <- c('prod1', 'date', 'zone', 'prod2', 'band', 'filename')
	m <- as.data.frame(m)
	m$band <- substr(m$band, 10, 12)
	return(m)
}


modisFilesClean <- function(path) {
	f <- list.files(path=path, pattern='.grd')
	x <- strsplit(f, '_')
	m <- matrix(, length(x), length(x[[1]]))
	for(i in 1:length(x)) { m[i,] <- x[[i]] }
	m <- m[,-4] 
	m <- cbind(m, f)
	colnames(m) <- c('date', 'zone', 'band', 'filename')
	m <- as.data.frame(m)
	m$band <- strsplit(as.vector(m$band), '\\.grd' )
	
	m$year <- as.integer(substr(as.vector(m$date), 2, 5))
	m$doy <- as.integer(substr(as.vector(m$date), 6, 8))
	m$caldate <- as.Date(m$doy, origin=paste(m$year-1, "-12-31", sep=''))
	return(m)
}
