# Authors: Robert J. Hijmans, Sonia Asilo, Jorrel Khalil S. Aunario
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


modisFilesClean <- function(path, pat) {
	f <- list.files(path=path, pattern=pat)
	x <- strsplit(f, '_')
	#m <- matrix(, length(x), length(x[[1]]))
	#m <- matrix(unlist(x), ncol=length(x[[1]]), nrow=length(f))
    m <- try((matrix(unlist(x), ncol=length(x[[1]]), nrow=length(f), byrow=TRUE)))
	m <- as.data.frame(m[,-4],stringsAsFactors=FALSE)
	m <- cbind(m,f)
	colnames(m) <- c('date', 'zone', 'band', 'filename')	
	#m$band <- strsplit(as.vector(m$band), '\\.grd' )
	m$year <- as.integer(substr(m$date, 2, 5))
	m$doy <- as.integer(substr(m$date, 6, 8))
	m$caldate <- as.Date(m$doy, origin=paste(m$year-1, "-12-31", sep=''))
	return(m)
}
