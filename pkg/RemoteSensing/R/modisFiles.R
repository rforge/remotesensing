# Authors: Robert J. Hijmans, Sonia Asilo
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


modisFiles <- function(path) {
	f <- list.files(path=path, pattern='.tif')
	x <- strsplit(f, "\\.")
	try(m <- cbind((matrix(unlist(x), ncol=length(x[[1]]), byrow=TRUE)),f))
	if (ncol(m) != 9) { 
        stop('oops, non standard filenames found') 
    }
	m <- as.data.frame(m[,-c(4,6,8)], stringsAsFactors=FALSE)
	colnames(m) <- c('prod1', 'date', 'zone', 'prod2', 'band', 'filename')
	m$band <- substr(m$band, 10, 12)
	return(m)
}


