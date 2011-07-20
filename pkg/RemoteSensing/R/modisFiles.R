# Authors: Robert J. Hijmans, Sonia Asilo, Jorrel Khalil Aunario
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3

# TODO: fully integrate dir parameters, i.e. pattern 
modisFiles <- function(sep="\\.", modisinfo=c('product', 'acqdate', 'zone', 'version', 'proddate', 'band', 'format'), format="GTiff",...) {
    
    filename <- dir(..., pattern=formatExt(format))    
    
    info <- sub(".hdf","",basename(filename))
	
	x <- unlist(strsplit(info, sep))
	m <- as.data.frame(matrix(x, ncol=length(modisinfo), byrow=TRUE), stringsAsFactors=FALSE)
	if (ncol(m) != length(modisinfo)) { 
        stop('oops, non standard filenames found') 
    }
    colnames(m) <- modisinfo
    year <- as.numeric(substr(m$acqdate,2,5))
    doy <- as.numeric(substr(m$acqdate,6,8))
	m <- cbind(filename,year, doy, m, stringsAsFactors=FALSE)
	m$band <- as.character(sub("sur_refl_","",m$band))
	return(m)
}


