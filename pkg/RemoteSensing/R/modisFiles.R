# Authors: Robert J. Hijmans, Sonia Asilo, Jorrel Khalil Aunario
# Date :  Feb 2009
# Version 0.2
# Licence GPL v3

modisFiles <- function(sep="\\.", modisinfo=c('product', 'acqdate', 'zone', 'version', 'proddate', 'band'), format="GTiff",...) {
    
    filename <- dir(...)    
    filename <- filename[grep(formatExt(format),filename)]
	if (length(filename)<1) {
		message("No files found matching ", formatExt(format))
		return(vector())
	}
	
    #if (format=="GTiff") info <- sub(".hdf","",basename(filename))	
	info <- basename(filename)
	
	# Remove Extension
	info <- gsub(paste(".", formatExt(format),sep=""),"",info)
	
	x <- unlist(strsplit(info, sep))
	m <- as.data.frame(matrix(x, ncol=length(modisinfo), byrow=TRUE), stringsAsFactors=FALSE)
	if (ncol(m) != length(modisinfo)) { 
		message("Non-standard filenames found ")
        return(vector()) 
    }
    colnames(m) <- modisinfo
    year <- as.numeric(substr(m$acqdate,2,5))
    doy <- as.numeric(substr(m$acqdate,6,8))
	m <- cbind(filename,year, doy, m, stringsAsFactors=FALSE)
	if ("band" %in% modisinfo) m$band <- as.character(sub("sur_refl_","",m$band))
	return(m)
}


