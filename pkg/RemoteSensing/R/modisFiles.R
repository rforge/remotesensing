# Authors: Robert J. Hijmans, Sonia Asilo, Jorrel Khalil Aunario
# Date :  Feb 2009
# Version 0.2
# Licence GPL v3

modisFiles <- function(sep="\\.", modisinfo=c('product', 'acqdate', 'zone', 'version', 'proddate', 'band'), format="GTiff",...) {
    
    mfiles <- dir(...)    
    mfiles <- mfiles[grep(paste(formatExt(format),"$",sep=""),mfiles)]
	if (length(mfiles)<1) {
		message("No files found matching ", formatExt(format))
		return(vector())
	}
	
    #if (format=="GTiff") info <- sub(".hdf","",basename(mfiles))	
	info <- basename(mfiles)
	
	# Remove Extension
	info <- gsub(paste(".", formatExt(format),sep=""),"",info)
	
	x <- unlist(strsplit(info, sep))
	m <- data.frame(matrix(x, ncol=length(modisinfo), byrow=TRUE), stringsAsFactors=FALSE)
	if (ncol(m) != length(modisinfo)) { 
		message("Non-standard filenames found ")
        return(vector()) 
    }
    colnames(m) <- modisinfo
    m$year <- as.numeric(substr(m$acqdate,2,5))
    m$doy <- as.numeric(substr(m$acqdate,6,8))
	m$filename <- mfiles
	m <- m[,c("filename", "year", "doy", modisinfo)]
	if ("band" %in% modisinfo) m$band <- as.character(sub("sur_refl_","",m$band))
	return(m)
}


