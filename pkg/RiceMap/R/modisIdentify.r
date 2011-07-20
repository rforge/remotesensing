# Author: Sonia Asilo, Jorrel Khalil S. Aunario
# IRRI
# License GPL3
# Version 2, March 2009

modis.identify <- function(modis, what=c("flooded1", "persistentwater", "drought", "forest", "shrub"), writeto=NA, verbose=TRUE){	
	# check if 64-bit 
	is64 <- version$arch=="x86_64"
	
	if (verbose) show.message(modis@acqdate, ": Delineating ", paste(what, collapse=", "), eol="\r")
	
	features <- modis.data(modis)
	if (is64) {
		features@imgvals <- modis.compute(modis@imgvals,funlist=what)
	} else {
		for (i in 1:length(what)){
			rbands <- getRequiredBands(what[i])
			input <- as.data.frame(modis@imgvals[,rbands])
			colnames(input) <- rbands
			if (i==1) {
				features@imgvals <- modis.compute(input,funlist=what[i], datatype="logical")
			} else {
				features@imgvals[,what[i]] <- modis.compute(input,funlist=what[i],datatype="logical")[,what[i]]
			}
			rm(input)
			gc(verbose=FALSE)
		}
	}
	
	if (is.character(writeto)){
		outdir <- normalizePath(writeto,mustWork=FALSE)
		force.directories(writeto, recursive=TRUE)
		
		if (verbose) show.message(modis@acqdate, ": Writing features to disk.", eol="\r")
		modis.brick(features, process="identify", intlayers=1:ncol(modis@imgvals), writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)		
	}
	if (verbose) show.message(modis@acqdate, ": ------------------ DONE IDENTIFYING -------------------", eol="\n")
	rm(modis)
	gc(verbose=FALSE)
	return(features)	
}

modis.composite <- function(composite, features=NULL){
	# check if 64-bit 
	is64 <- version$arch=="x86_64"
	
	if(is.null(features)){
		composite@acqdate <- paste(substr(composite@acqdate,1,5),"000",sep="")
		composite@proddate <- ""
		composite@imgvals$nimgs <- 1 
	} else {
		composite@imgvals <- composite@imgvals+cbind(features@imgvals,1)					
	}
	rm(features)
	gc(verbose=FALSE)
	return(composite)
}