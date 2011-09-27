# Author: Donna Aguirre, Francis Dimaano, Teejay Menciano, Jorrel Khalil S. Aunario, Kenneth Bruskiewicz, Richard Bruskiewich 
# IRRI
# License GPL3
# Version 1, August 2011

modis.download <- function(tile, ftp_dir, savedir, redownload=FALSE, verbose=TRUE){
	result <- NULL # NULL data is the default result of failure
	if (verbose)  {
		show.message(paste("Acquiring file list in", ftp_dir), eol="\n")
		show.message(paste("Saving files to ", savedir), eol="\n")
	}
	force.directories(savedir,recursive=TRUE)
	files <- withRetry(unlist(strsplit(getURL(ftp_dir, .opts=curlOptions(ftplistonly=TRUE)),"\n")))
	if (length(files)>0){
		BLOCK1 <- gsub("\r", "", files[grep(paste(tile,"hdf",sep=".*."),files)])
		BLOCK1 <-  BLOCK1[-grep("xml",BLOCK1)]
		
		if (length(BLOCK1)!=0){ 
			hdffile <- paste(savedir, BLOCK1, sep="/")
			if (file.exists(hdffile) & !redownload) {
				# File already present in local savedir
				if (verbose) show.message(paste(BLOCK1, " Exists. ", sep=""), eol="\n")
				result <- c(hdffile)	
			} else {
				# File not yet downloaded - attempt to get it!
				if (verbose) show.message(paste("Downloading ", ftp_dir, BLOCK1,sep=""), eol="\n")		
				items <- withRetry(download.file(paste(ftp_dir, BLOCK1,sep=""), destfile=hdffile, method='internal', mode='wb',quiet=TRUE),retries=49,verbose=verbose)
				
				if(length(items)>0) {
					show.message(paste(hdffile," successfully downloaded!",sep=""), eol="\n")
					result <- c(hdffile)
				} else {
					show.message(paste(ftp_dir, BLOCK1," seen but could not be downloaded?",sep=""), eol="\n")
					unlink(hdffile) 
				}
			}
			
		} else {
			show.message(paste(tile, "not found in", ftp_dir), eol="\n") 
			
		}
	}	
	return(result)
}

modis.hdf2tif <- function(hdffile, outdir, MRT_HOME=normalizePath(Sys.getenv("MRT_HOME"),winslash="/"), rm.hdf=FALSE){
	
	if(!is.character(hdffile)) {
		show.message(paste(hdffile," is not a valid HDF file name character string?",sep=""), eol="\n")
		return(FALSE)
	}	
	
	force.directories(outdir)
	success <- FALSE
	if (MRT_HOME=="") {
		show.message("MRT not installed. Download it here (https://lpdaac.usgs.gov/lpdaac/tools/modis_reprojection_tool)", eol="\n")
	} else {
		MRT <- paste(MRT_HOME,"bin", sep="/")
		
		filename <- paste(MRT, "/modisconfig.prm", sep="")
		mrtconfig <- c(paste('INPUT_FILENAME = ', hdffile, sep=""), 
				'SPECTRAL_SUBSET = ( 1 1 1 1 0 1 1 0 0 0 0 1 0 )',
				paste('OUTPUT_FILENAME = ', outdir,"/", basename(hdffile),'.tif', sep=""), 
				'RESAMPLING_TYPE = NEAREST_NEIGHBOR', 
				'OUTPUT_PROJECTION_TYPE = SIN',
				'OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )')
		writeLines(mrtconfig,filename)
		success <- system(paste(MRT, '/resample -p ', MRT, '/modisconfig.prm', sep=""))
		if (success==0) success <- TRUE else success <- FALSE 
		if (rm.hdf) unlink(hdffile)
	}	
	return(success)
}

