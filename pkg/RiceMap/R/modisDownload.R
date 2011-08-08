# Author: Donna Aguirre, Francis Dimaano, Teejay Menciano, Jorrel Khalil S. Aunario, Kenneth Bruskiewicz, Richard Bruskiewich 
# IRRI
# License GPL3
# Version 1, August 2011

modis.download <- function(tile, ftp_dir, savedir, redownload=FALSE, verbose=TRUE){
	success <- 0
	if (verbose) show.message(paste("Acquiring file list in", ftp_dir), eol="\n")
	force.directories(savedir)
	files <- withRetry(unlist(strsplit(getURL(ftp_dir, .opts=curlOptions(ftplistonly=TRUE)),"\n")))
	if (length(files)>0){
		BLOCK1 <- gsub("\r", "", files[grep(paste(tile,"hdf",sep=".*."),files)])
		BLOCK1 <-  BLOCK1[-grep("xml",BLOCK1)]
		
		if (length(BLOCK1)!=0){
			dfile <- paste(dldir, BLOCK1, sep="/")
			if (file.exists(dfile) & !redownload) {
				if (verbose) show.message(paste(BLOCK1, " Exists. ", sep=""), eol="\n")
				success <- dfile	
			} else {
				if (verbose) show.message(paste("Downloading ", BLOCK1, sep=""), eol="\n")		
				flag <- withRetry(download.file(paste(ftp_dir, BLOCK1,sep="/"), destfile=dfile, method='internal', mode='wb',quiet=TRUE),retries=49)
				
				if(class(flag)!="logical") success <- dfile	else unlink(dfile)			
			}
			
		} else {
			show.message(paste(tile, "not found in", ftp_dir), eol="\n")
			success <- 1
		}
	}	
	return(success)
}

modis.hdf2tif <- function(hdffile, outdir, MRT_HOME=normalizePath(Sys.getenv("MRT_HOME"),winslash="/"), rm.hdf=FALSE){
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