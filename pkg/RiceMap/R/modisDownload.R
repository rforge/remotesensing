# Author: Donna Aguirre, Francis Dimaano, Teejay Menciano, Jorrel Khalil S. Aunario, Kenneth Bruskiewicz, Richard Bruskiewich 
# IRRI
# License GPL3
# Version 1, August 2011

modis.download <- function(tile, years, doy=seq(from=1,to=365, by=8), product="MOD09A1", savedir=getwd(), modis.ftp="ftp://e4ftl01.cr.usgs.gov/MOLT/", redownload=FALSE, verbose=TRUE, checkurl=TRUE, ...){

	#Initialize required objects
	result <- vector() # Empty vector will contain
	
	if (!force.directories(savedir,recursive=TRUE)){ # Ensure the path exists on disk
		stop("Unable to create save directory. Kindly ensure you have the necessary permissions to use \n", savedir)
	}
	validff <- validFolders()
	for (i in 1:length(years)){
		# Generate ACQDATE based on year and doy
		acqdates <- format(as.Date(paste(years[i], doy), "%Y %j"), "%Y.%m.%d")
		#subfolders <- as.Date(validFolders()[1], "%Y.%m.%d")
		
		
		for (j in 1:length(acqdates)){
			# Check if acqdate is valid 
			# TODO: verify validFolders result are in ftp site
			if (!acqdates[j] %in% validff){
				if (verbose) message(acqdates[j], " is not a valid subfolder in modis ftp site. Skipping...", appendLF=TRUE)
				next
			}
			
			xstfile <- dir(savedir, pattern=paste(product,paste(years[i],sprintf("%0.3d",doy[j]),sep=""),tile,sep=".*."), full.names=TRUE)
			if (length(xstfile)>0 & !redownload) {
				# File already present in local savedir
				if (verbose) message(xstfile, " exists locally. Skipping download.", appendLF=TRUE)
				result <- c(result,xstfile)
				next
			}
			
			product.ftp <- paste(modis.ftp, product, ".005", "/", acqdates[j], "/", sep="") # MODIS FTP URL for specified product
						
			if (checkurl){
				if (verbose) message("Checking if ", acqdates[j], " is a valid subfolder in ", dirname(product.ftp), appendLF=TRUE)
				if(!url.exists(product.ftp)) {
					message(product.ftp, "does not exist. Skipping.", appendLF=TRUE)
					next
				}
			} 
			if (verbose)  {
				message("Acquiring file list in ", product.ftp, appendLF=TRUE)		
			}
			
			# get list of files in product.ftp 
			files <- getURL(product.ftp, .opts=curlOptions(ftplistonly=TRUE))	
			# parse list by line
			files <- unlist(strsplit(files,"\n")) 
			
			# if product.ftp has files from specified tile
			if (length(files)>0){
				# get hdf filename for specified tile 
				hdffile <- gsub("\r", "", files[grep(paste(tile,"hdf",sep=".*."),files)])
				#remove xml from file list
				hdffile <-  hdffile[-grep("xml",hdffile)]
				
				# if tile is found on product.ftp
				if (length(hdffile)!=0){ 
					
					# File not yet downloaded - attempt to get it!
					if (verbose) message("Downloading ", product.ftp, hdffile, appendLF=TRUE)		
					items <- download.file(paste(product.ftp, hdffile, sep=""), destfile=paste(savedir,hdffile, sep="/"), method='internal', mode='wb',quiet=!verbose)
					
					# Verify successful download
					if(length(items)>0) {
						message(hdffile," successfully downloaded!", appendLF=TRUE)
						result <- c(result,paste(savedir,hdffile,sep="/"))
					} else {
						message(product.ftp, hdffile," found but could not be downloaded.", appendLF=TRUE)
						unlink(hdffile) 
					}
				} else {
					message(tile, " not found in ", product.ftp, appendLF=TRUE) 
				}
			}				
		}		
	} 
	return(result)
}

modis.hdf2tif <- function(hdffile, outdir=getwd(), MRT_HOME=Sys.getenv("MRT_HOME"), rm.hdf=FALSE, res.files=TRUE,...){
	
	success <- FALSE
	
	if (!force.directories(outdir)){
		stop("Kindly ensure you have the necessary permissions to use \n", outdir)
	}
	
	#Check existing TIFF images related to hdffile. 
	xoutput <- dir(outdir, pattern=sub(".hdf","",basename(hdffile)), ...)
	
	# Skip if exists.
	if (length(xoutput)!=7){
		
		if(!is.character(hdffile)) {
			message(hdffile," is not a valid HDF file name character string?", appendLF=TRUE)
			return(FALSE)
		}	
		
		if (MRT_HOME=="") {
			message("MRT not installed. Download it here (https://lpdaac.usgs.gov/lpdaac/tools/modis_reprojection_tool)", appendLF=TRUE)
		} else {
			MRT <- paste(MRT_HOME,"bin", sep="/")
			
			filename <- paste(MRT, "/modisconfig.prm", sep="")
			mrtconfig <- c(paste('INPUT_FILENAME = ', hdffile, sep=""), 
					'SPECTRAL_SUBSET = ( 1 1 1 1 0 1 1 0 0 0 0 1 0 )',
					paste('OUTPUT_FILENAME = ', outdir,"/", sub(".hdf","",basename(hdffile)),'.tif', sep=""), 
					'RESAMPLING_TYPE = NEAREST_NEIGHBOR', 
					'OUTPUT_PROJECTION_TYPE = SIN',
					'OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )')
			writeLines(mrtconfig,filename)
			success <- system(paste(MRT, '/resample -p ', MRT, '/modisconfig.prm', sep=""))
			if (success==0) {
				success <- TRUE
				xoutput <- dir(outdir, pattern=sub(".hdf","",basename(hdffile)), ...)
			} else success <- FALSE 
			if (rm.hdf) unlink(hdffile)
		}
		
	} else success <- TRUE
	
	if (res.files){
		success <- xoutput
	}
	return(success)
}

