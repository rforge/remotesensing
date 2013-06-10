# Author: Donna Aguirre, Francis Dimaano, Teejay Menciano, Jorrel Khalil S. Aunario, Kenneth Bruskiewicz, Richard Bruskiewich 
# IRRI
# License GPL3
# Version 1, August 2011

modis.download <- function(tile, years, doy=seq(from=1,to=365, by=8), product="MOD09A1", savedir=getwd(), modis.site="http://e4ftl01.cr.usgs.gov/MOLT/", smart=TRUE, verbose=TRUE, checkurl=TRUE, ...){
	if(!require(RCurl)) stop("Package RCurl not found")
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
			if (!acqdates[j] %in% validff){
				if (verbose) message(acqdates[j], " is not a valid subfolder in modis ftp site. Skipping...", appendLF=TRUE)
				next
			}
			
			product.site <- paste(modis.site, product, ".005", "/", acqdates[j], "/", sep="") # MODIS FTP URL for specified product
						
			if (checkurl){
				if (verbose) message("Checking if ", acqdates[j], " is a valid subfolder in ", dirname(product.site), appendLF=TRUE)
				if(!url.exists(product.site)) {
					message(product.site, "does not exist. Skipping.", appendLF=TRUE)
					next
				}
			} 
			if (verbose) message("Acquiring file list in ", product.site, appendLF=TRUE)		
			
			# get list of files in product.site 
			files <- getURL(product.site, .opts=curlOptions(ftplistonly=TRUE))	
			# parse list by line
			files <- unlist(strsplit(files,"\n")) 
			
			# if product.site has files from specified tile
			if (length(files)>0){
				# get hdf filename for specified tile 
				tilefiles <- gsub("\r", "", files[grep(paste(tile,"hdf",sep=".*."),files)])
								
				# if tile is found on product.site
				if (length(tilefiles)==2){ 
					# extract filenames from html
					hdffile <- substr(tilefiles[1], regexpr(product,tilefiles[1]), regexpr("hdf",tilefiles[1])+2)
					xmlfile <- substr(tilefiles[2], regexpr(product,tilefiles[2]), regexpr("xml",tilefiles[2])+2)
										
					if (file.exists(paste(savedir,hdffile, sep="/")) & smart) {
						# File already present in local savedir
						if (verbose) message(hdffile, " exists locally.", appendLF=TRUE)
						
						if (verbose) message("Checking integrity...", appendLF=FALSE)
						xml <- unlist(strsplit(getURL(paste(product.site, xmlfile, sep="")),"\n"))
						cksumver <- try(system("cksum --version", intern=TRUE), silent=TRUE)
						# Validating existing file
						if (class(cksumver)!="try-error"){
							cksum <- system(paste("cksum", paste(savedir,hdffile, sep="/")), intern=TRUE)
							cksum <- unlist(strsplit(cksum[length(cksum)], " "))[1]
							chk <- xml[grep("Checksum>",xml)]
							idx <- unlist(gregexpr("[[:digit:]]", chk))
							chk <- substr(chk, min(idx), max(idx))						
						} else {
							cksum <- file.info(paste(savedir,hdffile, sep="/"))$size
							chk <- xml[grep("FileSize>",xml)]
							idx <- unlist(gregexpr("[[:digit:]]", chk))
							chk <- as.numeric(substr(chk, min(idx), max(idx)))
						}
						
						if(chk==cksum) {
							message(" SUCCESS!", appendLF=TRUE)
							result <- c(result,paste(savedir,hdffile,sep="/"))
							next
						} else {
							message(" FAILED! Redownload in progress.", appendLF=TRUE)
							unlink(paste(savedir,hdffile, sep="/")) 
						}
					} 
															
					# File not yet downloaded - attempt to get it!
					if (verbose) message("Downloading ", product.site, hdffile, appendLF=TRUE)		
					hdf <- download.file(paste(product.site, hdffile, sep=""), destfile=paste(savedir,hdffile, sep="/"), method='internal', mode='wb',quiet=!verbose)
					
					# check integrity
					if (verbose) message("Checking integrity...", appendLF=FALSE)
					xml <- unlist(strsplit(getURL(paste(product.site, xmlfile, sep="")),"\n"))
					if (system("cksum --version", show.output.on.console=FALSE)==0){
						cksum <- system(paste("cksum", paste(savedir,hdffile, sep="/")), intern=TRUE)
						cksum <- unlist(strsplit(cksum[length(cksum)], " "))[1]
						chk <- xml[grep("Checksum>",xml)]
						idx <- unlist(gregexpr("[[:digit:]]", chk))
						chk <- substr(chk, min(idx), max(idx))						
					} else {
						cksum <- file.info(paste(savedir,hdffile, sep="/"))$size
						chk <- xml[grep("FileSize>",xml)]
						idx <- unlist(gregexpr("[[:digit:]]", chk))
						chk <- as.numeric(substr(chk, min(idx), max(idx)))
					}
					
					# Verify successful download
					if(chk==cksum) {
						message(" SUCCESS!", appendLF=TRUE)
						result <- c(result,paste(savedir,hdffile,sep="/"))
					} else {
						message("FAILED. Try to redownload later.", appendLF=TRUE)
						unlink(paste(savedir,hdffile, sep="/")) 
					}
					
				} else {
					message(tile, " not found in ", product.site, appendLF=TRUE) 
				}
			}				
		}		
	} 
	return(result)
}

modis.hdf2tif <- function(hdffile, outdir=getwd(), MRT_HOME=Sys.getenv("MRT_HOME"), rm.hdf=FALSE, res.files=TRUE, spectral_subset=c(1,1,1,1,0,1,1,0,0,0,0,1,0), options=vector(),...){
	
	success <- FALSE
	
	if (!force.directories(outdir)){
		stop("Kindly ensure you have the necessary permissions to use \n", outdir)
	}
	
	#Check existing TIFF images related to hdffile. 
	xoutput <- dir(outdir, pattern=sub(".hdf","",basename(hdffile)), ...)
	
	# Skip if exists.
	if (length(xoutput)<sum(spectral_subset)){
		
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
					'SPECTRAL_SUBSET = ( ', paste(spectral_subset, collapse=" "),' )',
					paste('OUTPUT_FILENAME = ', outdir,"/", sub(".hdf","",basename(hdffile)),'.tif', sep=""), 
					'RESAMPLING_TYPE = NEAREST_NEIGHBOR', 
					'OUTPUT_PROJECTION_TYPE = SIN',
					'OUTPUT_PROJECTION_PARAMETERS = ( 6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 )',
					options)
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

