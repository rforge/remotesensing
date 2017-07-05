# Author: Donna Aguirre, Francis Dimaano, Teejay Menciano, Jorrel Khalil S. Aunario, Kenneth Bruskiewicz, Richard Bruskiewich 
# IRRI
# License GPL3
# Version 1, August 2011

dl.fast <- 0
dl.smart <- 1
dl.renew <- 2

modprods <- read.csv(system.file("modis.products.ref.csv", package="RiceMap"), stringsAsFactors=FALSE)

modis.integrity <- function(localfile, xml){
	cksumver <- Sys.which("cksum")	
	if (cksumver==""){
		cksum <- file.info(localfile)$size
		chk <- xml[grep("FileSize>",xml)]
		idx <- unlist(gregexpr("[[:digit:]]", chk))
		chk <- as.numeric(substr(chk, min(idx), max(idx)))
	} else {
		cksum <- system(paste("cksum", localfile), intern=TRUE)
		cksum <- unlist(strsplit(cksum[length(cksum)], " "))[1]
		chk <- xml[grep("Checksum>",xml)]						
		idx <- unlist(gregexpr("[[:digit:]]", chk))
		chk <- substr(chk, min(idx), max(idx))
		if (grepl("\\|", chk)) chk <- unlist(strsplit(cksum[length(chk)], "\\|"))[1]
	}
	return(cksum==chk)
}

modis.download <- function(tile, years, doy=NULL, product="MOD09A1", prod.ver=6, savedir=NULL, modis.site="http://e4ftl01.cr.usgs.gov/", dl.mode=dl.smart, integrity=TRUE, skip.exists=TRUE, verbose=TRUE, ...){
	#Initialize required objects
	if (is.null(savedir)){
		dirs <- apply(expand.grid(product, tiles, stringsAsFactors = FALSE), FUN = paste, MARGIN = 1, collapse="/")
		lapply(dirs, FUN=force.directories,recursive=TRUE)
	} else if (!force.directories(savedir,recursive=TRUE)){ # Ensure the path exists on disk
		stop("Unable to create save directory. Kindly ensure you have the necessary permissions to use \n", savedir)
	}
	
	if (dl.mode==dl.fast) {
		skip.exists <- TRUE
		integrity <- FALSE
	} else if (dl.mode==dl.smart) {
		integrity <- TRUE
	} else if (dl.mode==dl.renew) {
		skip.exists <- FALSE			
	}
	
	result <- vector()
	
	for (pr in product){
		prod.info <- modprods[grep(pr,modprods$ShortName),]
		
		if(!grepl("day", prod.info$Temporal.Granularity)){
			warning("Unsupported product ", product,". Kindly contact the developer.")
			next
		}
		prod.site <- paste(modis.site, "MO", switch(prod.info$Platform, Aqua="LA", Terra="LT", Combined="TA"), "/", paste(pr,sprintf(paste("%03d",sep=""),prod.ver),sep="."),sep="")
		if (is.null(doy)){
			tim.gran <- paste("t",gsub(" ", "", prod.info$Temporal.Granularity),sep="")
			doy <- switch(tim.gran, t4day=seq(from=1,to=365, by=4), t8day=seq(from=1,to=365, by=8), t16day=ifelse(rep(prod.info$Platform,23)=="Aqua", seq(from=9,to=365, by=16), seq(from=1,to=365, by=16)))				
		}
		
		for (yr in years) {
			for (d in doy){
				date.site <- paste(prod.site, "/",	format(as.Date(paste(yr, d), "%Y %j"), "%Y.%m.%d"), "/", sep="")				
				if (verbose) message("Acquiring file list in ", date.site, appendLF=TRUE)
				date.page <- unlist(strsplit(getURL(date.site, dirlistonly=TRUE),"\n"))
				acqdate <- paste("A",yr,sprintf("%03d",d),sep="")
				
				for (tl in tile){
					if (is.null(savedir)) tile.dir <- paste(pr,tl, sep="/") else tile.dir <- savedir
					tile.page <- date.page[grep(paste(pr, acqdate, tl, sep="."),date.page)]
					tile.page <- tile.page[-grep("BROWSE",tile.page)]
					link.st <- regexpr(paste(">",product,".*./",sep=""), tile.page)
					link.en <- regexpr("</a>", tile.page)
					tilefiles <- substr(tile.page, link.st+1,link.en-1)
					tile.site <- paste(date.site, tilefiles, sep="")
					
					if (length(tilefiles)==2){ 
						# extract filenames from html
						hdffile <- tilefiles[1]
						xmlfile <- tilefiles[2]
						
						if (file.exists(paste(tile.dir, hdffile, sep="/")) & skip.exists) {
							if (verbose) message(hdffile, " exists locally.", appendLF=TRUE)
							result <- c(result,paste(savedir,hdffile,sep="/"))
							next
							# File already present in local savedir	
						} 
						
						if (integrity) {
							download.file(tile.site[2], destfile=xmlfile, ...)
							xml <- readLines(xmlfile)
							file.remove(xmlfile)
							#xml <- unlist(strsplit(getURL(paste(product.site, xmlfile, sep="")),"\n"))
						}
						
						if (file.exists(paste(tile.dir, hdffile, sep="/")) & integrity){
							if (verbose) {
								message(hdffile, " exists locally.", appendLF=TRUE)
								message("Checking integrity...", appendLF=FALSE)
							}
							
							if(modis.integrity(localfile=paste(tile.dir, hdffile, sep="/"),xml=xml)) {
								message(" SUCCESS!", appendLF=TRUE)
								result <- c(result,paste(savedir,hdffile,sep="/"))
								next
							} else {
								#if (verbose) message("Downloading ", product.site, hdffile, appendLF=TRUE)		
								#hdf <- download.file(paste(product.site, hdffile, sep=""), destfile=paste(savedir,hdffile, sep="/"), method='internal', mode='wb',quiet=!verbose)								
								message("FAILED. Removing old file.", appendLF=TRUE)
								file.remove(paste(tile.dir, hdffile, sep="/")) 
							}
						} else if (file.exists(paste(tile.dir, hdffile, sep="/")) & !skip.exists){
							message("Removing old file.", appendLF=TRUE)
							file.remove(paste(savedir,hdffile, sep="/"))
						}
						
						
						# File not yet downloaded - attempt to get it!
						if (verbose) message("Downloading ", hdffile, appendLF=TRUE)		
						hdf <- download.file(tile.site[1], destfile=paste(tile.dir, hdffile, sep="/"), ...)
						
						# check integrity
						if (integrity){
							if (verbose) message("Checking integrity...", appendLF=FALSE)
							# Verify successful download
							if(modis.integrity(localfile=paste(tile.dir, hdffile, sep="/"),xml=xml)) {
								message(" SUCCESS!", appendLF=TRUE)
								result <- c(result,paste(tile.dir, hdffile, sep="/"))
								next
							} else {
								message("FAILED. Try to redownload later.", appendLF=TRUE)
								file.remove(paste(tile.dir, hdffile, sep="/")) 
							}
						}					
					} else {
						message(tile, " not found in ", prod.site, appendLF=TRUE) 
					}
					
				}			
			}
		}
	}
	
	return(result)
}

modis.hdf2tif <- function(hdffile, outdir=getwd(), MRT_HOME=Sys.getenv("MRT_HOME"), rm.hdf=FALSE, res.files=TRUE, spectral_subset=c(1,1,1,1,0,1,1,0,0,0,0,1,0), output_projection="SIN", resampling_type="NEAREST_NEIGHBOR", OPP="6371007.181 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0",options=vector(),...){
	
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
					paste('SPECTRAL_SUBSET = ( ', paste(spectral_subset, collapse=" "),' )', sep=""),
					paste('OUTPUT_FILENAME = ', outdir,"/", sub(".hdf","",basename(hdffile)),'.tif', sep=""), 
					paste('RESAMPLING_TYPE =', resampling_type), 
					paste('OUTPUT_PROJECTION_TYPE =', toupper(output_projection)),
					paste('OUTPUT_PROJECTION_PARAMETERS = (', OPP,')'),
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

