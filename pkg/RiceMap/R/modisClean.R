# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario, Andrew Nelson
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


modis.clean <- function(modfiles, modisdate, masklist=c("cloud","snow", "water"), bands=c("b01", "b02", "b03", "b04", "b05", "b06", "b07"), scalemultiplier=0.0001, savemask=TRUE, writeto="./clean", verbose=TRUE){
	# check if 64-bit 
	#is64 <- version$arch=="x86_64"
	
    # get only files with acqdate=modisdate
	files <- modfiles[modfiles$acqdate==modisdate,]
	
    if (verbose) message(modisdate, ": Reading MODIS images.\r", appendLF=FALSE)
	#pbands <- files[files$band %in% bands,] 	
	pstack <- suppressMessages(stack(files$filename))
	NAvalue(pstack) <- -28672
	stkvals <- as.data.frame(values(pstack))
	colnames(stkvals) <- files$band
    #rescale all bands except state_500m
    stkvals[,files$band %in% bands] <- stkvals[,files$band %in% bands]*scalemultiplier
	
    # Create modis.data object for mask
	mdata <- new("modis.data")
	mdata@product <- files$product[1]
	mdata@acqdate <-  files$acqdate[1]
	mdata@zone <- files$zone[1]
	mdata@version <- files$version[1]
	mdata@proddate <- files$proddate[1]
	mdata@projection <- projection(pstack)
	mdata@extent <- extent(pstack)
	mdata@ncols <- ncol(pstack)
	mdata@nrows <- nrow(pstack)
	# Duplicate mask storage for results 
	bdata <- mdata
	
	# COMPUTE MASKS
	if (verbose) message(modisdate, ": Identifying ", paste(masklist, collapse=", "), "\r", appendLF=FALSE)
	rbands <- getRequiredBands(masklist)
	mdata@imgvals <- modis.compute(stkvals[,files$band %in% rbands], funlist=masklist, datatype="logical")
	
    # Mask specified bands
    cname <- vector()
    masked <- vector()
	for (i in 1:nrow(files)){            
          if (!files$band[i] %in% bands) next
          cname <- c(cname,files$band[i])
		if (verbose) message(modisdate, ": Applying masks to ", files$band[i], "\r", appendLF=FALSE)
          masked <- cbind(masked,modis.mask(stkvals[,files$band[i]],mdata@imgvals))                        
	}
    colnames(masked) <- cname
    bdata@imgvals <- as.data.frame(masked)
	#}

    rm(masked,stkvals,pstack)
	gc(verbose=FALSE)
	
	if (is.character(writeto)){
		outdir <- normalizePath(writeto, mustWork=FALSE)
		force.directories(outdir, recursive=TRUE)
		
		if (verbose) message(modisdate, ": Writing clean bands to disk.", "\r", appendLF=FALSE)
		modis.brick(bdata, process="clean", writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)
		
		if(savemask){
			if (verbose) message(modisdate, ": Writing mask rasters to disk.", "\r", appendLF=FALSE)
			modis.brick(mdata, process="clean", intlayers=1:ncol(mdata@imgvals),writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)
		}
	}

	if (verbose) message(modisdate, ": -------------------- DONE CLEANING --------------------", eol="\n")
    rm(mdata)
	gc(verbose=FALSE)	
	return(bdata)
}

modis.clean2 <- function(modfiles, modisdate, masklist=c("cloud","snow", "water"), bands=c("b01", "b02", "b03", "b04", "b05", "b06", "b07"), scalemultiplier=0.0001, savemask=TRUE, writeto="./clean", verbose=TRUE){
	if (!require(mvbutils)){
		stop("Cleaning MODIS bands with cache support needs mvbutils package. Kindly install it first.")
	}
	# get only files with acqdate=modisdate
	files <- modfiles[modfiles$acqdate==modisdate,]
	
	if (verbose) message(modisdate, ": Reading MODIS images. ", "\r", appendLF=FALSE)
	#pbands <- files[files$band %in% bands,] 	
	pstack <- suppressMessages(stack(files$filename))
	NAvalue(pstack) <- -28672
	stkvals <- as.data.frame(values(pstack))
	
	mtidy(stkvals)
	# Create modis.data object for mask
	mdata <- new("modis.data")
	mdata@product <- files$product[1]
	mdata@acqdate <-  files$acqdate[1]
	mdata@zone <- files$zone[1]
	mdata@version <- files$version[1]
	mdata@proddate <- files$proddate[1]
	mdata@projection <- projection(pstack)
	mdata@extent <- extent(pstack)
	mdata@ncols <- ncol(pstack)
	mdata@nrows <- nrow(pstack)
	# Duplicate mask storage for results 
	bdata <- mdata
	
	#for (r in 1:nrow(pstack)){
		
		colnames(stkvals) <- files$band
		#rescale all bands except state_500m
		stkvals[,files$band %in% bands] <- stkvals[,files$band %in% bands]*scalemultiplier	
	
		# COMPUTE MASKS
		if (verbose) message(modisdate, ": Identifying ", paste(masklist, collapse=", "), "\r", appendLF=FALSE)
		rbands <- getRequiredBands(masklist)
		mvals <- modis.compute(stkvals[,files$band %in% rbands], funlist=masklist, datatype="logical")
		bvals <- modis.mask(stkvals[,files$band %in% bands],mvals)
		
		mdata@imgvals <- mvals
		bdata@imgvals <- bvals
		rm(mvals,bvals,stkvals)
		gc(verbose=FALSE)
	#}
	
	if (is.character(writeto)){
		outdir <- normalizePath(writeto, mustWork=FALSE)
		force.directories(outdir, recursive=TRUE)
		
		if (verbose) message(modisdate, ": Writing clean bands to disk.", "\r", appendLF=FALSE)
		modis.brick(bdata, process="clean", writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)
		
		if(savemask){
			if (verbose) message(modisdate, ": Writing mask rasters to disk.", "\r", appendLF=FALSE)
			modis.brick(mdata, process="clean", intlayers=1:ncol(mdata@imgvals),writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)
		}
	}
	
	if (verbose) message(modisdate, ": -------------------- DONE CLEANING --------------------", eol="\n")
	rm(mdata)
	gc(verbose=FALSE)	
	return(bdata)
}

modisClean <- function(inpath, outformat="raster", tiles="all", verbose=TRUE){
                                                                                                                    
    m <- modisFiles(path=inpath)
            
    # processing of all tiles in a directory
    if(tiles=="all"){
		cat("Acquiring available tiles in input folder.\n")
		flush.console()
		#print("Press CTRL + C to terminate.")
		tiles <- unique(m$zone)		
	}

    outpath <- paste(inpath,"/../clean",sep="")
    if (!file.exists(outpath)) dir.create(outpath, recursive=TRUE)
    
    if (!outformat %in% c("raster","GTiff")){
        cat("Unrecognized output format. Saving as raster (.grd). \n")
        flush.console()
    }
                
	FltNA <- -9999.0
    
	
	for (tile in tiles){
        
		cat("Processing tile:", tile, "\n")
        flush.console()
        
        
        dates <- unique(m$acqdate)
        
        for (d in dates){
            batch <- m[m$zone==tile & m$acqdate==d,]
            dlab <- paste("Date ", d, ":", sep ="")
            fname <- paste(outpath, "/", d, ".", tile, ".", sep="")
			
			#batch <- m[m$date==d & m$zone==tile,]
            cat(dlab, "Calculating masks. \r")
			flush.console()

			qfile <- paste(inpath,batch$filename[batch$band=="state_500m"], sep="/")
			b3file <- paste(inpath,batch$filename[batch$band=="b03"], sep="/")
			#b3 <- raster(b3file)
			rq <- raster(qfile)
			
			masks <- modisMask(qfile, b3file, saveRasters=TRUE, outdir=outpath)
			   	
			bands <- stack(paste(inpath,batch$filename[batch$band!="state_500m"], sep="/"))
			vbands <- NULL
            for(i in 1:nlayers(bands)){
       			cat(dlab, " Applying masks to ",batch$band[i],".\r", sep="")
    			flush.console()
                vals <- getValues(bands@layers[[i]])
                vals[vals<=-28672] <- NA
                vbands <- cbind(vbands, vals*masks/10000)
                
                #if (i==3) masks$b03_mask <- .blueMask(vbands[,i])
            }
            rm(bands)                                                   
            
            cat(dlab, "Computing NDSI and secondary snow mask. \r")
			flush.console()

            #NDSI <- ndsi(vbands[,4],vbands[,2])
            #masks$SnowMask2 <- .snowMask2(vbands[,2], NDSI)
            
            cat (dlab, " Writing output files.                 \r")
            flush.console()
            
            
            for(i in 1:ncol(vbands)){
                #rnew <- raster(rq)
                rnew <- setValues(rq, vbands[,i])
				
				bfname <- paste(fname, batch$band[i], ".clean.", formatExt(outformat),sep="")
                ifelse(class(try(writeRaster(rnew, filename=bfname, format=outformat, options=c("COMPRESS=LZW", "TFW=YES"), overwrite=TRUE, NAflag=FltNA, datatype="FLT4S")))=='try-error',
					writeRaster(rnew, filename=bfname, format=outformat, options=c("COMPRESS=LZW", "TFW=YES"), overwrite=TRUE, NAflag=FltNA, datatype="FLT4S"), TRUE)
				#band1 <- NDSI
                #band1[is.na(band1)] <- FltNA
                #rnew <- raster2SGDF(rq,vals=band1)    
                #bfname <- paste(fname, "ndsi.tif", sep="")
                #if (file.exists(bfname)) file.remove(bfname)
                #rnew <- writeGDAL(rnew,bfname, options=c("COMPRESS=LZW", "TFW=YES"))
                rm(rnew)
            } 

			message(dlab, " -------------------- DONE CLEANING -------------------- \n")            
            rm(masks,vbands)
            gc(verbose=FALSE)
            
        }
    }        
}
