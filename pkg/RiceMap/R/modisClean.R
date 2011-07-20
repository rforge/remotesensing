# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario, Andrew Nelson
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


modis.clean <- function(modfiles, modisdate, masklist=c("cloud","snow", "water"), bands=c("b01", "b02", "b03", "b04", "b05", "b06", "b07"), scalemultiplier=0.0001, savemask=TRUE, writeto=NA, verbose=TRUE){
	# check if 64-bit 
	is64 <- version$arch=="x86_64"
	
    # get only files with acqdate=modisdate
	files <- modfiles[modfiles$acqdate==modisdate,]
	
	# COMPUTE MASKS
	if (verbose) show.message(modisdate, ": Identifying ", paste(masklist, collapse=", "), eol="\r")
	rbands <- getRequiredBands(masklist)
	mstack <- stack(files$filename[files$band %in% rbands])

	mdata <- new("modis.data")
	mdata@product <- files$product[1]
	mdata@acqdate <-  files$acqdate[1]
	mdata@zone <- files$zone[1]
	mdata@version <- files$version[1]
	mdata@proddate <- files$proddate[1]
	mdata@projection <- projection(mstack)
	mdata@extent <- extent(mstack)
	mdata@ncols <- ncol(mstack)
	mdata@nrows <- nrow(mstack)
	
	bdata <- mdata
	
	NAvalue(mstack) <- -28672
	
	stkvals <- values(mstack)
	colnames(stkvals) <- rbands
	stkvals[,rbands %in% bands] <- stkvals[,rbands %in% bands]*scalemultiplier 
	mdata@imgvals <- modis.compute(as.data.frame(stkvals), funlist=masklist, datatype="logical")
	rm(stkvals)
	gc(verbose=FALSE)
	
	pbands <- files[files$band %in% bands,] 	
	pstack <- stack(pbands$filename)
	NAvalue(pstack) <- -28672
	bdata@imgvals <-as.data.frame(values(pstack)*scalemultiplier)
	colnames(bdata@imgvals) <- pbands$band
	
	if (is64){
		if (verbose) show.message(modisdate, ": Applying masks", eol="\r")
		bdata@imgvals <- modis.mask(bdata@imgvals,mdata@imgvals)
		gc(verbose=FALSE)
	} else {
		for (i in 1:ncol(bdata@imgvals)){
			if (verbose) show.message(modisdate, ": Applying masks to ", colnames(bdata@imgvals)[i], eol="\r")
			bdata@imgvals[,i] <- modis.mask(bdata@imgvals[,i],mdata@imgvals)
		}
		gc(verbose=FALSE)
	}
	
	if (is.character(writeto)){
		outdir <- normalizePath(writeto, mustWork=FALSE)
		force.directories(outdir, recursive=TRUE)
		
		if (verbose) show.message(modisdate, ": Writing clean bands to disk.", eol="\r")
		modis.brick(bdata, process="clean", writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)
		
		if(savemask){
			if (verbose) show.message(modisdate, ": Writing mask rasters to disk.", eol="\r")
			modis.brick(mdata, process="clean", intlayers=1:ncol(mdata@imgvals),writeto=outdir, options="COMPRESS=LZW", overwrite=TRUE)
			#for (i in 1:ncol(masks)){
			#	praster <- setValues(raster(mstack),masks[,i])
			#	fname <- paste(pbands$acqdate[i], pbands$zone[i], colnames(masks)[i], "clean", pbands$format[i], sep=".")
			#	fname <- as.character(paste(outdir,fname,sep="/"))
			#	writeRaster(praster,filename=fname,format="GTiff", options="COMPRESS=LZW", NAflag=-15, overwrite=TRUE, datatype="INT2S")
			#}
		}
		#for (i in 1:ncol(mdata@imgvals)){
			
		#	praster <- setValues(raster(pstack),bandsval[,i])
		#	fname <- as.character(paste(outdir, paste(pbands$acqdate[i], pbands$zone[i], colnames(bandsval)[i], "clean", pbands$format[i], sep="."),sep="/"))
		#	writeRaster(praster,filename=fname,format="GTiff", options="COMPRESS=LZW", NAflag=-9999.0, overwrite=TRUE)
		#}
	}

	if (verbose) 			show.message(modisdate, ": -------------------- DONE CLEANING --------------------", eol="\n")
    rm(mstack,pstack,mdata)
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

			show.message(dlab, " -------------------- DONE CLEANING -------------------- \n")            
            rm(masks,vbands)
            gc(verbose=FALSE)
            
        }
    }        
}
