# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario, Andrew Nelson
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3

modis.clean <- function(modfiles, masklist, modisdate, scalebands=c("b01", "b02", "b03", "b04", "b05", "b06", "b07"), scalemultiplier=0.0001, incmask=TRUE, verbose=TRUE){
        
    files <- modfiles[modfiles$acqdate==modisdate,]
	bands <- stack(files$filename)
	NAvalue(bands) <- -28672
	
    bandsval <- values(bands)
	colnames(bandsval) <- files$band
	
    bandsval[,colnames(bandsval) %in% scalebands] <- bandsval[,colnames(bandsval) %in% scalebands]*scalemultiplier 	
	masks <- modis.compute(as.data.frame(bandsval), funlist=masklist)
    bandsval[,-ncol(bandsval)] <- modis.mask(bandsval[,-ncol(bandsval)],masks) 
    
    if(incmask){         
        bandsval <- as.data.frame(cbind(bandsval[,-ncol(bandsval)],masks))
    }
    colnames(bandsval) <- paste(colnames(bandsval),"clean",sep=".")
    mdata <- new("modis.data")
        mdata@product <- files$product[1]
        mdata@acqdate <-  files$acqdate[1]
        mdata@zone <- files$zone[1]
        mdata@version <- files$version[1]
        mdata@proddate <- files$proddate[1]
        mdata@projection <- projection(bands)
        mdata@extent <- extent(bands)
        mdata@ncols <- ncol(bands)
        mdata@nrows <- nrow(bands)
        mdata@imgvals <- bandsval
     
	if(verbose){
        cat (modisdate, " -------------------- DONE -------------------- \n")
        flush.console()	
    }
    rm(masks,bands,bandsval)
    gc()
    return(mdata)  
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
			b3 <- raster(b3file)
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
                rnew <- raster(rq)
                rnew[] <- vbands[,i]    
                rnew[is.na(rnew[])] <- FltNA
                if (outformat=="GTiff"){
                    bfname <- paste(fname, batch$band[i], ".clean.tif", sep="")
                    rnew <- writeRaster(rnew, filename=bfname, format=outformat, options=c("COMPRESS=LZW", "TFW=YES"), overwrite=TRUE)
                } else {
                    rnew <- writeRaster(rnew,filename=paste(fname, batch$band[i], ".clean.grd", sep=""), format=outformat, datatype="FLT4S", overwrite=TRUE)                    
                }
                #band1 <- NDSI
                #band1[is.na(band1)] <- FltNA
                #rnew <- raster2SGDF(rq,vals=band1)    
                #bfname <- paste(fname, "ndsi.tif", sep="")
                #if (file.exists(bfname)) file.remove(bfname)
                #rnew <- writeGDAL(rnew,bfname, options=c("COMPRESS=LZW", "TFW=YES"))
                rm(rnew)
                
            } 

			cat (dlab, " -------------------- DONE -------------------- \n")
            flush.console()
            rm(masks,vbands)
            gc(verbose=FALSE)
            
        }
    }        
}
