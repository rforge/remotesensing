# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario, Andrew Nelson
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3

# sum (snow count)
mysum <- function(x){ 
	sum(x, na.rm=T)  
}

modisClean <- function(inpath, outformat="raster", tiles="all", snowx=TRUE){
    
    outpath <- paste(inpath,"/../clean",sep="")
    if (!file.exists(outpath)) dir.create(outpath, recursive=TRUE)
    
    if (!outformat %in% c("raster","GTiff")){
        cat("Unrecognized output format. Saving as raster (.grd). \n")
        flush.console()
    }
                
	FltNA <- -9999.0
    IntNA <- -15
    
    m <- modisFiles(inpath, pat=".*.tif")
    avtiles <- unique(m$zone)
    
    # processing of all tiles in a directory
    if (tiles=="all"){
		cat("Acquiring available tiles in input folder.\n")
		flush.console()
        tiles <- avtiles				
	} else if (!tiles %in% avtiles){
	   stop("The tile you've specified is not available in your input folder.")
    }
	
	for (tile in tiles){
        
		cat("Processing tile:", tile, "\n")
        flush.console()
        
        dates <- unique(m$date[m$zone==tile])
        
        for (d in dates){
            dlab <- paste("Date ", d, ":", sep ="")
            fname <- paste(outpath, "/", d, "_", tile, "_", sep="")
			
			batch <- m[m$date==d & m$zone==tile,]
            cat(dlab, "Calculating masks. \r")
			flush.console()

			qfile <- paste(inpath,batch$filename[batch$band=="sta"], sep="/")
			b3file <- paste(inpath,batch$filename[batch$band=="b03"], sep="/")
			#b <- subset(mm, mm$date == d & mm$band != "sta")
			rq <- raster(qfile)
			
			masks <- modisMask(qfile, b3file, saveRasters=TRUE, outdir=outpath)
			   	
			bands <- stack(paste(inpath,batch$filename[batch$band!="sta"], sep="/"))
			vbands <- getValues(bands)
			vbands[vbands<=-28672] <- NA
			vbands <- vbands*masks/10000
            #for(i in 1:nlayers(bands)){
       		#	cat(dlab, " Applying masks to ",batch$band[i],".\r", sep="")
    		#	flush.console()
            #    vals <- getValues(bands@layers[[i]])
            #    vals[vals<=-28672] <- NA
                
                #if (i==3) masks$b03_mask <- .blueMask(vbands[,i])
            #}
            rm(bands)
            
            cat(dlab, "Computing NDSI and snow mask. \r")
			flush.console()
			
			if (snowx){
				NDSI <- ndsi(vbands[,4],vbands[,2])
				SnowMask2 <- .snowMask2(vbands[,2], NDSI)
			} else {
				NDSI <- ndsi(vbands[,4],vbands[,5])
				SnowMask2 <- .snowMask3(vbands[,2], vbands[,4], NDSI)
			}
			
			SnowMask2[is.na(SnowMask2)] <- IntNA
            
            cat (dlab, " Writing output files.                 \r")
            flush.console()
            
            if (outformat=="GTiff"){
                for(i in 1:ncol(vbands)){
                    band1 <- vbands[,i]
                    band1[is.na(band1)] <- FltNA
                    rnew <- raster2SGDF(rq,vals=band1)    
                    bfname <- paste(fname, batch$band[i], "_clean.tif", sep="")
                    if (file.exists(bfname)) file.remove(bfname)
                    rnew <- writeGDAL(rnew,bfname, options=c("COMPRESS=LZW", "TFW=YES"))
					rm(rnew, band1)
                }
				NDSI[is.na(NDSI)] <- FltNA            
                band1 <- NDSI
                rnew <- raster2SGDF(rq,vals=band1)
				rnew <- writeGDAL(rnew,paste(fname, "ndsi.tif", sep=""), options=c("COMPRESS=LZW", "TFW=YES"), type = "Float32")
				rm(rnew)

                band1 <- SnowMask2
                rnew <- raster2SGDF(rq,vals=band1)
				rnew <- writeGDAL(rnew,paste(fname, "SnowMask2.tif", sep=""), options=c("COMPRESS=LZW", "TFW=YES"), type = "Int16")
				#bfname <- paste(fname, "ndsi.tif", sep="")
                #if (file.exists(bfname)) file.remove(bfname)
                #rnew <- writeGDAL(rnew,bfname, options=c("COMPRESS=LZW", "TFW=YES"))
            } else {
                r <- raster(rq)
                for(i in 1:ncol(vbands)){
                    rnew <- setValues(r, vbands[,i])
                    rnew <- writeRaster(rnew,filename=paste(fname, batch$band[i], "_clean.grd", sep=""), format=outformat, datatype="FLT4S", overwrite=TRUE)
                }
				rnew <- setValues(r, NDSI)
                rnew <- writeRaster(rnew,filename=paste(fname, "ndsi.grd", sep=""), format=outformat, datatype="FLT4S", overwrite=TRUE)
				rnew <- setValues(r, SnowMask2)
                rnew <- writeRaster(rnew,filename=paste(fname, "SnowMask2.grd", sep=""), format=outformat, datatype="INT1U", overwrite=TRUE)                
                #for(i in 1:length(masks)){
                 #   rnew <- setValues(r, masks[[i]])
                  #  rnew[is.na(rnew)] <- 0 
                  #  rnew <- writeRaster(rnew,filename=paste(fname, names(masks)[i], ".grd", sep=""), format=outformat, datatype="INT1S", overwrite=TRUE)
                #}
                #rnew <- setValues(r, NDSI)
                #rnew <- writeRaster(rnew,filename=paste(fname, "NDSI", ".grd", sep=""), format=outformat, datatype="FLT4S", overwrite=TRUE)
                rm(r)
            } 

			cat (dlab, " -------------------- DONE -------------------- \n")
            flush.console()
            rm(rnew, NDSI, SnowMask2, band1, masks, vbands)
            gc(verbose=FALSE)
            
        }
    }        
}
