# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario, Andrew Nelson
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


modisClean <- function(inpath, format="raster", tiles="all", snowx=TRUE){
    
    outpath <- paste(inpath,"/../clean",sep="")
    if (!file.exists(outpath)) dir.create(outpath, recursive=TRUE)
           
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
            
            r <- raster(rq)
             for(i in 1:ncol(vbands)){
                 rnew <- setValues(r, vbands[,i])
                 rnew <- writeRaster(rnew,filename=paste(fname, batch$band[i], "_clean", sep=""), format=format, datatype="FLT4S", overwrite=TRUE)
             }
				rnew <- setValues(r, NDSI)
                rnew <- writeRaster(rnew,filename=paste(fname, "ndsi", sep=""), format=format, datatype="FLT4S", overwrite=TRUE)
				rnew <- setValues(r, SnowMask2)
                rnew <- writeRaster(rnew,filename=paste(fname, "SnowMask2", sep=""), format=format, datatype="INT1U", overwrite=TRUE)                
                rm(r)
             

			cat (dlab, " -------------------- DONE -------------------- \n")
            flush.console()
            rm(rnew, NDSI, SnowMask2, masks, vbands)
            gc(verbose=FALSE)
            
        }
    }        
}
