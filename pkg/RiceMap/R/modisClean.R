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

modisClean <- function(inpath, format="raster", tiles="all", snowx=TRUE, outpath=paste(inpath,"../clean",sep="/")){
    
    #outpath <- paste(inpath,"/../clean",sep="")
    if (!file.exists(outpath)) dir.create(outpath, recursive=TRUE)
           
	FltNA <- -9999.0
    IntNA <- -15
    
    m <- modisFiles(path=inpath)
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
        
        batch <- m[m$zone==tile,]
        dates <- unique(batch$acqdate)

        for (d in dates){
            dlab <- paste("Date ", d, ":", sep ="")
            fname <- paste(outpath, "/", d, "_", tile, "_", sep="")
			
			batch <- m[m$date==d & m$zone==tile,]
            cat(dlab, "Calculating masks. \r")
			flush.console()

			qfile <- paste(inpath,batch$filename[batch$band=="state_500m"], sep="/")
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
