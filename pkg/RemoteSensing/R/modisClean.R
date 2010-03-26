# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


# Internal cloud  algorithm flag
.internalCloud <- function (pixel) {
	pixel <- modis.sqa500f(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
	return(pixel)
}

# Cloud shadow mask
.cloudShadow <- function (pixel) {
	pixel <- modis.sqa500b(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
	return(pixel)
}

# Water mask
.waterMask <- function(pixel) {
	res <- modis.sqa500c(pixel)
	res[res < 1 | res == 2 | res >= 3 ] <- 0
	res[res >= 1] <- 1
	return(res)
}

# Internal Snow mask
.snowMask <- function(pixel) {
	pixel <- modis.sqa500k(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
	return(pixel)
}

# Blue mask
.blueMask <- function(pixel) {
    pixel[pixel >= 0.2] <- NA
	pixel[pixel < 0.2] <- 1
	pixel[is.na(pixel)] <- 0
	return(pixel)
}


# second snow mask
.snowMask2 <- function(nir, ndsi) {
	res <- rep(NA, length(nir))
	res [!((nir > 0.11) & (ndsi > 0.40))]<- 1
	res[(nir > 0.11) & (ndsi > 0.40)] <- 0
	res[is.na(res)] <- 0
	return(res)
}

# sum (snow count)
mysum <- function(x){ 
	sum(x, na.rm=T)  
}

modisClean <- function(inpath, outformat="raster", tiles="all"){
    
    outpath <- paste(inpath,"/../clean",sep="")
    if (!file.exists(outpath)) dir.create(outpath, recursive=TRUE)

	FltNA <- -9999.0
    
    if (outformat=="raster"){
        ext <- ".grd"
    } else if (outformat=="GTiff"){
        if (!require(rgdal)) stop("rgdal loading failed")    
        ext <- ".tif"
        opts <- c("COMPRESS=LZW", "TFW=YES")
    } else {
        cat(paste("Unsupported output format '", outformat, "'. Will write files in raster instead.", sep=""))
        ext <- ".grd"
        outformat <- "raster"                
    }
			
    # processing of all tiles in a directory
    if(tiles=="all"){
		cat("Acquiring available tiles in input folder.\n")
		flush.console()
		#print("Press CTRL + C to terminate.")
		tiles <- unique(substr(list.files(inpath, pattern="001.*b01.*.tif"), 18, 23))		
	}
	
	for (tile in tiles){
        pat <- paste(tile, ".*.tif", sep="")
        m <- modisFiles(inpath, pat)
        
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
			#b <- subset(mm, mm$date == d & mm$band != "sta")
			rq <- raster(qfile, values=TRUE)
			
			masks <- list()
			masks$CloudMask <- .internalCloud(values(rq))
			masks$ShadowMask <- .cloudShadow(values(rq))
            masks$WaterMask <- .waterMask(values(rq))
			masks$SnowMask <- .snowMask(values(rq))
            
            masks$CloudMask[masks$CloudMask==0] <- NA 
			masks$ShadowMask[masks$ShadowMask==0] <- NA 
			masks$WaterMask[masks$WaterMask==0] <- NA
			masks$SnowMask[masks$SnowMask==0] <- NA
			   	
			bands <- stack(paste(inpath,batch$filename[batch$band!="sta"], sep="/"))
			vbands <- NULL
            for(i in 1:nlayers(bands)){
       			cat(dlab, " Applying masks to ",batch$band[i],".\r", sep="")
    			flush.console()
                vals <- getValues(bands@layers[[i]])
                vals[vals<=-28672] <- NA
                vbands <- cbind(vbands, vals*masks$CloudMask*masks$ShadowMask*masks$WaterMask*masks$SnowMask/10000)
                
                if (i==3) masks$b03_mask <- .blueMask(vbands[,i])
            }
            rm(bands)
            
            cat(dlab, "Computing NDSI and secondary snow mask. \r")
			flush.console()

            NDSI <- ndsi(vbands[,4],vbands[,2])
            masks$SnowMask2 <- .snowMask2(vbands[,2], NDSI)
            
            cat (dlab, " Writing output files.                 \r")
            flush.console()
            
            if (outformat=="raster"){
                r <- raster(rq)
                for(i in 1:ncol(vbands)){
                    rnew <- setValues(r, vbands[,i])
                    rnew <- writeRaster(rnew,filename=paste(fname, batch$band[i], "_clean",ext, sep=""), format=outformat, datatype="FLT4S", overwrite=TRUE)
                }                
                for(i in 1:length(masks)){
                    rnew <- setValues(r, masks[[i]])
                    rnew[is.na(rnew)] <- 0 
                    rnew <- writeRaster(rnew,filename=paste(fname, names(masks)[i], ext, sep=""), format=outformat, datatype="INT1S", overwrite=TRUE)
                }
                rnew <- setValues(r, NDSI)
                rnew <- writeRaster(rnew,filename=paste(fname, "NDSI", ext, sep=""), format=outformat, datatype="FLT4S", overwrite=TRUE)
                rm(r)
            } else if (outformat=="GTiff"){
                proj <- CRS(projection(rq))
                gtop <- GridTopology(c(xmin(rq)+(xres(rq)/2),ymin(rq)+(yres(rq)/2)),c(xres(rq),yres(rq)),c(ncol(rq),nrow(rq)))
                for(i in 1:ncol(vbands)){
                    band1 <- vbands[,i]
                    band1[is.na(band1)] <- FltNA
                    band1 <- as.data.frame(band1)
                    bfname <- paste(fname, batch$band[i], "_clean", ext, sep="")
                    rnew <- SpatialGridDataFrame(gtop, band1, proj4string=proj)
                    if (file.exists(bfname)) file.remove(bfname)
                    rnew <- writeGDAL(rnew,bfname, options=opts)
                }
                for(i in 1:length(masks)){
                    band1 <- masks[[i]]
                    band1[is.na(band1)] <- 0
                    band1 <- as.data.frame(band1)
                    bfname <- paste(fname, names(masks)[i], ext, sep="")
                    rnew <- SpatialGridDataFrame(gtop, band1, proj4string=proj)
                    if (file.exists(bfname)) file.remove(bfname)
                    rnew <- writeGDAL(rnew,bfname, options=opts, type = "Int16")
                }
                band1 <- NDSI
                band1[is.na(band1)] <- FltNA    
                band1 <- as.data.frame(band1)
                bfname <- paste(fname, "ndsi", ext, sep="")
                rnew <- SpatialGridDataFrame(gtop, band1, proj4string=proj)
                if (file.exists(bfname)) file.remove(bfname)
                rnew <- writeGDAL(rnew,bfname, options=opts)
                rm(rnew)
                
            }

			cat (dlab, " -------------------- DONE -------------------- \n")
            flush.console()
            rm(masks,vbands)
            gc(verbose=FALSE)
            
        }
    }        
}