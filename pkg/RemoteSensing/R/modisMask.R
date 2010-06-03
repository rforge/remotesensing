# Author: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario 
# International Rice Research Institute
# Date : 21 May 2010
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

modisMask <- function(qcimage, saveRasters=FALSE, outdir=NULL){
    rq <- raster(qcimage, values=TRUE)
	vals <- values(rq)		
	masks <- data.frame(CloudMask=integer(ncell(rq)), ShadowMask=integer(ncell(rq)), WaterMask=integer(ncell(rq)), SnowMask=integer(ncell(rq)))
	masks$CloudMask <- .internalCloud(vals)
	masks$ShadowMask <- .cloudShadow(vals)
    masks$WaterMask <- .waterMask(vals)
	masks$SnowMask <- .snowMask(vals)
    
    mask <- masks$CloudMask*masks$ShadowMask*masks$WaterMask*masks$SnowMask
    mask[mask==0] <- NA
    
    if(saveRasters){
        for(i in 1:length(masks)){
            band1 <- masks[[i]]
            band1[is.na(band1)] <- 0
            band1 <- as.data.frame(band1)
            bfname <- paste(outdir, names(masks)[i], ".tif", sep="")
            rnew <- SpatialGridDataFrame(gtop, band1, proj4string=proj)
            if (file.exists(bfname)) file.remove(bfname)
            rnew <- writeGDAL(rnew,bfname, options=c("COMPRESS=LZW", "TFW=YES"), type = "Int16")
        }
                    
    }            
    #masks$CloudMask[masks$CloudMask==0] <- NA 
	#masks$ShadowMask[masks$ShadowMask==0] <- NA 
	#masks$WaterMask[masks$WaterMask==0] <- NA
	#masks$SnowMask[masks$SnowMask==0] <- NA
	rm(masks,rnew)
    return(mask)    
}
