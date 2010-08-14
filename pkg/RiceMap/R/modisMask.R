# Author: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario 
# International Rice Research Institute
# Date : 21 May 2010
# Version 0,1
# Licence GPL v3

# Cloud Mask
.cloudMask <- function(b3){
	b3 <- b3/100
	res <- (((b3 > 18)*0) + (b3 < 18))
    return(res)
}

# Internal cloud  algorithm flag
#.internalCloud <- function (pixel) {
#	pixel <- modis.sqa500f(pixel)
#	pixel <- pixel + 1
#	pixel[pixel > 1] <- 0
#	return(pixel)
#}

# Cloud shadow mask
#.cloudShadow <- function (pixel) {
#	pixel <- modis.sqa500b(pixel)
#	pixel <- pixel + 1
#	pixel[pixel > 1] <- 0
#	return(pixel)
#}

# Water mask
.waterMask <- function(pixel) {
	res <- modis.sqa500c(pixel)
	res[res < 1 | res == 2 | res >= 3 ] <- 0
	res[res >= 1] <- 1
	return(res)
}

#Internal Snow mask
.snowMask <- function(pixel) {
	pixel <- modis.sqa500k(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
	return(pixel)
}

# Blue mask
#.blueMask <- function(pixel) {
#    pixel[pixel >= 0.2] <- NA 
#	pixel[pixel < 0.2] <- 1
#	pixel[is.na(pixel)] <- 0
#	return(pixel)
#}

#second snow mask
.snowMask2 <- function(nir, ndsi) {
	res <- rep(NA, length(nir))
	res [!((nir > 0.11) & (ndsi > 0.40))]<- 1
	res[(nir > 0.11) & (ndsi > 0.40)] <- 0
	res[is.na(res)] <- -15
	return(res)
}

.snowMask3 <- function(nir, green, ndsi) {
	res <- rep(NA, length(nir))
	res [!((nir > 0.10) & (green > 0.10) & (ndsi >= 0.40))]<- 1
	res[(nir > 0.10) & (green > 0.10) & (ndsi >= 0.40)] <- 0
	res[is.na(res)] <- -15
	return(res)
}

modisMask <- function(qcfile, b3file, saveRasters=FALSE, outdir=NULL){
    namecomps <- unlist(strsplit(basename(qcfile),"\\."))
    rq <- raster(qcfile)
	b3 <- raster(b3file)
	
	#masks <- data.frame(CloudMask=integer(ncell(rq)), ShadowMask=integer(ncell(rq)), WaterMask=integer(ncell(rq)), SnowMask=integer(ncell(rq)))
	masks <- data.frame(CloudMask=integer(ncell(rq)), WaterMask=integer(ncell(rq)))
	masks$CloudMask <- .cloudMask(getValues(b3))
	#masks$ShadowMask <- .cloudShadow(vals)
    masks$WaterMask <- .waterMask(getValues(rq))
	masks$SnowMask <- .snowMask(getValues(rq))
	mask <- masks$CloudMask*masks$WaterMask*masks$SnowMask
    
    #mask <- masks$CloudMask*masks$ShadowMask*masks$WaterMask*masks$SnowMask
    mask[mask==0] <- NA
    
    if(saveRasters){
        if(is.null(outdir)){
            outdir <- paste(dirname(qcfile),"/../clean",sep="")            
        }
        if(!file.exists(outdir)){
            dir.create(outdir, recursive=TRUE)
        }
        for(i in 1:length(masks)){
            band1 <- masks[[i]]
            band1[is.na(band1)] <- 0
            bfname <- paste(outdir, "/", paste(namecomps[2],namecomps[3],names(masks)[i], sep="_"), ".tif", sep="")
			band1 <- writeRaster(band1, filename=bfname, overwrite=TRUE, datatype='INT2S', options=c("COMPRESS=LZW", "TFW=YES"))
		}
    }            
    #masks$CloudMask[masks$CloudMask==0] <- NA 
	#masks$ShadowMask[masks$ShadowMask==0] <- NA 
	#masks$WaterMask[masks$WaterMask==0] <- NA
	#masks$SnowMask[masks$SnowMask==0] <- NA
    return(mask)    
}
