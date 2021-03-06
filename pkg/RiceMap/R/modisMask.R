# Author: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario 
# International Rice Research Institute
# Date : 21 May 2010
# Version 0,1
# Licence GPL v3

# Cloud Mask
.cloudMask <- function(b3){
#	res <- (((b3 > 18)*0) + (b3 < 18))
	res <- b3 < 0.18
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
	vals <- modis.sqa500c(pixel)
	res <- (!(vals == 2 | vals >= 3)) & (vals >= 1)
	return(res)
}

#Internal Snow mask
.snowMask <- function(pixel) {
	vals <- modis.sqa500k(pixel)
	vals <- vals + 1
	res <- vals <= 1
	return(res)
}

# Blue mask
#.blueMask <- function(pixel) {
#    pixel[pixel >= 0.2] <- NA 
#	pixel[pixel < 0.2] <- 1
#	pixel[is.na(pixel)] <- 0
#	return(pixel)
#}

#second snow mask
snow2 <- function(ndsi, nir) {    
	res <- !((nir > 0.11) & (ndsi > 0.40))
#	res[(nir > 0.11) & (ndsi > 0.40)] <- 0
#	res[is.na(res)] <- -15
	return(res)
}

snow3 <- function(ndsi, green, nir) {
	res <- !((nir > 0.10) & (green > 0.10) & (ndsi >= 0.40))
#	res[(nir > 0.10) & (green > 0.10) & (ndsi >= 0.40)] <- 0
#	res[is.na(res)] <- -15
	return(res)
}

cloud <- function(b03){    
    return(.cloudMask(b03))
}

water <- function(state_500m){    
    return(.waterMask(state_500m))    
}

snow <- function(state_500m){    
    return(.snowMask(state_500m))    
}

modis.mask <- function(modvals, masks){
    masks <- as.matrix(masks)
    m <- rowSums(masks, na.rm=TRUE)
    mm <- which(m<ncol(masks)) 
    if (is.null(ncol(modvals))) modvals[mm] <- NA else {
        for (i in 1:ncol(modvals)) modvals[mm,i] <- NA
    }
    return(modvals)
}

modisMask <- function(qcfile, b3file, saveRasters=TRUE, outdir=NULL){
    namecomps <- unlist(strsplit(basename(qcfile),"\\."))
    rq <- raster(qcfile)
	b3 <- raster(b3file)
	
	#masks <- data.frame(CloudMask=integer(ncell(rq)), ShadowMask=integer(ncell(rq)), WaterMask=integer(ncell(rq)), SnowMask=integer(ncell(rq)))
	masks <- data.frame(cloud.mask=integer(ncell(rq)), water.mask=integer(ncell(rq)))
	masks$cloud.mask <- .cloudMask(getValues(b3))
	#masks$ShadowMask <- .cloudShadow(vals)
    masks$water.mask <- .waterMask(getValues(rq))
	masks$snow.mask <- .snowMask(getValues(rq))
	mask <- masks$cloud.mask*masks$water.mask*masks$snow.mask
    
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
            band1 <- setValues(rq, masks[[i]])
            bfname <- paste(outdir, "/", paste(namecomps[2],namecomps[3],names(masks)[i], sep="."), ".tif", sep="")
			ifelse(class(try(writeRaster(band1, filename=bfname, overwrite=TRUE, datatype='INT2S', options=c("COMPRESS=LZW", "TFW=YES"), NAflag=-15), silent=TRUE))=="try-error",
				writeRaster(band1, filename=bfname, overwrite=TRUE, datatype='INT2S', options=c("COMPRESS=LZW", "TFW=YES"), NAflag=-15), TRUE)
			rm(band1)
			gc(verbose=FALSE)
		}
    }            
    #masks$CloudMask[masks$CloudMask==0] <- NA 
	#masks$ShadowMask[masks$ShadowMask==0] <- NA 
	#masks$WaterMask[masks$WaterMask==0] <- NA
	#masks$SnowMask[masks$SnowMask==0] <- NA
	rm(masks)
	gc(verbose=FALSE)
    return(mask)    
}
