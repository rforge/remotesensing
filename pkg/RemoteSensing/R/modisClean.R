# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin and Angelo Carlo Pacheco
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
	res <- (nir > 0.11) & (ndsi > 0.40)
	return(res)
}

# snow function (reclass)
.snow <- function(pixel){
	pixel[pixel >= 1] <- NA
	pixel[pixel == 0] <- 1
	pixel[is.na(pixel)] <- 0
	return(pixel)
}

# sum (snow count)
mysum <- function(x){ 
	sum(x, na.rm=T)  
}


modisClean <- function(inpath, tileNumber="0"){

	cleanFxn <- function(inpath, tileNumber){
		#reading of files
		pat <- paste(tileNumber, ".*.tif", sep="")
		print(pat)
		m <- modisFiles(inpath, pat)

		#creation of output director "tif" folder
		outpath <- paste(inpath,"/../clean",sep="")
		if (!file.exists(outpath)) dir.create(outpath, recursive=TRUE)

		#looping
		zones <- as.vector(unique(m$zone))
		for (z in zones) {

			mm <- subset(m, m$zone==z)
			dates <- as.vector(unique(mm$date))
			cat(paste('Zone:', z, "\n"))
			flush.console()

			
			for (d in dates) {
				cat(d, "\r")
				flush.console()
				qfile <- paste(inpath,m$filename[which(mm$date==d & mm$band=="sta")], sep="/")
				b <- subset(mm, mm$date == d & mm$band != "sta")
				rq <- raster(qfile, TRUE)
				cloudmask <- calc(rq, .internalCloud, filename=paste(outpath, '/', d, '_', z, '_', 'CloudMask.grd', sep=''),format='raster',datatype='INT1S', overwrite=TRUE)
				shadowmask <- calc(rq, .cloudShadow, filename= paste(outpath, '/', d, '_', z, '_', 'ShadowMask.grd', sep=''),format='raster',datatype='INT1S', overwrite=TRUE)
				watermask <- calc(rq, .waterMask, filename= paste(outpath, '/', d, '_', z, '_', 'WaterMask.grd', sep=''),format='raster',datatype='INT1S', overwrite=TRUE)
				snowmask <-  calc(rq, .snowMask, filename= paste(outpath, '/', d, '_', z, '_', 'SnowMask.grd', sep=''),format='raster',datatype='INT1S', overwrite=TRUE)
				NAvalue(cloudmask) <- 0
                NAvalue(shadowmask) <- 0
                NAvalue(watermask) <- 0
                NAvalue(snowmask) <- 0

                fname <- paste(outpath, '/', d, '_', z, '_', sep='')

				#looping for each of the 6 bands
				for (i in 1:nrow(b)) {
					r <- raster( paste(inpath, '/', b$filename[i], sep='') )
					NAvalue(r) <- -28672
					r <-  r*cloudmask*shadowmask*watermask*snowmask/10000
					#r[cloudmask==0 | shadowmask==0 | watermask ==0 | snowmask==0] <- NA  # apply mask 
					#r[] <- values(r) / 10000
					filenamec <- paste(fname, b$band[i], '_clean.grd', sep='')
					
					if (b$band[i]=="b03"){
					   # BLUE
                        fnameblumask <- paste(fname, "b03_mask.grd", sep="")
                        blumask <- calc(r, fun= .blueMask, filename = fnameblumask, datatype='INT1S', overwrite=TRUE)    
                    } else if (b$band[i]=="b02"){
                        # NIR
                        nir <- r
                    } else if(b$band[i]=="b04"){
                        # GREEN
                        green <- r
                    }
					r <- writeRaster(r, filename=filenamec, datatype="FLT4S", fileformat='raster', overwrite=TRUE)
				}

				# create ndsi
				NDSI <- overlay(green, nir, fun= ndsi, filename=paste(fname, "ndsi.grd", sep=""), datatype='FLT4S', overwrite=TRUE)

				# create second snow mask
				snowpix <- overlay(nir, NDSI, fun=.snowMask2, filename=paste(fname, "SnowPixels.grd", sep=""), datatype='INT1S', overwrite=TRUE)
				snowmask <- calc(snowpix, fun=.snow, filename=paste(fname, "SnowMask2.grd", sep=""), datatype='INT1S', overwrite=TRUE)
			}
		}
	}

	# processing of all tiles in a directory
	if(tileNumber=="0"){
		cat("You did not indicate a tile number. The script will process all the existing tiles in the inpath...\n")
		flush.console()
		#print("Press CTRL + C to terminate.")
		str <- list.files(inpath, pattern="001.*b01")
		str2 <- substr(str, 18, 23)
		for(i in str2){
			cat(paste("Now mapping tile:", i ), "\r")
			flush.console()
			cleanFxn(inpath, i)
		}
	}
	else{
		cleanFxn(inpath, tileNumber)
	}
}