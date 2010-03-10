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
		#print(pat)
		m <- modisFiles(inpath, pat)

		#creation of output director "tif" folder
		outpath <- paste(inpath,"/../clean",sep="")
		if (!file.exists(outpath)) dir.create(outpath, recursive=TRUE)

		#looping
		zones <- as.vector(unique(m$zone))
		for (z in zones) {
            cat("Processing Zone", z, "\n")
			flush.console()

			mm <- subset(m, m$zone==z)
			dates <- as.vector(unique(mm$date))			
			
			for (d in dates) {
			    dlab <- paste("Date ", d, ":", sep ="")
				cat(dlab, "Calculating masks. \r")
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
				    cat(dlab, " Applying masks to ",b$band[i],".\r", sep="")
    				flush.console()

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
					r <- writeRaster(r, filename=filenamec, datatype="FLT4S", format='raster', overwrite=TRUE)
				}

				# create ndsi
				cat(dlab, "Computing NDSI and secondary snow mask. \r")
				flush.console()

				NDSI <- overlay(green, nir, fun= ndsi, filename=paste(fname, "ndsi.grd", sep=""), datatype='FLT4S', overwrite=TRUE)

				# create second snow mask
				#snowpix <- overlay(nir, NDSI, fun=.snowMask2, filename=paste(fname, "SnowPixels.grd", sep=""), datatype='INT1S', overwrite=TRUE)
				snowmask <-  overlay(nir, NDSI, fun=.snowMask2, filename=paste(fname, "SnowMask2.grd", sep=""), datatype='INT1S', overwrite=TRUE)
                cat (dlab, " -------------------- DONE -------------------- \n")
                flush.console()

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
			cleanFxn(inpath, i)
		}
	}
	else{
		cleanFxn(inpath, tileNumber)
	}
}