# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin and Angelo Carlo Pacheco
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


# Internal cloud  algorithm flag
.internalcloud <- function (pixel) {
	pixel <- modis.sqa500f(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
	return(pixel)
}

# Cloud shadow mask
.cloudshadow <- function (pixel) {
	pixel <- modis.sqa500b(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
	return(pixel)
}

# Water mask
.watermask <- function(pixel) {
	res <- modis.sqa500c(pixel)
	res[res < 1 | res == 2 | res > 5 ] <- 0
	res[res >= 3] <- 0
	res[res >= 1] <- 1
	return(res)
}

# Internal Snow mask
.snowmask <- function(pixel) {
	pixel <- modis.sqa500k(pixel)
	pixel <- pixel + 1
	pixel[pixel > 1] <- 0
	return(pixel)
}

# Blue mask
.bluemask <- function(pixel) {
	pixel[pixel >= 0.2] <- NA
	pixel[pixel < 0.2] <- 1
	pixel[is.na(pixel)] <- 0
	return(pixel)
}


# second snow mask
.snowmask2 <- function(nir, ndsi) {
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
		outpath <- paste(inpath,"/../clean/",sep="")
		dir.create(outpath, showWarnings = FALSE)

		#looping
		zones <- as.vector(unique(m$zone))
		for (z in zones) {

			mm <- subset(m, m$zone==z)
			dates <- as.vector(unique(mm$date))
			print(paste('Zone:', z))

			
			for (d in dates) {
				print(d)
				qfile <- paste(inpath, as.vector(subset(mm, mm$date == d & mm$band == "sta")$filename), sep='')
				b <- subset(mm, mm$date == d & mm$band != "sta")
				rq <- raster(qfile, TRUE)
				mask <- calc(rq, .internalcloud) * calc(rq, .cloudshadow) * calc(rq, .watermask) * calc(rq, .snowmask)
				fname <- paste(outpath, d, '_', z, '_', sep='')

				#looping for each of the 6 bands
				for (i in 1:length(b[,1])) {
					r <- raster( paste(inpath, b$filename[i], sep='') )
					NAvalue(r) <- -28672
					r <- readAll(r)
					r[mask == 0] <- NA  # apply mask 
					r[] <- values(r) / 10000
					filenamec <- paste(fname, b$band[i], '_clean.grd', sep='')
					r <- writeRaster(r, filename=filenamec, datatype="FLT4S", fileformat='raster', overwrite=TRUE)
				}

				# needs b03 (rasterization)
				BLUE <- list.files(outpath, pattern=paste(d,"_",z, "_b03_clean.grd", sep=""))
				bluefiles <- raster(paste(outpath, BLUE[1], sep=""))
				fnameblumask <- paste(outpath, substr(BLUE[1], 1,16), "b03_mask.grd", sep="")
				blumask <- calc(bluefiles, fun= .bluemask, filename = fnameblumask, datatype='INT2S', overwrite=TRUE)

				# needs b02 and b04 (rasterization)
				NIRfile <- list.files(outpath, paste(d,"_",z, "_b02_clean.grd", sep=""))
				nir <- raster(paste(outpath, NIRfile[1], sep=""))
				GREENfile <- list.files(outpath, paste(d,"_",z, "_b04_clean.grd", sep=""))
				green <- raster(paste(outpath, GREENfile[1], sep=""))

				# create ndsi
				fname <- paste(outpath, substr(NIRfile[1], 1,16), "ndsi.grd", sep="")
				NDSI <- overlay(green, nir, fun= ndsi, filename=fname, datatype='INT2S', overwrite=TRUE)

				# create second snow mask
				fname1 <- paste(outpath, substr(NIRfile[1], 1,16), "SnowPixels.grd", sep="")
				snow <- overlay(nir, NDSI, fun=.snowmask2, filename=fname1, overwrite=TRUE)
				fname2 <- paste(outpath, substr(NIRfile[1], 1,16), "SnowMask.grd", sep="")
				snowmask <- calc(snow, fun=.snow, filename=fname2, overwite=T)
			}
		}
	}

	# processing of all tiles in a directory
	if(tileNumber=="0"){
		print("You did not indicate a tile number. The script will process all the existing tiles in the inpath...")
		#print("Press CTRL + C to terminate.")
		str <- list.files(inpath, pattern="001.*b01")
		str2 <- substr(str, 18, 23)
		for(i in str2){
			print(paste("Now mapping tile:", i ))
			cleanFxn(inpath, i)
		}
	}
	else{
		cleanFxn(inpath, tileNumber)
	}
}