# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


# water masking
persistentwater <- function(ndvi,lswi){ 	
	result <- (ndvi < 0.10) & (ndvi < lswi)
	return(result)
}

# multiply function used in masking snow, blue
multiply <- function(x,y,z) {
	result <- x*y*z
	return(result)
}


modisVeg <- function(inpath, tileNumber="0"){

	vegFxn <- function(inpath, tileNumber){
		# file reading
		pat <- paste(tileNumber, ".*clean.grd", sep="")
		m <- modisFilesClean(inpath, pat)

		#creation of output director "tif" folder
		outpth <- paste(inpath,"/../veg/",sep="")
		dir.create(outpth, showWarnings = FALSE)

		# looping
		zones <- as.vector(unique(m$zone))
		for (z in zones) {
			mm <- subset(m, m$zone==z)
			dates <- as.vector(unique(mm$date))
			print(paste('Zone:', z))

			for (d in dates) {
				print(d)
				b <- subset(mm, mm$date == d)
				fname <- paste(outpth, b$date[1], '_', b$zone[1], '_', sep='')
				for (i in 1:length(b[,1])) { 
					if (b$band[i] == 'b01') {
						red <- raster(paste(inpath, b$filename[i], sep=''), FALSE )
					} else if (b$band[i] == 'b02') {
						nir <- raster(paste(inpath, b$filename[i], sep=''), FALSE )
					} else if (b$band[i] == 'b03') {
						blue <- raster(paste(inpath, b$filename[i], sep=''), FALSE )
					} else if (b$band[i] == 'b04') {
						green <- raster(paste(inpath, b$filename[i], sep=''), FALSE)
					} else if (b$band[i] == 'b06') {
						swir2 <- raster(paste(inpath, b$filename[i], sep=''), FALSE)
					} 
				}
				# making of VIs
				NDVI <- overlay(red, nir, fun=ndvi, filename=paste(fname, 'ndvi.grd', sep=''), overwrite=T)
				LSWI <- overlay(nir, swir2, fun=lswi,  filename=paste(fname, 'lswi.grd', sep=''), overwrite=T)
				EVI <- overlay(blue, red, nir, fun=evi, filename=paste(fname, 'evi.grd', sep=''), overwrite=T)

				# masking of VIs
				pat1 <- paste(d, "_", z, "_b03_mask.grd", sep="")
				bluemaskfiles <- list.files(inpath, pattern=pat1)
				bluemask <- raster(paste(inpath, FILES[1], sep=""))
				
				pat2 <- paste((d, "_", z, "_SnowMask.grd", sep="")
				FILES <- list.files(inpath, pattern=pat2)
				snowmask <- raster(paste(inpath, FILES[1], sep=""))
				
				NDVI <- overlay(NDVI, bluemask, snowmask, fun=multiply, filename=paste(fname, 'ndvi_cleaned.grd', sep=''), overwrite=T)
				LSWI <- overlay(LSWI, bluemask, snowmask, fun=multiply,  filename=paste(fname, 'lswi_cleaned.grd', sep=''), overwrite=T)
				EVI <- overlay(EVI, bluemask, snowmask, fun=multiply, filename=paste(fname, 'evi_cleaned.grd', sep=''), overwrite=T)

				# writing of flooded and permanent water
				flood <- overlay(LSWI, NDVI, EVI, fun=flooded, filename=paste(fname, 'flooded.grd', sep=''), overwrite=T,  datatype='INT2S')
				permanent <- overlay(NDVI, LSWI, fun=persistentwater, filename=paste(fname, 'permanent.grd', sep=''), overwrite=T, datatype='INT2S')
			}
		}
	}
	# processing of all tiles in a directory
	if(tileNumber=="0"){
		print("You did not indicate a tile number. The script will process all the existing tiles in the inpath...")
		str <- list.files(inpath, pattern="001.*b01")
		str2 <- substr(str, 18, 23)
		for(i in str2){
			print(paste("Now mapping tile:", i ))
			vegFxn(inpath, i)
		}
	}
	# Processing of only one tile indicated in the function parameter
	else{
		vegFxn(inpath, tileNumber)
	}

	
}