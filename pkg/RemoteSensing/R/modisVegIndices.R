# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco, Jorrel Khalil S. Aunario
# International Rice Research Institute
# Date :  Feb 2009
# Version 0,1
# Licence GPL v3


# DROUGHT where drought = 2, non- drought=1
.drought <- function(NDVI, NDWI) {
	res <- ((NDVI < 0.5 & NDWI < 0.3)*2) + ((NDVI > 0.6 & NDWI > 0.4)*1)
	return(res)
	}

modisVeg <- function(inpath, tileNumber="0"){

	# multiply function used in masking snow, blue band values >=0.20
	multiply <- function(x,y,z) {
		result <- x*y*z
		return(result)
	}


	vegFxn <- function(inpath, tileNumber){
		# file reading
		pat <- paste(tileNumber, ".*clean.grd", sep="")
		m <- modisFilesClean(inpath, pat)

		#creation of output director "tif" folder
		outpath <- paste(inpath,"/../veg",sep="")
		if (!file.exists(outpath)) dir.create(outpath, recursive=TRUE)

		# looping
		zones <- unique(m$zone)
		for (z in zones) {
			mm <- subset(m, m$zone==z)
			dates <- unique(mm$date)
			cat("Zone:", z, "\n")
			flush.console()

			for (d in dates) {
			    dlab <- paste("Date ", d, ":", sep ="")
				cat(dlab, "loading images. \r")
				flush.console()
				b <- subset(mm, mm$date == d)
				fname <- paste(outpath, "/", b$date[1], "_", b$zone[1], "_", sep="")
				
                red <- raster(paste(inpath, b$filename[b$band=="b01"], sep="/"), values=TRUE)
				nir <- raster(paste(inpath, b$filename[b$band=="b02"], sep="/"), values=TRUE)
				blue <- raster(paste(inpath, b$filename[b$band=="b03"], sep="/"), values=TRUE)
				green <- raster(paste(inpath, b$filename[b$band=="b04"], sep="/"), values=TRUE)
				swir1 <- raster(paste(inpath, b$filename[b$band=="b06"], sep="/"), values=TRUE)
				swir2 <- raster(paste(inpath, b$filename[b$band=="b07"], sep="/"), values=TRUE)
				
                # making of VIs
                cat (dlab, "computing vegetation indices. \r")
                flush.console()
				NDVI <- overlay(red, nir, fun=ndvi)
				LSWI <- overlay(nir, swir1, fun=lswi)
				EVI <- overlay(blue, red, nir, fun=evi)
				NDWI <- overlay(nir, swir2, fun=ndwi)
				
				# masking of VIs
				cat (dlab, "masking vegetation indices.   \r")
                flush.console()
				
				#pat1 <- paste(d, "_", z, "_b03_mask.grd", sep="") 
				bluemaskfile <- paste(inpath, paste(d, "_", z, "_b03_mask.grd", sep=""), sep="/")
				if(!file.exists(bluemaskfile)) stop(paste(bluemaskfile, "does not exist!"))
				bluemask <- raster(bluemaskfile)
				NAvalue(bluemask) <- 0
				
				#pat2 <- paste(d, "_", z, "_SnowMask2.grd", sep="")
				#snowmaskfiles <- list.files(inpath, pattern=pat2)
				snowmaskfile <- paste(inpath, paste(d, "_", z, "_SnowMask2.grd", sep=""), sep="/")
				if(!file.exists(snowmaskfile)) stop(paste(snowmaskfile, "does not exist!"))
				snowmask <- raster(snowmaskfile, values=TRUE)
				NAvalue(snowmask) <- 0
				
				
				NDVI <- overlay(NDVI, bluemask, snowmask, fun=multiply, filename=paste(fname, "ndvi-cleaned.grd", sep=""), overwrite=TRUE)
				LSWI <- overlay(LSWI, bluemask, snowmask, fun=multiply,  filename=paste(fname, "lswi-cleaned.grd", sep=""), overwrite=TRUE)
				EVI <- overlay(EVI, bluemask, snowmask, fun=multiply, filename=paste(fname, "evi-cleaned.grd", sep=""), overwrite=TRUE)
				NDWI <- overlay(NDWI, bluemask, snowmask, fun=multiply, filename=paste(fname, "ndwi-cleaned.grd", sep=""), format='raster', overwrite=TRUE)
				NDDI <- overlay(NDVI, NDWI, fun=nddi, filename=paste(fname, "nddi-cleaned.grd", sep=""), format='raster', overwrite=TRUE)
								
				# writing of flooded,permanent water and drought
				cat (dlab, "Computing drought, flooded, and permanent water \r")
                flush.console()				
				flood <- overlay(LSWI, NDVI, EVI, fun=flooded, filename=paste(fname, "flooded.grd", sep=""), datatype="INT1S", overwrite=TRUE)
				permanent <- overlay(NDVI, LSWI, fun=persistentwater, filename=paste(fname, "permanent.grd", sep=""), datatype="INT1S", overwrite=TRUE)
				droughtpix <- overlay(NDVI, NDWI, fun=.drought, filename=paste(fname, 'drought.grd', sep=''), format='raster', datatype='INT1S', overwrite=TRUE)
                cat (dlab, " -------------------- DONE -------------------- \n")
                flush.console()
				
				
			}
		}
	}
	# processing of all tiles in a directory
	if(tileNumber=="0"){
		cat("You did not indicate a tile number. The script will process all the existing tiles in the inpath... \n")
		flush.console()
		str <- list.files(inpath, pattern="001.*b01.*grd")
		str2 <- substr(str, 10, 15)
		for(i in str2){
			# print(paste("Now mapping tile:", i ))
			vegFxn(inpath, i)
		}
	}
	# Processing of only one tile indicated in the function parameter
	else{
		vegFxn(inpath, tileNumber)
	}	
}
