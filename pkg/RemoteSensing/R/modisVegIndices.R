# Authors: Sonia Asilo, Ritsuko Fuchiyama, Robert J. Hijmans, Yann Chemin, Angelo Carlo Pacheco
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
						swir1 <- raster(paste(inpath, b$filename[i], sep=''), FALSE)
					} else if (b$band[i] == 'b07') {
						swir2 <- raster(paste(inpath, b$filename[i], sep=''), FALSE)
					}
				}
				# making of VIs
				NDVI <- overlay(red, nir, fun=ndvi, filename=paste(fname, 'ndvi.grd', sep=''), overwrite=TRUE)
				LSWI <- overlay(nir, swir1, fun=lswi,  filename=paste(fname, 'lswi.grd', sep=''), overwrite=TRUE)
				EVI <- overlay(blue, red, nir, fun=evi, filename=paste(fname, 'evi.grd', sep=''), overwrite=TRUE)
				NDWI <- overlay(nir, swir2, fun=ndwi, filename=paste(fname, 'ndwi.grd', sep=''), overwrite=TRUE)
				
				# masking of VIs
				pat1 <- paste(d, "_", z, "_b03_mask.grd", sep="")
				bluemaskfiles <- list.files(inpath, pattern=pat1)
				bluemask <- raster(paste(inpath, bluemaskfiles[1], sep=""))
				
				pat2 <- paste(d, "_", z, "_SnowMask2.grd", sep="")
				snowmaskfiles <- list.files(inpath, pattern=pat2)
				snowmask <- raster(paste(inpath, snowmaskfiles[1], sep=""))
				
				NDVI <- overlay(NDVI, bluemask, snowmask, fun=multiply, filename=paste(fname, 'ndvi-cleaned.grd', sep=''), overwrite=TRUE)
				LSWI <- overlay(LSWI, bluemask, snowmask, fun=multiply,  filename=paste(fname, 'lswi-cleaned.grd', sep=''), overwrite=TRUE)
				EVI <- overlay(EVI, bluemask, snowmask, fun=multiply, filename=paste(fname, 'evi-cleaned.grd', sep=''), overwrite=TRUE)
				NDWI <- overlay(NDWI, bluemask, snowmask, fun=multiply, filename=paste(fname, 'ndwi-cleaned.grd', sep=''), overwrite=TRUE)
				NDDI <- overlay(NDVI, NDWI, fun=nddi, filename=paste(fname, 'nddi-cleaned.grd', sep=''), overwrite=TRUE)
								
				# writing of flooded,permanent water and drought
				flood <- overlay(LSWI, NDVI, EVI, fun=flooded, filename=paste(fname, 'flooded.grd', sep=''), overwrite=TRUE,  datatype='INT2S')
				permanent <- overlay(NDVI, LSWI, fun=persistentwater, filename=paste(fname, 'permanent.grd', sep=''), overwrite=TRUE, datatype='INT2S')
				droughtpix <- overlay(NDVI, NDWI, fun=.drought, filename=paste(fname, 'drought.grd', sep=''), overwrite=TRUE, datatype='INT2S')
			}
		}
	}
	# processing of all tiles in a directory
	if(tileNumber=="0"){
		print("You did not indicate a tile number. The script will process all the existing tiles in the inpath...")
		str <- list.files(inpath, pattern="001.*b01")
		str2 <- substr(str, 10, 15)
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
