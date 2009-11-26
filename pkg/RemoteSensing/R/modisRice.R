# Authors: Sonia Asilo, Robert J. Hijmans, Ritsuko Fuchiyama,  Yann Chemin, Angelo Carlo Pacheco
# International Rice Research Institute
# Date :  Feb 2009
# Version 0.1
# Licence GPL v3

modisRice <- function(inpath,tileNumber="") {

	mysum <- function(x){ sum(x, na.rm=T) }
	sumNotNA <- function(x){ sum(!is.na(x)) }
	mymean <- function(x) {
		sumv <- mysum(x)
		sumnotna <- sumNotNA(x)
		return(sumv/sumnotna)
	}
	mymax <- function(x) {
		x <- na.omit(x)
		if (length(x) > 0) {
		
			return(max(x)) 
		} else { 
			return( NA ) 
		}
	}
	
	Flooded <- function (flooded) {sum(flooded, na.rm=T) > 0}	#Flooded= 1  ; not flooded = 0	
	Permanent <- function (permanent) { sum(permanent, na.rm=T) >= 10} # permanent = 1; not permanet = 0
	Forest <- function(ndvi){ sum( ndvi >= 0.7 , na.rm=T) > 20}	# Forest: 1, ; not forest =0
	Shrub <- function(lswi){ sum(lswi < 0.1, na.rm=T) == 0 } # shrub=1; not shrub = 0
	# Bare <- function(ndvi){ sum(ndvi > 0.1, na.rm=T) < 2 }

	riceFxn <- function(inpath, tileNumber){
		# file reading
		pat <- paste(tileNumber, ".*.grd", sep="")
		m <- modisFilesClean(inpath, pat)
		m$filename <- paste(inpath, m$filename, sep="")

		# creation of output director "tif" folder
		outpath <- paste(inpath,"/../rice/",sep="")
		dir.create(outpath, showWarnings = FALSE)

		# looping
		zones <- as.vector(unique(m$zone))
		for (z in zones) {
			mm <- subset(m, m$zone==z)
			years <- as.vector(unique(mm$year))
			print(paste('Zone:', z))
			for (y in years) {
				print(paste('Year:', y))
				mmm <- subset(mm, mm$year == y, )
				dates <- as.vector(unique(mmm$date))
				dates <- sort(dates)
				if (length(dates) < 46) { 
					if (length(dates) < 43) { 
						stop(paste('expected 46 files, found:', length(dates))) 
					}
					warning(paste('expected 46 files, found:', length(dates))) 
				}

				ndvistk <- stack( as.vector( mmm$filename[mmm$band=='ndvi-cleaned']) )
				evistk <- stack( as.vector( mmm$filename[mmm$band=='evi-cleaned']) )
				lswistk <- stack( as.vector( mmm$filename[mmm$band=='lswi-cleaned']) )
				floodstk <- stack( as.vector( mmm$filename[mmm$band=='flooded']) )
				permanentstk <- stack(as.vector( mmm$filename[mmm$band=='permanent']) )

				fnameflood <- paste(outpath, 'flooded_', z, '_', y, '.grd', sep='')
				flooded <- calc(floodstk, fun=Flooded, filename= fnameflood, overwrite=TRUE)
				flooded <- readAll(flooded)

				fnamepermanent <- paste(outpath, 'permanent_', z, '_', y, '.grd', sep='')
				permanent <- calc(permanentstk, fun=Permanent, filename=fnamepermanent, overwrite=TRUE)
				permanent <- readAll(permanent)

				fnameforest <- paste(outpath, 'forest_', z, '_', y, '.grd', sep='') 
				forest <- calc(ndvistk, fun=Forest, filename=fnameforest, overwrite=TRUE)
				forest <- readAll(forest)

				fnameshrub <- paste(outpath, 'shrub_', z, '_', y, '.grd', sep='') 
				shrub <- calc(lswistk, fun=Shrub, filename=fnameshrub, overwrite=TRUE) 
				shrub <- readAll(shrub)
				shrub  <- shrub & !forest
        
				notrice <- (permanent | forest | shrub)
				notrice <- readAll(notrice)
				filenamenr <- paste(outpath, 'notrice_', z, '_', y, '.grd', sep='')
				writeRaster(notrice, filename=filenamenr, , filetype='raster', overwrite=TRUE)

				perhapsrice <- flooded & !notrice
				filenamephr <- paste(outpath, 'perhapsrice_', z, '_', y, '.grd', sep='')
				perhapsrice <- writeRaster(perhapsrice, filename=filenamephr, filetype='raster', overwrite=TRUE)
			}
		}
	}


	# processing of all tiles in a directory
	if(tileNumber==""){
		print("You did not indicate a tile number. The script will process all the existing tiles in the inpath...")
		str <- list.files(inpath, pattern="001.*b01")
		str2 <- substr(str, 10, 15)
		for(i in str2){
			print(paste("Now mapping tile:", i ))
			riceFxn(inpath, i)
		}
	}
	# Processing of only one tile indicated in the function parameter
	else{
		riceFxn(inpath, tileNumber)
	}
	
}

