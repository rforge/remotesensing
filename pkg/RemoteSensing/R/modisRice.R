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
		m$filename <- paste(inpath, m$filename, sep="/")

		# creation of output director "tif" folder
		outpath <- paste(inpath,"/../rice",sep="")
		if(!file.exists(outpath)) dir.create(outpath, recursive=TRUE)

		# looping
		zones <- unique(m$zone)
		for (z in zones) {
			mm <- subset(m, m$zone==z)
			years <- unique(mm$year)
			cat(paste("Processing Zone", z, "\n"))
			flush.console()
			for (y in years) {
				cat("Year:", y, "\n")
				flush.console()
				mmm <- subset(mm, mm$year == y, )
				dates <- unique(mmm$date)
				dates <- sort(dates)
				if (length(dates) < 46) { 
					if (length(dates) < 43) { 
						stop(paste("expected 46 files, found:", length(dates))) 
					}
					warning(paste("expected 46 files, found:", length(dates))) 
				}

				ndvistk <- stack(mmm$filename[grep('ndvi-cleaned',mmm$band)])
				evistk <- stack( as.vector( mmm$filename[grep('evi-cleaned',mmm$band)]) )
				lswistk <- stack( as.vector( mmm$filename[grep('lswi-cleaned',mmm$band)]) )
				floodstk <- stack( as.vector( mmm$filename[grep('flooded',mmm$band)]) )
				permanentstk <- stack(as.vector( mmm$filename[grep('permanent',mmm$band)]) )

				#fnameflood <- paste(outpath, 'flooded_', z, '_', y, '.grd', sep='')
				#flooded <- calc(floodstk, fun=Flooded, filename= fnameflood, datatype='INT1S', overwrite=TRUE)
				#flooded <- readAll(flooded)


				fnameflood <- paste(outpath, "/flooded_", z, "_", y, ".grd", sep="")
				flooded <- calc(floodstk, fun=Flooded, filename= fnameflood, format='raster', datatype="INT1S", overwrite=TRUE)
				flooded <- readAll(flooded)

				fnamepermanent <- paste(outpath, "/permanent_", z, "_", y, ".grd", sep="")
				permanent <- calc(permanentstk, fun=Permanent, filename=fnamepermanent, format='raster', datatype="INT1S", overwrite=TRUE)
				permanent <- readAll(permanent)

				fnameforest <- paste(outpath, "/forest_", z, "_", y, ".grd", sep="") 
				forest <- calc(ndvistk, fun=Forest, filename=fnameforest, format='raster', datatype="INT1S", overwrite=TRUE)
				forest <- readAll(forest)

				fnameshrub <- paste(outpath, "/shrub_", z, "_", y, ".grd", sep="") 
				shrub <- calc(lswistk, fun=Shrub, filename=fnameshrub, format='raster', datatype="INT1S", overwrite=TRUE) 
				shrub <- readAll(shrub)
				shrub  <- shrub & !forest
        
				notrice <- (permanent | forest | shrub)
				#notrice <- readAll(notrice)
				#filenamenr <- paste(outpath, "notrice_", z, "_", y, ".grd", sep="")
				notrice <- writeRaster(notrice, filename=paste(outpath, "/notrice_", z, "_", y, ".grd", sep=""), format="raster", datatype="INT1S",overwrite=TRUE)

				perhapsrice <- flooded & !notrice
				#filenamephr <- paste(outpath, "perhapsrice_", z, "_", y, ".grd", sep="")
				perhapsrice <- writeRaster(perhapsrice, filename=paste(outpath, "/perhapsrice_", z, "_", y, ".grd", sep=""), format="raster", datatype="INT1S",overwrite=TRUE)
			}
		}
	}


	# processing of all tiles in a directory
	if(tileNumber==""){
		cat("You did not indicate a tile number. The script will process all the existing tiles in the inpath...\n")
		flush.console()
		tiles <- substr(list.files(inpath, pattern="001.*ndvi-cleaned.grd"), 10, 15)
		for(i in tiles){
			#print(paste("Now mapping tile:", i ))
			riceFxn(inpath, i)
		}
	}
	# Processing of only one tile indicated in the function parameter
	else{
		riceFxn(inpath, tileNumber)
	}
	
}

