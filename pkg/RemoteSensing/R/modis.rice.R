# Authors: Sonia Asilo, Robert J. Hijmans, Ritsuko Fuchiyama,  Yann Chemin
# International Rice Research Institute
# Date :  Feb 2009
# Version 0.1
# Licence GPL v3

rice <- function(inpath, outpath) {

	inpath <- paste(inpath, "/", sep="")
	outpath <- paste(outpath, "/", sep="")

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

	m <- modisFilesClean(inpath)
	m$filename <- paste(inpath, m$filename, sep="")
	dir.create(outpath, showWarnings = FALSE)
	zones <- as.vector(unique(m$zone))
	for (z in zones) {
		mm <- subset(m, m$zone==z)
		years <- as.vector(unique(mm$year))
		print(paste('Zone:', z))
		for (y in years) {
			mmm <- subset(mm, mm$year == y, )
			dates <- as.vector(unique(mmm$date))
			dates <- sort(dates)
			if (length(dates) < 46) { 
				if (length(dates) < 43) { 
					stop(paste('expected 46 files, found:', length(dates))) 
				}
				warning(paste('expected 46 files, found:', length(dates))) 
			}
						
			ndvistk <- stack( as.vector( mmm$filename[mmm$band=='ndvi']) )
			evistk <- stack( as.vector( mmm$filename[mmm$band=='evi']) )
			lswistk <- stack( as.vector( mmm$filename[mmm$band=='lswi']) )
			floodstk <- stack( as.vector( mmm$filename[mmm$band=='flooded']) )
			permanentstk <- stack(as.vector( mmm$filename[mmm$band=='permanent']) )

			fnameflood <- paste(outpath, 'flooded_', z, '_', y, '.grd', sep='')
			flooded <- calc(floodstk, fun=Flooded, filename = fnameflood, overwrite=T)
						
			fnamepermanent <- paste(outpath, 'permanent_', z, '_', y, '.grd', sep='')
			permanent <- calc(permanentstk, fun=Permanent, filename=fnamepermanent, overwrite=T)
			permanent <- readAll(permanent)
												
			fnameforest <- paste(outpath, 'forest_', z, '_', y, '.grd', sep='') 
			forest <- calc(ndvistk, fun=Forest, filename=fnameforest, overwrite=T)
			forest <- readAll(forest)
			
			fnameshrub <- paste(outpath, 'shrub_', z, '_', y, '.grd', sep='') 
			shrub <- calc(lswistk, fun=Shrub, filename=fnameshrub, overwrite=T) 
			shrub <- readAll(shrub)
			shrub  <- shrub & !forest
										
			#notrice <- any(permanent, forest, shrub)           # It doesnt work on my computer
			notrice <- (permanent | forest | shrub)
			notrice <- readAll(notrice)
			filename(notrice) <- paste(outpath, 'notrice_', z, '_', y, '.grd', sep='')
			notrice <- writeRaster(notrice)
			
			perhapsrice <- flooded & !notrice
			filename(perhapsrice) <- paste(outpath, 'perhapsrice_', z, '_', y, '.grd', sep='')
			perhapsrice <- writeRaster(perhapsrice)
		}	
	}		
}		

