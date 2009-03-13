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

	Forest <- function(ndvi){ sum( ndvi > 0.7 , na.rm=T) > 14 }
	Shrub <- function(lswi){ sum(lswi < 0.1, na.rm=T) < 3 }
	Bare <- function(ndvi){ sum(ndvi > 0.1, na.rm=T) < 2 }

	m <- modisFilesClean(inpath)
	m$filename <- paste(inpath, m$filename, sep="")
	dir.create(outpath, showWarnings = FALSE)
	zones <- as.vector(unique(m$zone))
	for (z in zones) {
		mm <- subset(m, m$zone==z)
		years <- as.vector(unique(mm$year))
		for (y in years) {
			mmm <- subset(mm, mm$year == y)
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

			flooded <- calc(floodstk, fun=mysum,  filename="")
			permanent <- flooded > 30
			flooded[flooded > 0] <- 1
			
			forest <- calc(ndvistk, fun=Forest)
			shrub <- calc(lswistk, fun=Shrub)
			shrub  <- shrub & !forest
			bare <- calc(ndvistk, fun=Bare)

			filename(bare) <- paste(outpath, 'bare_', y, '.grd', sep='')			
			filename(shrub) <- paste(outpath, 'shrub_', y, '.grd', sep='')			
			filename(forest) <- paste(outpath, 'forest_', y, '.grd', sep='')			
			bare <- writeRaster(bare)
			shrub <- writeRaster(shrub)
			forest <- writeRaster(forest)
			
			notrice <- any(permanent, forest, shrub)
			perhapsrice <- flooded & !notrice

			filename(notrice) <- paste(outpath, 'notrice_', y, '.grd', sep='')			
			notrice <- writeRaster(notrice)
			filename(perhapsrice) <- paste(outpath, 'perhapsrice_', y, '.grd', sep='')
			perhapsrice <- writeRaster(perhapsrice)
			
			maxevi <- calc(evistk, fun=mymax, filename="")
			
			xiaorice <- setRaster(evistk)
			xiaorice <- setDatatype(xiaorice, 'INT2S')
			filename(xiaorice) <- 'xiaorice.grd'
			for (r in 1:nrow(evistk)) {
				evistk <- readRow(evistk,r)
				max_per <- apply(values(evistk), 1, which.max)
				d <- matrix(NA, nrow=length(max_per), ncol=6)
				d[,6] <- max_per
				d[,5] <- max_per - 1
				d[,4] <- max_per - 2
				d[,3] <- max_per - 3
				d[,2] <- max_per - 4
				d[,1] <- max_per - 5
				d[d<1] <- nlayers(evistk) + d[d<1]
				index <- cbind(rep(1:length(d[,1]), each=6), as.vector(t(d)))
				floodstk <- readRow(floodstk, r)
				isrice <- apply(matrix(values(floodstk)[d], ncol=6, byrow=T), 1, max)
				xiaorice <- setValues(xiaorice, isrice , r ) 
				xiaorice <- writeRaster(xiaorice)
			}
		}
	}
}
			


